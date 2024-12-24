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

unit MLexDist;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

Uses
  Classes, MTokenizer, MegaConsts, KeywordConsts;

type
  TLexDist = class(TTokenizer)
  private
    procedure updateOptions(Variable, Option: AnsiString);
    procedure updateDefaults;

    function ReadDistMatrix: Boolean;   // reads data assuming header already read

  private
    FDistMatrix : PDistanceMatrix; // contains distance matrix

    function GetDist(i,j: Integer): Double;
  public
    constructor Create;
    destructor Destroy; override;

    function ReadHeader: Boolean;  // Reads full header
    function ReadData: Boolean;    // reads data assuming header already read

    property Dist[i, j: Integer]: Double read GetDist;
    property Title;
    property Description;
    property FoundFormat;
    property MissingSym;
    property IdenticalSym;
    property GapSym;
    property NoOfOtus;
    property Taxon;
    property NoOfGps;
    property GpName;
    property Row;
    property Col;
  end;

implementation

uses
  SysUtils, Graphics, Controls, Forms, Dialogs, Buttons, TypInfo,
  MegaUtils, mgeographical_info;

constructor TLexDist.Create;
begin
  inherited;
  FDistMatrix := nil;

  UpdateOptProc := UpdateOptions;
  UpdateDefProc := UpdateDefaults;
end;

//---- Destroy
destructor TLexDist.Destroy;
begin
  if FDistMatrix <> nil then
    FreeDistMatrix(FDistMatrix, FNoOfOtus);
  inherited;
end;

function TLexDist.GetDist(i,j: Integer): Double;
begin
  if      i=j then Result := 0
  else if i>j then Result := FDistMatrix[i,j]
  else             Result := FDistMatrix[j,i];
end;

//---- reads data only; assumes header already read
function TLexDist.ReadData: Boolean;
begin
  {$IFDEF VISUAL_BUILD}
  if FGauge <> nil then  FGauge.Show;
  {$ENDIF}
  UpdateDefaults;

  if FCaption <> nil then  FCaption.Text := 'Reading distances';
  ReadDistMatrix;
  {$IFDEF VISUAL_BUILD}
  if FGauge   <> nil then  FGauge.Position := 100;
  {$ENDIF}
  if FCaption <> nil then  FCaption.Text := 'Done Reading';
  Result := True;
end;

//---- Reads complete file header
function TLexDist.ReadHeader: Boolean;  // only reads header
var
  IsCommandEverFound: Boolean;
begin
  Unwind;       //--- Unwind

  //--- Read #MEGA
  NextToken;
  CheckTokenSymbol('#');
  NextToken;
  CheckTokenSymbol('MEGA');

  IsCommandEverFound := False;
  while True do  // read until valid commands are found
  begin
    NextToken;
    if Token = toComment then
    begin
      GetStringToken(toCommentEnd);
      continue;
    end;
    if Token = toCommentMEGA1 then
    begin
      GetStringToken(toCommentMEGA1);
      continue;
    end;

    if Token <> toCommand then
    begin
      if IsCommandEverFound then
      begin
        FIsTokenPending := True;
        Break;
      end;
      //otherwise it is a MEGA1 style format
      if TokenSymbolIs('Title') then
      begin
        GetStringToken(#13);
        FStringToken := Trim(FStringToken);
        if Length(FStringToken) > 0 then
          if FStringToken[length(FStringToken)] = ';' then // remove semi colon
            FStringToken[length(FStringToken)] := ' ';
        FStringToken := Trim(FStringToken);
        FTitle := FStringToken;
        FOptions.Add('Title='+FTitle);
        SkipUntilChar('#'); // this just removes all the garbage after Title line
        Break;
      end
      else
        ErrorStr('The data file must contain a Title command following the #mega');
    end;

    IsCommandEverFound := True;
    NextToken;
    if TokenSymbolIs('Title') then
    begin
      if FOptions.IndexOfName('Title') > 0 then
        ErrorStr('Duplicate Title found');
      GetStringToken(toCommandEnd);
      FTitle := FStringToken;
      FOptions.Add('Title='+FTitle);
    end
    else if TokenSymbolIs('Description') then
    begin
      if FOptions.IndexOfName('Description') > 0 then
        ErrorStr('Duplicate Descripton found');
      GetStringToken(toCommandEnd);
      FDescription := FStringToken;
      FOptions.Add('Description='+FDescription);
    end
    else if TokenSymbolIs('Format') then
    begin
      if FFoundFormat = True then
        ErrorStr('Duplicate Format command found');
      FIsTokenPending := True;
      ReadFormat;
      FFoundFormat := True;
    end
    else
      Break;
  end; // end of while
  Result := True;
end;

function TLexDist.ReadDistMatrix: Boolean;  // rewritten for mega2b2
var
  aInfo: TGeographicalInfo = nil;
  AStr: AnsiString = '';
  aMsg: AnsiString = '';
  TotalTaxa: Integer;
  d: Double;
  fromY, toY, toX, CurX, CurY, i, j: Integer;
  TaxaNameBlockEnd : Boolean;

  function HasTaxa(const HT_AStr: AnsiString): Boolean;
  var
    ht_i: Integer;
  begin
    Result := False;
    for ht_i := 0 to FTaxa.Count-1 do
      if CompareText(FTaxa[ht_i], HT_AStr) = 0 then
      begin
        Result := True;
        Exit;
       end;
  end;
begin
  Result := False;
  // first read all the labels
  TotalTaxa := 0;
  TaxaNameBlockEnd := False;

  while True do
  begin
    NextToken;
    case Token of  // this part is rewritten in M2build2
      toPound:
        begin
          ReadOtuLabel;
          TrimTaxaName(OtuLabel);
          if Length(GpLabel) > 0 then
            TrimTaxaName(GpLabel);
          if Length(SpLabel) > 0 then
            TrimTaxaName(SpLabel);
          if Length(PopLabel) > 0 then
            TrimTaxaName(PopLabel);
          if Length(ContinentLablel) > 0 then
            TrimTaxaName(ContinentLablel);
          if Length(CountryLabel) > 0 then
            TrimTaxaName(CountryLabel);
          if Length(CityLabel) > 0 then
            TrimTaxaName(CityLabel);
          if HasTaxa(OtuLabel) then
             ErrorStr('MEGA has detected duplicate taxa labels.');
          aInfo := TGeographicalInfo.Create;
          if not aInfo.InitFromLabels(GpLabel, SpLabel, PopLabel, ContinentLablel, CountryLabel, CityLabel, YearLabel, MonthLabel, DayLabel, TimeLabel, aMsg) then
            ErrorStr(Format('Failed to parse otu name command statement(%s) - %s', [FullLabel, aMsg]));
          FGeographicalInfoList.Add(aInfo);
          FTaxa.Add(OtuLabel);
          if Length(GpLabel) > 0 then
            FGps.Add(GpLabel)
          else
            FGps.Add(EmptyStr);
          Inc(TotalTaxa);
        end;
      toCommand:
        ErrorStr('Command statements are not allowed in the distance data files.');
      toComment:
        GetStringToken(toCommentEnd);
      toCommentMEGA1:
        GetStringToken(toCommentMEGA1);
    else
      begin
        FIsTokenPending := True;
        TaxaNameBlockEnd := True;
        break;
      end;
    end;
    if TaxaNameBlockEnd then
      break;
  end;

  // we mut have a token at this time
  if Token = toEOF then
    ErrorStr('Unexpected end of file while reading taxa names.');

  if (FNoOfOtus > 0) and (TotalTaxa <> FNoOfOTUs) then
    ErrorStr(Format('Number of declared taxa (%d in !Format section) doens''t match the number MEGA read (%d).  Check for invalid characters in taxa names; Taxa which are missing or added but not declared in the !Format section.', [FNoOfOtus, TotalTaxa]));

  // Now we read the matrix
  if TotalTaxa > 2 then
  begin
    FDistMatrix := NewDistMatrix(TotalTaxa, False);
    for i:= 1 to FNoOfOtus-1 do
      for j:= 0 to i-1 do
        FDistMatrix[i][j] := 0.0;
  end
  else
    ErrorLimitationStr('Pairwise distance for at least 3 taxa should be included in the file.');

  //===== now read complete distance matrix
  fromY := 0;
  toY   := 0;
  case FDataFormat of
    snUpperMatrix:  begin fromY := 0; toY := TotalTaxa-2; end;
    snLowerMatrix:  begin fromY := 1; toY := TotalTaxa-1; end;
  end;

  curY := fromY;
  while True do  // external while loop; runs over curY
  begin
    toX  := 0;
    CurX := 0;
    case FDataFormat of
      snUpperMatrix: begin curX := curY+1; toX := TotalTaxa-1; end;
      snLowerMatrix: begin curX := 0; toX := curY-1; end;
    end;

    while True do  // internal while loop; runs over curX
    begin
      d := InvalidDistValue;
      if Token = toSymbol then
      begin
        if TokenSymbolIs('?') then
          d := InvalidDistValue
        else if
          TokenSymbolIsFloat(TokenString) then d := TokenFloat
        else
          ErrorStr('Expecting a distance value, but found :'+TokenString);
      end;

      FIsTokenPending := False;

      if d < 0.0 then
        d := InvalidDistValue;

      case FDataFormat of
        snUpperMatrix: FDistMatrix[CurX][CurY] := d;
        snLowerMatrix: FDistMatrix[CurY][CurX] := d;
      end;

      NextToken;
      while Token = toComment do
      begin
        GetStringToken(toCommentEnd);
        NextToken;
      end;
      while Token = toCommentMEGA1 do
      begin
        GetStringToken(toCommentMEGA1);
        NextToken;
      end;

      Inc(CurX);
      if CurX > toX then
        Break;

      if Token = toEOF then
        ErrorStr('End of file seen prematurely while reading the distance pair '+IntToStr(CurY+1)+','+IntToStr(CurX+1));
    end;
    Inc(curY);
    if CurY > toY then
      Break;
    if Token = toEOF then
      ErrorStr('End of file seen prematurely at the distance pair '+IntToStr(CurY+1)+','+IntToStr(CurX+1));
  end;

  //===
  if (FNoOfOtus > 0) and (FNoOfOtus <> TotalTaxa) then
  begin
    AStr := 'The specified number of sequences in the format statement ('+
             IntToStr(FNoOfOtus)+') does not match the number of taxa found by MEGA ('+
             IntToStr(TotalTaxa)+').  ';
    if MessageDlg(AStr + 'Ignore the difference and proceed with what MEGA read?', mtConfirmation, [mbYes,mbNo], 0) <> mrYes then
      ErrorStr(AStr);
  end;
  FNoOfOtus := TotalTaxa;
  if FNoOfOtus = 0 then
    ErrorStr('No taxa found');
  Result := True;

  if IsPhyloQAnalysis then // for the metagenomics analysis, so that we search using the first taxon instead of the last. This assumes that a lower triangular distance matrix is used.
    begin
      SwapDistEntries(FDistMatrix, TotalTaxa, 0, TotalTaxa - 1);
      FTaxa.Exchange(0, TotalTaxa - 1);
    end;
end;

procedure TLexDist.UpdateOptions(Variable, Option: AnsiString);
begin
  if FOptions <> nil then FOptions.Add(Variable+'='+Option);
  Option := Lowercase(Option);
  Variable := Lowercase(Variable);

  if CompareText(Variable, 'datatype') = 0 then
  begin
    if      CompareText(Option, 'distance')        = 0 then FDataType := snDistance
    else
      Error(SInvalidProperty);
  end
  else if CompareText(Variable, 'ntaxa') = 0 then
  begin
    FNoOfOtus := StrToInt(Option);
  end
  else if CompareText(Variable, 'dataformat') = 0 then
  begin
    if      CompareText(Option, 'lowerleft') = 0 then FDataFormat  := snLowerMatrix
    else if CompareText(Option, 'upperright')= 0  then FDataFormat := snUpperMatrix
    else if CompareText(Option, 'column') = 0  then FDataFormat    := snColumn
    else
      Error(SInvalidProperty);
  end
  else
    ErrorStr('Invalid statement in format');
end;

procedure TLexDist.updateDefaults;
begin
  if FDataFormat = snNoToken then
    FDataFormat := snLowerMatrix;
end;

end.

