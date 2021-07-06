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

unit mmodel_test_results;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, MAnalysisInfo, mmodeltestmatrixexport, ExcelWrite, MLegendGenerator,
  LCLIntf, LCLType;

type

  { TModelTestResults }

  TModelTestResults = class(TObject)
    private
      FAnalysisInfo: TAnalysisInfo;
      FCaptionExpert: TLegendGenerator;
      FMatrixExport: TModelTestMatrixExport;
      FCaption: TStringList;
      procedure GenerateMatrixExport;
      procedure GenerateCaptionExpert;
      procedure GenerateCaption;
    public
      constructor Create(aInfo: TAnalysisInfo);
      function GetMatrixExport: TModelTestMatrixExport;
      function GetCaptionExpert: TLegendGenerator;
      function GetCaption: TStringList;
  end;

  function CompAICc(Item1, Item2: Pointer): Integer;
  function CompBIC(Item1, Item2: Pointer): Integer;

implementation

uses
  MLTree, Graphics, MegaUtils, MegaConsts;

function CompAICc(Item1, Item2: Pointer): Integer;
begin
  if TModelInfo(Item1).AICc > TModelInfo(Item2).AICc then
    result := 1
  else if TModelInfo(Item1).AICc < TModelInfo(Item2).AICc then
    result := -1
  else
    result := 0;
end;

function CompBIC(Item1, Item2: Pointer): Integer;
begin
  if TModelInfo(Item1).BIC > TModelInfo(Item2).BIC then
    result := 1
  else if TModelInfo(Item1).BIC < TModelInfo(Item2).BIC then
    result := -1
  else
    result := 0;
end;

{ TModelTestResults }

procedure TModelTestResults.GenerateMatrixExport;
var
  RowColor: TColor;
  colorRow: boolean;
  aRect: TRect;
  listofentries : String;
  max: integer;
  k: integer;
  i, j: Integer;
begin
  FMatrixExport := TModelTestMatrixExport.Create;
  FMatrixExport.SavedExcelWrite := TExcelWrite.Create(nil, 'Info');
  FMatrixExport.SavedExcelWrite.IsXLS := True;
  FMatrixExport.SavedExcelWrite.AddWorksheet('Caption');
  if FAnalysisInfo.isAminoAcid then
    listofentries  := 'ARNDCQEGHILKMFPSTWYV'
  else
    listofentries := 'ATCG';

  FMatrixExport.SavedExcelWrite.Add('Model');
  FMatrixExport.SavedExcelWrite.Add('#Param');
  FMatrixExport.SavedExcelWrite.Add('BIC');
  FMatrixExport.SavedExcelWrite.Add('AICc');
  FMatrixExport.SavedExcelWrite.Add('lnL');
  FMatrixExport.SavedExcelWrite.Add('Invariant');
  FMatrixExport.SavedExcelWrite.Add('Gamma');

  if not FAnalysisInfo.isAminoAcid then
    FMatrixExport.SavedExcelWrite.Add('R');

  for i := 1 to Length(listofentries) do
    FMatrixExport.SavedExcelWrite.Add('Freq ' + listofentries[i]);

  if not FAnalysisInfo.isAminoAcid then   //Display changes list i.e. A=>T A=>G  A=>C headers
    for i := 1 to Length(listofentries) do
      for j := 1 to Length(listofentries) do
          if i<> j then
            FMatrixExport.SavedExcelWrite.Add(listofentries[i] +'=>'+ ListOfentries[j]);

  FMatrixExport.SavedExcelWrite.WriteLine();

  aRect.Top := 0;
  aRect.Bottom := 0;
  aRect.Left := 0;
  aRect.Right := Length(listofentries) + 7 + Length(listofentries)*(Length(listofentries) - 1) - 1;
  if not FAnalysisInfo.isAminoAcid then
    aRect.Right := aRect.Right + 1;
  FMatrixExport.SavedExcelWrite.BoldCells(aRect);
  FMatrixExport.SavedExcelWrite.AlignCells(aRect, aCenter);
  for i := 0 to FAnalysisInfo.MyMLAnalysisPack.ModelInfoList.Count-1 do
  with FAnalysisInfo.MyMLAnalysisPack.ModelInfoList[i] do
  begin
    aRect := FMatrixExport.SavedExcelWrite.LastCellWriteXY();
    colorRow := ((aRect.top mod 2) = 0);
    if ColorRow then
      RowColor := RGB(210,210,210)
    else
      RowColor := RGB(255, 255, 255);
    max := Length(listofentries)-1;

    FMatrixExport.SavedExcelWrite.Add(ModelName, RowColor);
    FMatrixExport.SavedExcelWrite.Add(NoOfParams, RowColor);
    FMatrixExport.SavedExcelWrite.Add(BIC, RowColor);
    FMatrixExport.SavedExcelWrite.Add(AICc, RowColor);
    FMatrixExport.SavedExcelWrite.Add(LogL, RowColor);

    if Pos('+I', ModelName) > 0 then
      FMatrixExport.SavedExcelWrite.Add(Invar, RowColor)
    else
      FMatrixExport.SavedExcelWrite.Add('n/a', RowColor);

    if Pos('+G', ModelName) > 0 then
      FMatrixExport.SavedExcelWrite.Add(Gamma, RowColor)
    else
      FMatrixExport.SavedExcelWrite.Add('n/a', RowColor);

    if not FAnalysisInfo.isAminoAcid then
      FMatrixExport.SavedExcelWrite.Add(SVR, RowColor);

    for j :=0 to max do
      FMatrixExport.SavedExcelWrite.Add(Freq[j], RowColor);

    if not FAnalysisInfo.isAminoAcid then  // Display changes list A=>T A=>G  A=>C  values i.e.  0.16, 0.1, etc.
    begin
      for j := 0 to max  do
        for k := 0 to max do
        if j <> k  then
          FMatrixExport.SavedExcelWrite.Add(FloatToPrecision(Matrix[j][k], 2), RowColor);
    end;
    FMatrixExport.SavedExcelWrite.WriteLine();
    FMatrixExport.SavedExcelWrite.AddWorksheet(ModelName);
    FMatrixExport.SavedExcelWrite.Add('From\To');
    for j := 0 to Length(listofentries)-1 do
      FMatrixExport.SavedExcelWrite.Add(listofentries[j+1]);
    FMatrixExport.SavedExcelWrite.WriteLine(i+2);

    for j := 0 to max  do
    begin
      FMatrixExport.SavedExcelWrite.Add(listofentries[j+1]);
      for k := 0 to max do
        if j <> k  then
          FMatrixExport.SavedExcelWrite.Add(Matrix[j][k])
        else
          FMatrixExport.SavedExcelWrite.Add('-');

      FMatrixExport.SavedExcelWrite.WriteLine(i+2);
    end;
    aRect.Left := 0;
    aRect.Right := max+1;
    aRect.Bottom := max+1;
    aRect.top := 0;
    FMatrixExport.SavedExcelWrite.AlignCells(aRect, aCenter, aNone, i+2);
    aRect.Bottom := 0;
    FMatrixExport.SavedExcelWrite.ColorCells(aRect, 0, xlBorderBottom,i+2);
    aRect.Bottom := Max+1;
    aRect.Right := 0;
    FMatrixExport.SavedExcelWrite.ColorCells(aRect, 0, xlBorderRight, i+2);
  end;
  FCaption := GetCaption;
  FMatrixExport.SavedExcelWrite.AddCaptionAsWorksheet(FCaption, 1);
end;

procedure TModelTestResults.GenerateCaptionExpert;
var
  TableInHTML: AnsiString;
  listofentries : AnsiString;
  max: integer;
  k: integer;
  i, j: Integer;
begin
    FCaptionExpert := TLegendGenerator.Create;
    FCaptionExpert.LoadTemplateFromFile('Model_Test_Template.htm');
    FCaptionExpert.AssignData('NoOfSeqs', IntToStr(FAnalysisInfo.NoOfSeqs));
    FCaptionExpert.AssignData('NoOfBLenPara', IntToStr(2*FAnalysisInfo.NoOfSeqs-3));
    FCaptionExpert.AssignData('NoOfSites', IntToStr(FAnalysisInfo.NoOfSites));
    FCaptionExpert.AssignData('NoOfGammaCat', IntToStr(FAnalysisInfo.MyMLAnalysisPack.DefaultNoOfRates));
    if FAnalysisInfo.MyMLAnalysisPack.IsInitialTree then
      FCaptionExpert.AssignData('TreeUsedString','For estimating ML values, a user-specified topology was used.')
    else
      FCaptionExpert.AssignData('TreeUsedString','For estimating ML values, a tree topology was automatically computed.');
    if FAnalysisInfo.isAminoAcid then
      listofentries  := 'ARNDCQEGHILKMFPSTWYV'
    else
      listofentries := 'ATCG';

    if FAnalysisInfo.isAminoAcid then
      max := 19
    else
      max := 3;
    TableInHTML := '<TABLE class="preventwrap"><TR><TH>Model </TH><TH>Parameters </TH>'+
                   '<TH>BIC </TH><TH>AICc </TH><TH><i>lnL</i> </TH>'+
                   '<TH>(+<i>I</i>) </TH><TH>(+<i>G</i>) </TH>';
    if not FAnalysisInfo.isAminoAcid then
      TableInHTML := TableInHTML + '<TH><i>R</i> </TH>';
    for i := 1 to max+1 do
      TableInHTML := TableInHTML + '<TH><i>f</i>(' + listofentries[i] +') </TH>';
    for i := 1 to max+1 do
    begin
      for j := 1 to max+1 do
      begin
        if i <> j then
          TableInHTML := TableInHTML + '<TH><i>r</i>(' + listofentries[i] + listofentries[j] + ') </TH>';
      end;
    end;
    TableInHTML := TableInHTML + '</TR>';

    for i :=0 to FAnalysisInfo.MyMLAnalysisPack.ModelInfoList.Count-1 do
    begin
      if i = FAnalysisInfo.MyMLAnalysisPack.ModelInfoList.Count-1 then
        TableInHTML := TableInHTML + '<TR class="lastRow">'
      else
        TableInHTML := TableInHTML + '<TR>';

      with FAnalysisInfo.MyMLAnalysisPack.ModelInfoList[i] do
      begin
        TableInHTML := TableInHTML + '<TD>'+ModelName+'</TD>';
        TableInHTML := TableInHTML + '<TD>'+IntToStr(NoOfParams)+' </TD>';
        TableInHTML := TableInHTML + '<TD>'+FloatToStrF(BIC, ffFixed, 12, lnLDigits)+' </TD>';
        TableInHTML := TableInHTML + '<TD>'+FloatToStrF(AICc, ffFixed, 12,  lnLDigits)+' </TD>';
        TableInHTML := TableInHTML + '<TD>'+FloatToStrF(LogL, ffFixed, 12, lnLDigits)+' </TD>';

        if Pos('+I', ModelName) > 0 then
          TableInHTML := TableInHTML + '<TD>' + FloatToStrF(Invar, ffFixed, 12, paraDigits)+' </TD>'
        else
          TableInHTML := TableInHTML + '<TD>n/a </TD>';
        if Pos('+G', ModelName) > 0 then
          TableInHTML := TableInHTML + '<TD>' + FloatToStrF(Gamma, ffFixed, 12, paraDigits)+' </TD>'
        else
          TableInHTML := TableInHTML + '<TD>n/a </TD>';
        if not FAnalysisInfo.isAminoAcid then
          TableInHTML := TableInHTML + '<TD> '+FloatToStrF(FloatToPrecision(FAnalysisInfo.MyMLAnalysisPack.ModelInfoList[i].SVR, paraDigits), ffFixed, 12, paraDigits)+' </TD>';

        for j :=0 to max do
          TableInHTML := TableInHTML + '<TD>' +FloatToStrF(Freq[j], ffFixed, 12, freqDigits)+' </TD>';

        for j := 0 to max  do
          for k := 0 to max do
            if j <> k  then
              TableInHTML := TableInHTML + '<td>' + FloatToStrF(Matrix[j][k], ffFixed, 12, freqDigits) + ' </td>';
        TableInHTML := TableInHTML + '</TR>';
      end;
    end;
    TableInHTML := TableInHTML + '</TABLE>';
    FCaptionExpert.AssignData('MatrixTable', TableInHTML);
    FCaptionExpert.BindData(FAnalysisInfo, True);
    FCaptionExpert.BindData(FAnalysisInfo.MyDistPack, True);
end;

procedure TModelTestResults.GenerateCaption;
begin
  FCaptionExpert := GetCaptionExpert;
  FCaption := TStringList.Create;
  FCaptionExpert.GenerateNoteAsText(FCaption);
end;

constructor TModelTestResults.Create(aInfo: TAnalysisInfo);
begin
  FAnalysisInfo := aInfo;
  FAnalysisInfo.MyMLAnalysisPack.ModelInfoList.Sort(CompBIC);
  FMatrixExport := nil;
  FCaptionExpert := nil;
  FCaption := nil;
end;

function TModelTestResults.GetMatrixExport: TModelTestMatrixExport;
begin
  if not Assigned(FMatrixExport) then
    GenerateMatrixExport;
  Result := FMatrixExport;
end;

function TModelTestResults.GetCaptionExpert: TLegendGenerator;
begin
  if not Assigned(FCaptionExpert) then
    GenerateCaptionExpert;
  Result := FCaptionExpert;
end;

function TModelTestResults.GetCaption: TStringList;
begin
  if not Assigned(FCaptionExpert) then
    GenerateCaptionExpert;
  if not Assigned(FCaption) then
    GenerateCaption;
  Result := FCaption;
end;

end.

