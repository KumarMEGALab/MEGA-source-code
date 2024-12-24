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

unit mmodeltestmatrixexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MMatrixExport, MegaConsts;

type

  { TModelTestMatrixExport }

  TModelTestMatrixExport = class(TMatrixExport)
    private
      ModelTestData, SplitString: TStringList;
    protected
      procedure SaveToTargetDestination(ExportType: TExportType; var SaveLocation: String); override;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetDataAsCSVStringList(Data: TStringList);
      procedure DoResultsExport(ExportType: TExportType; SaveLocation: String = ''); override;
      function ExportProcedureString: String; override;
      procedure Assign(Source: TModelTestMatrixExport);
  end;

implementation

uses
  {$IFDEF VISUAL_BUILD}MEditorForm, {$ENDIF}
  ExcelWrite, MegaUtils;

procedure TModelTestMatrixExport.SaveToTargetDestination(ExportType: TExportType; var SaveLocation: String);
begin
  if SaveLocation = EmptyStr then
    SaveLocation := GetSaveLocation(ExportType);
  if SaveLocation <> EmptyStr then
  begin
    if ExportType <> EXtext then
      SavedExcelWrite.SaveFile(SaveLocation, ExcelExportToFileType(ExportType));
    {$IFDEF VISUAL_BUILD}
    if SavedExcelWrite.IsXLS and hasExcel and ((ExportType = EXexcelDisp) or (ExportType = EXexcelXmlDisp) or (ExportType = EXodsDisp) or (ExportType = EXcsvDisp)) then
      RunAProgram(SaveLocation)
    else if (ExportType = EXtext) then
     OpenStringList(ModelTestData, '*Model Selection Data')
    else if (ExportType = EXcsvDisp) then
    begin
      OpenFileAndFocus(SaveLocation, 0, 0);
    end
    {$ELSE}
     if (ExportType = EXtext) then
       ModelTestData.SaveToFile(SaveLocation)
     else if (ExportType = EXcsvDisp) then
       SavedExcelWrite.SaveFile(SaveLocation, ExportCSV)
     {$ENDIF}
    else if (ExportType = EXcsvSave) then
      SavedExcelWrite.SaveFile(SaveLocation, ExportCSV);
  end;
end;

constructor TModelTestMatrixExport.Create;
begin
  ModelTestData := nil;
  SplitString := nil;
  SavedExcelWrite := nil;
end;

destructor TModelTestMatrixExport.Destroy;
begin
  if Assigned(ModelTestData) then
    ModelTestData.Free;
  if Assigned(SplitString) then
    SplitString.Free;
  inherited Destroy;
end;

procedure TModelTestMatrixExport.SetDataAsCSVStringList(Data: TStringList);
begin
  ModelTestData := TStringList.Create;
  ModelTestData.Assign(Data);
end;

procedure TModelTestMatrixExport.DoResultsExport(ExportType: TExportType; SaveLocation: String);
var
  i,j:integer;
  TempFloat : Double;
  TempInt : integer;
begin
  if not Assigned(SavedExcelWrite) then
  begin
    SavedExcelWrite := TExcelWrite.Create(nil);
    SavedExcelWrite.IsXLS := True;
    if (ExportType <> EXtext) then
    begin
      for i:=0 to (ModelTestData.Count -1) do           //Data is CSV seperated by column and each new string in string list is a new row
      begin
        SplitString := TStringList.Create;
        splitStr(ModelTestData[i], ',', SplitString);  //Split by the ',' to get each element this procedure correctly handles spaces unlike the stringlist's delimited text function
        for j:=0 to (SplitString.Count -1) do
        begin
          if TryStrToFloat(SplitString.strings[j], TempFloat)  then    //If its a float, then add it to excel as a float
            SavedExcelWrite.Add(TempFloat)
          else if TryStrToInt(SplitString.strings[j], TempInt) then    //If its an int, add it to excel as an int
            SavedExcelWrite.Add(TempInt)
          else
            SavedExcelWrite.Add(SplitString.Strings[j]);    //Since we can definately represent it as a string for all other cases display it as a string
        end;
        SavedExcelWrite.WriteLine(0);
        FreeAndNil(SplitString);
      end;
    end;
  end;
  SaveToTargetDestination(ExportType, SaveLocation);
end;

function TModelTestMatrixExport.ExportProcedureString: String;
begin
  Result := 'model_selection';
end;

procedure TModelTestMatrixExport.Assign(Source: TModelTestMatrixExport);
begin
  DataType := Source.DataType;
  if Assigned(Source.FCaption) then
  begin
    FCaption := TStringList.Create;
    FCaption.Assign(Source.FCaption);
  end;
  if Assigned(Source.ModelTestData) then
  begin
    ModelTestData := TStringList.Create;
    ModelTestData.Assign(Source.ModelTestData);
  end;
  if Assigned(Source.SplitString) then
  begin
    SplitString := TStringList.Create;
    SplitString.Assign(Source.SplitString);
  end;
end;

end.

