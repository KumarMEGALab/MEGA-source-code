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

unit ExcelWrite;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  FileUtil, LCLIntF, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, fpspreadsheet, fpstypes, fpsallformats, Registry, MPleaseWait, MegaErrUtils,
  MegaConsts, fpjson, jsonparser {$IFDEF VISUAL_BUILD},MMultiStageProgress{$ENDIF};

const
  XLROTATIONNONE = 0;
  XLROTATIONTEXTVERTICALCHARHORIZONTAL = 255;
  XLROTATIONVERTICALUP = 90;
  XLROTATIONVERTICALDOWN = 180;
  MAX_COLS = 1024;

type
  TAlign = (aTop, aCenter, aBottom, aLeft, aRight, aNone);
  TCellBorderIndex =
   (xlBorderLeft,
    xlBorderRight,
    xlBorderTop,
    xlBorderBottom,
    xlBorderAll);

  TXLBorderStyle =
   (bsNone,
    bsThin,
    bsMedium,
    bsDashed,
    bsDotted,
    bsThick,
    bsDouble,
    bsHair,
    bsMediumDashed,
    bsDashDot,
    bsMediumDashDot,
    bsDashDotDot,
    bsMediumDashDotDot,
    bsSlantedDashDot);


  { TExcelWrite }

  TExcelWrite = class
    private
      FRichTextParams: array of TsRichTextParam;
      FSpreadsheetFormat: TsSpreadsheetFormat;
      FWorkbook: TsWorkbook;
      IsExcelOutput: boolean;
      OutputLine: Variant;
      HighlightLine: Array[1..16384] of TColor;
      FontLine: Array[1..16384] of TColor;
      Rotation: Array[1..16384] of Byte;
      PrecisionFormat: Array[1..16384] of Integer;
      AutoColWidths: T2dArrayOfInteger;
      TempColWidths: Array[1..16384] of Integer;
      OutputLinePos: Integer;
      OutputLineLevel: Array[0..200] of Integer;
      MaxColumns: Integer;
      RowsPerLevel: Integer;
      FVisible : Boolean;
      FAutoFitCols: Boolean;
      function getLineLvl(Index: Integer): Integer;
     // Gets the current level of the output line for the specified worksheet
      function ByteToRotationType(aByte: Byte): TsTextRotation;
      function GetRichTextParams(aFormat: String): TsRichTextParams;
      function GetNumberFormat(aFormat: String): TsNumberFormat;
      function GetCellBorder(borderIndex: TCellBorderIndex): TsCellBorders;
      procedure SetSpreadsheetFormat(AValue: TsSpreadsheetFormat);
      procedure UpdateOutputFormat(OutFormat: TOutputFileType);
      procedure InitPrecision;
      procedure InitAutoColWidths(sheet: Integer);
      procedure UpdateColWidths(sheet: Integer);
      function IsNumericType(BasicType: Integer): Boolean;
      function IsFloatingPointType(BasicType: Integer): Boolean;
      function IsLargeFile: Boolean; // true if we should show a PleaseWait dialog when saving
    public
      Font: TFont;
      CurStr: AnsiString;
      WaitDlg: TPleaseWait;
      ColumnSize: Array[1..1025] of Integer;
      ShowPleaseWait: Boolean;
      {$IFDEF VISUAL_BUILD}
      MSProgress: TMultiStageProgressForm;
      {$ENDIF}
      function ChangeFileExtForOutputFileType(filename: String; aType: TOutputFileType): String;
      procedure BoldCells(aRect: TRect);
      procedure ItalicizeCells(aRect: TRect);
      procedure BoldItalicizeCells(aRect: TRect);
      procedure SetFontSize(Worksheet, Column, Row, FontSize: integer);
      procedure SetFontColor(Worksheet, Column, Row: Integer; FontColor: TColor);
      procedure WrapRow(WorkSheet, RowToWrap: Integer; NumCols: Integer);
      procedure SetColumnWidth(Worksheet, Col, NumChars: Integer);
      procedure SetRowHeight(Worksheet, Row, HeightInChars: Integer);
      function Add(toadd: Variant; highlight: TColor = clWhite; fontcolor: TColor = clWhite; rotate : integer = xlRotationNone): Boolean;
      function AddBlankCell(highlight: TColor = clWhite): Boolean;
      function AddBlankCells(NumBlankCells: Integer; highlight: TColor = clWhite): Boolean;
      function AddP(toadd: Variant; Precision: Integer; highlight: TColor = clWhite; fontcolor: TColor = clWhite): Boolean;  // Short for Add Precision.  This always adds full precision to Excel but truncates precision to the desired length for CSV.
      procedure Empty;
      procedure AlignCells(aRect : TRect; HAlign: TAlign = aNone; VAlign: TAlign = aNone; WorkSheet : Integer = 0);
      procedure WriteKeypair(first: Variant; second: Variant; Worksheet: Integer = 0; Start: AnsiChar = 'A');
      procedure WriteLine(Worksheet: Integer = 0; Start: AnsiChar = 'A'; aFormat: String = ''; MergeRow: Boolean = false); overload;
      procedure WriteLine(Line: Variant; Worksheet: Integer = 0; Start: Char = 'A'; Format: String = ''; Highlight: TColor = clWhite); overload;
      procedure ApplyFormat(Left, Top, Right, Bottom: Integer; numFormat: String; worksheet: Integer = 0);
      function AddWorksheet(SheetName: String = 'WorkSheet'): Integer;
      procedure RemoveWorksheet(index: Integer);
      procedure AddCaptionAsWorksheet(captionStrings: TStringList; aName: String = 'Caption'); overload;
      procedure AddCaptionAsWorksheet(captionStrings: TStringList; sheet: Integer); overload;
      procedure NewLine(worksheet: Integer = 0);
      procedure SetIsExcelOutput(XLS: Boolean);
      procedure HadError;
      procedure MergeCells(aRect: TRect; HAlign: TAlign = aNone; VAlign: TAlign = aNone);
      procedure ColorCells(aRect: TRect; Color: TColor; BorderType : TCellBorderIndex = xlBorderAll; Sheet : Integer = 0);
      function LastCellWriteXY(worksheet:integer=0): TRect;
      procedure ColWidth(Range: TRect; Width, worksheet: Integer);
      procedure AutoSizeColumns;
      function SaveFile(SaveTo : String; OutFormat: TOutputFileType; IsInGuiThread: Boolean=True): Boolean; //saves the file based on the string passed to it if user doesn't have excel.  If user has excel empty string is passed to it
      function SaveFileNoAutoFit(SaveTo: String): Boolean; // use this one when launching the writer in a separate thread and using your own please wait
      function GetCsvText: String;
      function GetCsvList: TStringList;
      function GetJsonText: String;
      function GetAsTabDelimitedText: String;
      function GetAsTabDelimitedList: TStringList;
      function PleaseWaitDlg(action: String ): TPleaseWait;
      Constructor Create(Sender: TForm; FirstSheetName: String = 'Tab1');
      Destructor  Destroy; override;
      property Visible: Boolean read FVisible write FVisible;
      property IsXLS: Boolean read IsExcelOutput write SetIsExcelOutput;
      property SpreadsheetFormat: TsSpreadsheetFormat read FSpreadsheetFormat write SetSpreadsheetFormat;
      Property OutputLineLvl[Index: Integer] : Integer read getLineLvl;
      Property OutputPos: Integer read OutputLinePos;
      property MaxCols: Integer read MaxColumns;
      property RowsPerLvl: Integer read RowsPerLevel write RowsPerLevel;
      property AutoFitCols: Boolean read FAutoFitCols write FAutoFitCols;
    end;

    TExcelWriteDoneCallBack = procedure(AThread: TThread; Status: String) of object;
    TExcelWriteThread = class(TThread)
      private

      public
        Writer: TExcelWrite;
        FileName: String;
        CallBack: TExcelWriteDoneCallBack;
        //OnProgress: XLSBase.TProgressEvent;
        OnProgress: TNotifyEvent;

        constructor Create(CreateSuspended: Boolean);
        procedure Execute; override;
    end;

  function HasExcel(fileExt: String = '.xls'):Boolean;
  function GetSaveLocation(ExportType: TExportType=EXnone): String; overload;
  function GetSaveLocation(FileFormat: TOutputFileType): String; overload;
  function FileExtForExportType(ExportType: TExportType): String;
  function OutputFileTypeForFileExt(FileExt: String): TOutputFileType;
  procedure RunAProgram (const theProgram: String);

implementation

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV,{$ENDIF}MegaUtils, fpscsv, math, fpsclasses;

constructor TExcelWrite.Create(Sender: TForm; FirstSheetName: String);
var
  a: Integer;
begin
  WaitDlg := nil;
  InitPrecision;
  InitAutoColWidths(0);
  SetLength(FRichTextParams, 0);
  CSVParams.Delimiter := ',';
  FWorkbook := TsWorkbook.Create;
  FWorkbook.FormatSettings.ShortDateFormat := 'mmm/yy';
  ShowPleaseWait := True;
  FAutoFitCols := true;
  FSpreadsheetFormat := sfOOXML;
  OutputLinePos := 1; // starts at 1 so it coresponds to the excel cells
  for a := 0 to High(OutputLineLevel) do
    OutputLineLevel[a] := 1; // Keeps track of the current Row # to write, index of OutputLineLevel is the Worksheet you're targeting.

  OutputLine := VarArrayCreate([1, 16384], varVariant);  // The array is limited to 16384 since that is the number of maximum columns any version of Excel can support.
  for a := 1 to 16384 do  // The default value of an array of TColor is Black, so if we ever forgot to set the highlight to white it would be highlighted black without this loop.
  begin
    HighlightLine[a] := clWhite;
    FontLine[a] := clWhite;
  end;
  FWorkbook.AddWorksheet(FirstSheetName);
end;

procedure TExcelWrite.BoldCells(aRect: TRect);
var
  col, row: Integer;
  sheet: TsWorksheet;
begin
  if not IsExcelOutput then
    exit;
  Assert((aRect.Left < 1024) and (aRect.Right < 1024));
  sheet := FWorkbook.ActiveWorksheet;
  for col := aRect.Left to aRect.Right do
    for row := aRect.Top to aRect.Bottom do
      sheet.WriteFontStyle(row, col, [fssBold]);
end;

procedure TExcelWrite.ItalicizeCells(aRect: TRect);
var
  col, row: Integer;
  sheet: TsWorksheet;
begin
  if not IsExcelOutput then
    exit;
  Assert((aRect.Left < 1024) and (aRect.Right < 1024));
  sheet := FWorkbook.ActiveWorksheet;
  for col := aRect.Left to aRect.Right do
    for row := aRect.Top to aRect.Bottom do
      sheet.WriteFontStyle(row, col, [fssItalic]);
end;

procedure TExcelWrite.BoldItalicizeCells(aRect: TRect);
var
  col, row: Integer;
  sheet: TsWorksheet;
begin
  if not IsExcelOutput then
    exit;
  Assert((aRect.Left < 1024) and (aRect.Right < 1024));
  sheet := FWorkbook.ActiveWorksheet;
  for col := aRect.Left to aRect.Right do
    for row := aRect.Top to aRect.Bottom do
      sheet.WriteFontStyle(row, col, [fssBold, fssItalic]);
end;


function HasExcel(fileExt: String = '.xls'):Boolean;

  function GetExeByExtension(sExt : AnsiString) : AnsiString;
  var
     sExtDesc:AnsiString;
  begin
     result := EmptyStr;
     with TRegistry.Create do
     begin
       try
       try
         RootKey:=HKEY_CLASSES_ROOT;
         if OpenKeyReadOnly(sExt) then
         begin
           sExtDesc:=ReadString('') ;
           CloseKey;
         end;
         if sExtDesc <>'' then
         begin
           if OpenKeyReadOnly(sExtDesc + '\Shell\Open\Command') then
           begin
             Result:= ReadString('') ;
           end
         end;
       Except
         // do nothing, but we do need this Except, to handle the exception.
       end;
       finally
         Free;
       end;
     end;

     if Result <> EmptyStr then
     begin
       if Result[1] = '"' then
       begin
         Result:=Copy(Result,2,-1 + Pos('"',Copy(Result,2,MaxINt))) ;
       end
     end;
  end;

begin
  Result := False;
  {$IFDEF MSWINDOWS}
  result := (GetExeByExtension(fileExt) <> '');
  {$ELSE}
  Result := True; { at this point we should be able to assume that all computers have some application to open spreadsheets (Linux comes with Libre/Open Office)}
  {$ENDIF}
end;

procedure TExcelWrite.AlignCells(aRect: TRect; HAlign: TAlign; VAlign: TAlign;
  WorkSheet: Integer);
var
   col, row: Integer;
   sheet: TsWorksheet;
begin
  if not IsExcelOutput then
    Exit;
  sheet := FWorkbook.GetWorksheetByIndex(Worksheet);
  for col := aRect.Left to aRect.Right do
    for row := aRect.Top to aRect.Bottom do
    begin
      case HAlign of
        aCenter: sheet.WriteHorAlignment(row, col, haCenter);
        aLeft: sheet.WriteHorAlignment(row, col, haLeft);
        aRight: sheet.WriteHorAlignment(row, col, haRight);
        aNone: sheet.WriteHorAlignment(row, col, haDefault);
      end;
      case VAlign of
        aTop: sheet.WriteVertAlignment(row, col, vaTop);
        aCenter: sheet.WriteVertAlignment(row, col, vaCenter);
        aBottom: sheet.WriteVertAlignment(row, col, vaBottom);
        aNone: sheet.WriteVertAlignment(row, col, vaDefault);
      end;
    end;
end;


function GetSaveLocation(ExportType: TExportType=EXnone):String;
var
  TempFileName : String;
  ext: String;
  FileN : TSaveDialog;
  i, Response : integer;
begin
  TempFileName := EmptyStr;
  ext := FileExtensionForExportType(ExportType);
  // Check registery to see if any program is associated with the .xls extension ( will any program handle it when we open the file )
  try
    Result := ''; //If they have excel an empty string will be returned.
    If HasExcel(ext) then  //If they don't have something assocated with the excel extension
    begin
      i:= 0;
      while TempFileName = EmptyStr do  // Run through a version of this loop on MEGA close, and delete all the files you can ( ones that are not still open )
      begin
        if not FileExists(GetTempDir(False) + TEMPORARY_MEGA + IntToStr(i) + ext) then
          TempFileName := GetTempDir(False) + TEMPORARY_MEGA + IntToStr(i) + ext;
        Inc(i);
      end;
      Result := TempFileName;
    end
    else
    begin
      FileN := TSaveDialog.Create(Application);    //Get where to save the Excel file and return it from function
      if (ExportType = EXcsvSave) or (ExportType = EXcsvDisp) then
      begin
        FileN.DefaultExt := '*.csv';
        FileN.Filter := 'CSV (*.csv)|*.csv|All files | *.*';
      end
      else if (ExportType = EXodsSave) or (ExportType = EXodsDisp) then
      begin
        FileN.DefaultExt := '*.ods';
        FileN.Filter := 'Open/Libre Office (*.ods)|*.ods|All files | *.*';
      end
      else
      begin
        FileN.DefaultExt := '*.xls';
        FileN.Filter := 'Excel (*.xlsx, *.xls )|*.xlsx;*.xls|All files | *.*';
      end;

      FileN.FilterIndex := 1;
      FileN.InitialDir := ChangeInitialDirectorySaveDialogForMac(FileN.InitialDir);
      while FileN.Execute do
      begin
        Result := FileN.FileName;
        if FileExists(Result) then
        begin
          Response := MessageDlg('Confirm Overwrite', 'The selected filename already exists. Overwrite it?', mtConfirmation, mbYesNo, 0);
          if Response = mrYes then
            break
          else
            Result := EmptyStr;
        end
        else
          break;
      end;
      FreeAndNil(FileN);
    end;
  except
    on E : Exception do
      RaiseErrorMessage(-1, 'Unable to get location to save file Error:' + E.Message);
    end;
end;

function GetSaveLocation(FileFormat: TOutputFileType): String;
begin
  case FileFormat of
    ExportExelXML: Result := GetSaveLocation(EXexcelXmlSave);
    ExportExcel: Result := GetSaveLocation(EXexcelSave);
    ExportODS: Result := GetSaveLocation(EXodsSave);
    ExportCSV: Result := GetSaveLocation(EXcsvSave);
    ExportText: Result := GetSaveLocation(EXtext);
    ExportMega, ExportFasta, ExportNone, ExportInvalid: Result := GetSaveLocation(EXnone);
    else
      Result := GetSaveLocation(EXnone);
  end;
end;

function FileExtForExportType(ExportType: TExportType): String;
begin
  case ExportType of
    EXInvalid, EXnone: Result := EmptyStr;
    EXtext: Result := '.txt';
    EXcsvDisp, EXcsvSave: Result := '.csv';
    EXexcelDisp, EXexcelSave: Result := '.xls';
    EXexcelXmlDisp, EXexcelXmlSave: Result := '.xlsx';
    EXfasta: Result := '.fasta';
    EXodsDisp, EXodsSave: Result := '.ods';
    else
      Result := EmptyStr;
  end;
end;

procedure TExcelWrite.SetIsExcelOutput(XLS: Boolean);
begin
  if IsExcelOutput = XLS then Exit;
  IsExcelOutput := XLS;
end;

procedure TExcelWrite.HadError; // Call before freeing in exception blocks.  Will stop Excel from saving and show users their data in text format
begin
  IsExcelOutput := False; // Users will at least get their data as a text file in the even of an error
end;

destructor TExcelWrite.Destroy;
begin
  FWorkbook.Free;
  FreeAndNil(WaitDlg);
end;


function TExcelWrite.SaveFile(SaveTo: String; OutFormat: TOutputFileType; IsInGuiThread: Boolean=True):Boolean;
begin
  UpdateOutputFormat(OutFormat);
  try  try
    Result := False;
    if IsInGuiThread and IsLargeFile then
    begin
      WaitDlg := PleaseWaitDlg('Preparing: Fitting column(s)');
      Application.ProcessMessages;
    end;
      if FAutoFitCols and (OutFormat <> ExportCSV) then
        AutoSizeColumns;
      SaveTo := ChangeFileExtForOutputFileType(SaveTo, OutFormat);
      if HasExcel then  // If they can read it then save it as a temp file and just display it, othwerwise ask for a save location
      begin
        FWorkbook.WriteToFile(saveTo, FSpreadsheetFormat, FileExists(SaveTo));// Just a temporary file, gets written to then executed.
      end
      else
      begin
        if AnsiCompareStr(SaveTo, '') <> 0 then
        begin
          if IsInGuiThread and IsLargeFile then
            WaitDlg.Action := 'Writing File: May take a few minutes';
          FWorkbook.WriteToFile(SaveTo, FSpreadsheetFormat, FileExists(SaveTo));
        end;
      end;
      Result := True; //we've correctly saved the file
  Except
    on E: Exception do
      if IsInGuiThread and IsLargeFile then
        RaiseErrorMessage(-1, 'Unable to write Excel file.  The following error occured: ' + E.Message)
      else
        Result := False;
  end;
  finally
    if IsInGuiThread and IsLargeFile and Assigned(WaitDlg) then
    begin
      WaitDlg.UseProgressTimer(False);
      Sleep(600); { this is to ensure the ontimer procedure has finished or we can get an access violation}
      FreeAndNil(WaitDlg);
    end;
  end;
end;

function TExcelWrite.SaveFileNoAutoFit(SaveTo: String): Boolean;
begin
  try
    Result := False;
    if isExcelOutput then
    begin
      if HasExcel then  // If they can read it then save it as a temp file and just display it, othwerwise ask for a save location
        FWorkbook.WriteToFile(SaveTo, FSpreadsheetFormat)
      else
      begin
        FWorkbook.WriteToFile(SaveTo, FSpreadsheetFormat);
      end;
      Result := True; //we've correctly saved the file
    end;
  Except on E: Exception do
    RaiseErrorMessage(-1, 'Unable to write Excel file.  The following error occured: ' + E.Message);
  end;
end;

function TExcelWrite.GetCsvText: String;
var
  aList: TStringList = nil;
begin
  try
    aList := GetCsvList;
    Result := aList.Text;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TExcelWrite.GetCsvList: TStringList;
var
  filename: String;
begin
  filename := NextAvailableFilename(GetTemp + TEMPORARY_MEGA);
  FWorkbook.WriteToFile(filename, sfCSV, True);
  Result := TStringList.Create;
  Result.LoadFromFile(filename);
  try
    DeleteFile(filename);
  except

  end;
end;

function TExcelWrite.GetJsonText: String;
var
  ws: TsWorksheet = nil;
  row, col: Integer;
  enum: TsCellEnumerator = nil;
  p: PCell;
  cell: TCell;
  innerJson: TJSONArray = nil;
  outerJson: TJSONArray = nil;
  aTime: TDateTime;
begin
  try
    outerJson := TJSONArray.Create;
    ws := FWorkbook.ActiveWorksheet;
    for row := 0 to ws.GetLastOccupiedRowIndex do
    begin
      innerJson := TJSONArray.Create;
      if ws.GetLastOccupiedColIndex > 0 then
        for col := 0 to ws.GetLastOccupiedColIndex - 1 do
        begin
          p := ws.GetCell(row, col);
          cell := p^;
          case cell.ContentType of
            cctEmpty: innerJson.Add(' ');
            cctFormula: ;
            cctNumber: innerJson.Add(TJSONFloat(ws.ReadAsNumber(p)));
            cctUTF8String:innerJson.Add(ws.ReadAsText(p));
            cctDateTime:
              begin
                if ws.ReadAsDateTime(p, aTime) then
                  innerJson.Add(aTime);
              end;
            cctBool: innerJson.Add(cell.BoolValue);
            cctError: ;
            else
              innerJson.Add(ws.ReadAsText(p));
          end;
        end;
      outerJson.Add(innerJson);
    end;
    Result := outerJson.AsJSON;
  finally
    if Assigned(outerJson) then
      outerJson.Free;
  end;
end;

function TExcelWrite.GetAsTabDelimitedText: String;
var
  aList: TStringList = nil;
begin
  aList := GetAsTabDelimitedList;
  Result := aList.Text;
  aList.Free;
end;

function TExcelWrite.GetAsTabDelimitedList: TStringList;
begin
  CSVParams.Delimiter := #9;
  Result := GetCsvList;
end;

procedure TExcelWrite.NewLine(worksheet: Integer = 0);  // Simply moves down the next output when using Array method
begin
  Inc(OutputLineLevel[worksheet]);
end;

function TExcelWrite.AddWorksheet(SheetName: String = 'WorkSheet'): Integer;
begin
  Result := -1;
  if not IsExcelOutput then
    Exit;
  if Trim(SheetName) = EmptyStr then
    SheetName := 'Sheet' + IntToStr(FWorkbook.GetWorksheetCount);
  FWorkbook.AddWorksheet(SheetName, True);
  InitAutoColWidths(FWorkbook.GetWorksheetCount - 1);
  Result := (FWorkbook.GetWorksheetCount - 1);
end;

procedure TExcelWrite.RemoveWorksheet(index: Integer);
begin
  if (index >= 0) and (index < FWorkbook.GetWorksheetCount) then
    FWorkbook.RemoveWorksheet(FWorkbook.GetWorksheetByIndex(index));
end;

procedure TExcelWrite.AddCaptionAsWorksheet(captionStrings: TStringList; aName: String);
var
  index: Integer;
begin
  index := AddWorksheet(aName);
  AddCaptionAsWorksheet(captionStrings, index);
end;

procedure TExcelWrite.AddCaptionAsWorksheet(captionStrings: TStringList; sheet: Integer);
const
  COL_WIDTH = 150;
var
  line: Integer;
  aStr: String;
begin
  if captionStrings.Count > 0 then
    for line := 0 to captionStrings.Count - 1 do
    begin
      aStr := captionStrings[line];
      if Pos('Disclaimer:', aStr) = 1 then
       begin
         SetFontSize(sheet, 0, line, 8);
         SetFontColor(sheet, 0, line, RGB(100,100,100));
       end;
      WrapRow(sheet, line, 1);
        if Trim(aStr) <> EmptyStr then
      SetRowHeight(sheet, line, ceil(Length(aStr)/COL_WIDTH) + 1);
      WriteLine(aStr, sheet);
    end;
  AutoColWidths[sheet][0] := COL_WIDTH;
end;

function TExcelWrite.Add(toadd: Variant; highlight: TColor = clWhite; fontcolor: TColor = clWhite; rotate : integer = xlRotationNone): Boolean; // Adds a Variant to the next position in the Array
var
  TempString: String;
  BasicType: Integer;
begin
  Result := True;
  if (not IsExcelOutput) then
    exit;
  if OutputLinePos > MAX_COLS then
  begin
    result := False;
    Exit;
  end;

  BasicType := VarType(toadd) and varTypeMask;
  if BasicType = VarInteger then
    TempString := IntToStr(toadd)
  else if BasicType = varDouble then
    TempString := FloatToStr(toadd)
  else
    TempString := VarToStr(toadd);

  if TempString = EmptyStr then
    Exit;

  OutputLine[OutputLinePos] := toadd;
  HighlightLine[OutputLinePos] := highlight;
  FontLine[OutputLinePos] := fontcolor;
  Rotation[OutputLinePos] := rotate;
  TempColWidths[OutputLinePos] := Max(TempColWidths[OutputLinePos], Length(TempString));
  Inc(OutputLinePos);
end;

function TExcelWrite.AddP(toadd: Variant; Precision: Integer; highlight: TColor = clWhite; fontcolor: TColor = clWhite): Boolean;
begin
  Result := True;
  if (not IsExcelOutput) then
    Exit;
  if OutputLinePos > MAX_COLS then
  begin
    Result := False;
    Exit;
  end;
  OutputLine[OutputLinePos] := toadd; 
  HighlightLine[OutputLinePos] := highlight;
  FontLine[OutputLinePos] := fontcolor;
  Rotation[OutputLinePos] := xlRotationNone;
  PrecisionFormat[OutputLinePos] := Precision;
  TempColWidths[OutputLinePos] := Max(TempColWidths[OutputLinePos], Precision + 2);
  Inc(OutputLinePos);
end;

procedure TExcelWrite.Empty;
begin
  OutputLinePos := 1;
end;

procedure TExcelWrite.WriteKeypair(first: Variant; second: Variant; Worksheet: Integer = 0; Start: AnsiChar = 'A'); // There's a number of times when you need to add a KeyPair eg. Type: DNA, this function saves 2 lines by adding both and writing them to the next line using the Array method
begin
  Add(first);
  Add(second);
  WriteLine(Worksheet, Start);
end;

procedure TExcelWrite.WriteLine(Worksheet: Integer; Start: AnsiChar;
  aFormat: String; MergeRow: Boolean); // Main output method.  This writes the array to which we have been adding elements out to the next line of Excel
 var
   i, Row, Col, FStart: Integer;
   sheet: TsWorksheet;
   BasicType: Integer;
   rtp: TsRichTextParams;
   numFormat: TsNumberFormat;
   numDecimalsStr: String;
   dbl: Double;
 begin
   if not IsExcelOutput then
     Exit;
   if OutputLinePos > MaxColumns then
     MaxColumns := OutputLinePos;
   if Worksheet > FWorkbook.GetWorksheetCount - 1 then
   begin
     for i := 0 to (Worksheet - FWorkbook.GetWorksheetCount - 1) do
       sheet := FWorkbook.AddWorksheet('Sheet ' + IntToStr(i + 1), True);
     InitAutoColWidths(Worksheet);
   end;
     if Start <> 'A' then
     begin
       if Integer(Start) < 91 then
         Start := Char(Chr(Integer(Start) + 32)); // If it's uppercase, then add 32 to make it lowercase ( would normally use LowerCase but it doesn't with chars )
       FStart := Integer(Start)-96;
     end
     else
       FStart := 1; // 1 if it is A.
     Row := OutputLineLevel[Worksheet] - 1;
     sheet := FWorkbook.GetWorksheetByIndex(Worksheet);
     for i := FStart to OutputLinePos - 2 + FStart do
     begin
       Col := i - 1;
       BasicType := VarType(OutputLine[i - FStart + 1]) and varTypeMask;
       if IsNumericType(BasicType) then
       begin
         if aFormat <> EmptyStr then
         begin
           numFormat := GetNumberFormat(aFormat);
           sheet.WriteNumber(row, col, OutputLine[i - FStart + 1], numFormat, aFormat);
           if IsFloatingPointType(BasicType) then
           begin
             numDecimalsStr := '%.' + IntToStr(Length(aFormat) - 2) + 'f';
             dbl := Double(OutputLine[i - FStart + 1]);
             TempColWidths[col + 1] := Max(TempColWidths[col + 1], Length(Format(numDecimalsStr, [dbl])) + 1);
           end
           else
             TempColWidths[col + 1] := Max(TempColWidths[col + 1], Length(IntToStr(Integer(OutputLine[i - FStart + 1]))) + 1);
         end
         else
         begin
           sheet.WriteNumber(row, col, OutputLine[i - FStart + 1]);
           if BasicType = varDouble then
           begin
             dbl := Double(OutputLine[i - FStart + 1]);
             TempColWidths[col + 1] := Max(TempColWidths[col + 1], Length(Format('%.4f', [dbl])) + 1);
           end
           else
             TempColWidths[col + 1] := Max(TempColWidths[col + 1], Length(IntToStr(Integer(OutputLine[i - FStart + 1]))));
         end;
       end
       else
       begin
         if aFormat <> EmptyStr then
         begin
           rtp := GetRichTextParams(aFormat);
           sheet.WriteText(Row, Col, VarToStr(OutputLine[i - FStart + 1]), rtp);
         end
         else
           sheet.WriteText(Row, Col, VarToStr(OutputLine[i - FStart + 1]));
         TempColWidths[col + 1] := Max(TempColWidths[col + 1], Length(VarToStr(OutputLine[i - FStart + 1])));
       end;
       if (Rotation[i] <> xlRotationNone) then
         sheet.WriteTextRotation(Row, Col, ByteToRotationType(Rotation[i]));

       if (HighlightLine[i] <> clWhite) then
       begin
         sheet.WriteBackground(Row, Col, fsSolidFill, HighlightLine[i - FStart + 1]);
       end;
       if (FontLine[i] <> clWhite) then
         sheet.WriteFontColor(Row, Col, FontLine[i - FStart + 1]);
     end;
   if MergeRow then
     sheet.MergeCells(OutputLineLevel[worksheet]-1, FStart-1, OutputLineLevel[worksheet]-1, 16382-FStart);
   UpdateColWidths(Worksheet);
   Empty; // Clears the OutputLine and HighlightLine, sets output pos to 1;

   Inc(OutputLineLevel[worksheet]);
 end;

  procedure TExcelWrite.WrapRow(WorkSheet, RowToWrap: Integer; NumCols: Integer);
 var
   sheet: TsWorksheet;
   col: Integer;
 begin
   if not IsExcelOutput then
     exit;
   sheet := FWorkbook.GetWorksheetByIndex(Worksheet);
   for col := 0 to NumCols - 1 do
     sheet.WriteWordwrap(RowToWrap, col, True);
 end;

 procedure TExcelWrite.SetColumnWidth(Worksheet, Col, NumChars: Integer);
 var
   sheet: TsWorksheet;
 begin
   if not IsExcelOutput then
     Exit;
   sheet := FWorkbook.GetWorksheetByIndex(Worksheet);
   sheet.WriteColWidth(Col, NumChars, suChars);
 end;

 procedure TExcelWrite.SetRowHeight(Worksheet, Row, HeightInChars: Integer);
  var
   sheet: TsWorksheet;
 begin
   sheet := FWorkbook.GetWorksheetByIndex(Worksheet);
   sheet.WriteRowHeight(Row, HeightInChars, suLines);
 end;

 procedure TExcelWrite.SetFontSize(Worksheet, Column, Row, FontSize : integer);
 var
   sheet: TsWorksheet;
 begin
   if not IsExcelOutput then
     Exit;
   sheet := FWorkbook.GetWorksheetByIndex(Worksheet);
   sheet.WriteFontSize(Row, Column, FontSize);
 end;

 procedure TExcelWrite.SetFontColor(Worksheet, Column, Row : Integer; FontColor : TColor);
 var
   sheet: TsWorksheet;
 begin
   if not IsExcelOutput then
     Exit;
   sheet := FWorkbook.GetWorksheetByIndex(Worksheet);
   sheet.WriteFontColor(Row, Column, FontColor);
 end;

   procedure TExcelWrite.WriteLine(Line: Variant; Worksheet: Integer = 0; Start: Char = 'A'; Format: String = ''; Highlight: TColor = clWhite);
 var
   i, basicType: Integer;
   LineStr: AnsiString;
   sheet: TsWorksheet;
   row, col: Integer;
   rtp: TsRichTextParams;
   numFormat: TsNumberFormat;
 begin
   if not IsExcelOutput then
     Exit;
   if OutputLinePos > MaxColumns then
     MaxColumns := OutputLinePos;

   if worksheet > FWorkbook.GetWorksheetCount - 1 then
   begin
     for i :=0 to (worksheet - (FWorkbook.GetWorksheetCount - 1)) do
       sheet := FWorkbook.AddWorksheet('Sheet ' + IntToStr(i + 1), True);
     InitAutoColWidths(Worksheet);
   end;

   LineStr := VarToStr(Line);
   basicType := VarType(Line) and VarTypeMask;
   row := OutputLineLevel[Worksheet] - 1;
   col := 0;
   sheet := FWorkbook.GetWorksheetByIndex(Worksheet);
   case basicType of
     varInteger:
       begin
         if Trim(Format) <> EmptyStr then
         begin
           numFormat := GetNumberFormat(Format);
           sheet.WriteNumber(row, col, StrToInt(LineStr), numFormat, 0);
           TempColWidths[col + 1] := Max(TempColWidths[col + 1], Length(LineStr));
         end
         else
           sheet.WriteNumber(row, col, StrToInt(LineStr));
       end;
     varDouble:
       begin
         if Trim(Format) <> EmptyStr then
         begin
           numFormat := GetNumberFormat(Format);
           sheet.WriteNumber(row, col, StrToFloat(LineStr), numFormat, Length(Format) - 2);
           TempColWidths[col + 1] := Max(TempColWidths[col + 1], Length(Format));
         end
         else
           sheet.WriteNumber(row, col, StrToFloat(LineStr));
       end;
     varOleStr, varString:
       begin
         if Trim(Format) <> EmptyStr then
         begin
           rtp := GetRichTextParams(Format);
           sheet.WriteText(row, col, LineStr, rtp);
         end
         else
           sheet.WriteText(row, col, LineStr);
       end
     else
       begin
         if Trim(Format) <> EmptyStr then
         begin
           rtp := GetRichTextParams(Format);
           sheet.WriteText(row, col, LineStr, rtp);
         end
         else
           sheet.WriteText(row, col, LineStr);
       end;
   end;

   if (Highlight <> clWhite) then
     sheet.WriteBackground(row, col, fsSolidFill, Highlight);
   UpdateColWidths(Worksheet);
   OutputLineLevel[worksheet] := OutputLineLevel[worksheet] + 1;
 end;


 procedure TExcelWrite.ApplyFormat(Left, Top, Right, Bottom: Integer; numFormat: String; worksheet: Integer = 0);
 var
   //FormatIndex: Integer;
   i, j: Integer;
 begin
   if not IsExcelOutput then
     Exit;

     for i:= Top to Bottom do
     begin
       for j:= Left to Right do
       begin
         try
           //FXLSFile.Workbook.Sheets[worksheet].Cells[i, j].FormatStringIndex := FormatIndex;
         except
           {$IFDEF VISUAL_BUILD} // todo -o Glen, refactor so we don't need the ifdef here
           ShowMessage('Unknown error encountered, cell[' + IntToStr(i) + ', ' + IntToStr(j) + ']');
           {$ELSE}
           warn_nv('An unknown error occured while formating the output.  This should not effect the actual results just the way that it appears visually');
           {$ENDIF}
         end;
       end;
     end;
 end;

procedure TExcelWrite.ColWidth(Range: TRect; Width, worksheet: Integer);
  var
    col: Integer;
    sheet: TsWorksheet;
  begin
    if not IsExcelOutput then
      Exit;
    sheet := FWorkbook.GetWorksheetByIndex(worksheet);
    for col := Range.Left to Range.Right do
      sheet.WriteColWidth(col, Width, suChars);
  end;

procedure TExcelWrite.AutoSizeColumns;
var
  i, j: Integer;
  sheet: TsWorksheet;
begin
  for i := 0 to FWorkbook.GetWorksheetCount - 1 do
  begin
    sheet := FWorkbook.GetWorksheetByIndex(i);
    for j := 0 to Length(AutoColWidths[i]) - 1 do
    begin
      if AutoColWidths[i][j] > 0 then
        sheet.WriteColWidth(j, AutoColWidths[i][j], suChars);
    end;
  end;
end;

 function TExcelWrite.getLineLvl(Index: Integer): Integer;
 begin
   result := OutputLineLevel[Index];
 end;

 function TExcelWrite.ByteToRotationType(aByte: Byte): TsTextRotation;
 begin
   case aByte of
     XLROTATIONNONE: Result := trHorizontal;
     XLROTATIONTEXTVERTICALCHARHORIZONTAL: Result := rtStacked;
     XLROTATIONVERTICALUP: Result := rt90DegreeCounterClockwiseRotation;
     XLROTATIONVERTICALDOWN: Result := rt90DegreeClockwiseRotation;
     else
     begin
       Assert(False, 'invalid text rotation requested');
       Result := trHorizontal;
     end;
   end;
 end;

 function TExcelWrite.GetRichTextParams(aFormat: String): TsRichTextParams;
 begin
   SetLength(Result, 1);
   Result[0].FirstIndex := 1;
   Result[0].FontIndex := 0;
 end;

 function TExcelWrite.GetNumberFormat(aFormat: String): TsNumberFormat;
 begin
   Result := nfGeneral;
   if Pos('0.0', aFormat) > 0 then
     Result := nfFixed;
 end;

 function TExcelWrite.GetCellBorder(borderIndex: TCellBorderIndex): TsCellBorders;
 begin
   Result := [];
   case borderIndex of
     xlBorderLeft: Result := [cbWest];
     xlBorderRight: Result := [cbEast];
     xlBorderTop: Result := [cbNorth];
     xlBorderBottom: Result := [cbSouth];
     xlBorderAll: Result := [cbNorth, cbSouth, cbEast, cbWest];
   end;
 end;

procedure TExcelWrite.SetSpreadsheetFormat(AValue: TsSpreadsheetFormat);
begin
  if FSpreadsheetFormat=AValue then Exit;
  FSpreadsheetFormat:=AValue;
end;

procedure TExcelWrite.UpdateOutputFormat(OutFormat: TOutputFileType);
begin
  case OutFormat of
    ExportExelXML: FSpreadsheetFormat := sfOOXML;
    ExportExcel: FSpreadsheetFormat := sfExcel8;
    ExportODS: FSpreadsheetFormat := sfOpenDocument;
    ExportCSV: FSpreadsheetFormat := sfCSV;
    ExportText, ExportMega, ExportFasta, ExportNone, ExportInvalid:
      begin
        raise Exception.Create('invalid spreadsheet format');
      end;
  end;
end;

procedure TExcelWrite.InitPrecision;
var
  i: Integer;
begin
  for i := 1 to Length(PrecisionFormat) do
    PrecisionFormat[i] := -1;
end;

procedure TExcelWrite.InitAutoColWidths(sheet: Integer);
var
  i, j: Integer;
begin
  if Length(AutoColWidths) <= sheet then
  begin
    i := Length(AutoColWidths);
    setLength(AutoColWidths, sheet + 1);
    while i < Length(AutoColWidths) do
    begin
      SetLength(AutoColWidths[i], 16384);
      for j := 0 to Length(AutoColWidths[i]) - 1 do
        AutoColWidths[i][j] := -1;
      inc(i);
    end;
  end;
end;

procedure TExcelWrite.UpdateColWidths(sheet: Integer);
var
  i: Integer;
begin
  for i := 1 to Length(TempColWidths) do
    if TempColWidths[i] > 0 then
      AutoColWidths[sheet][i - 1] := Max(2, TempColWidths[i]);
end;

function TExcelWrite.IsNumericType(BasicType: Integer): Boolean;
begin
  Result := False;
  case BasicType of
    varsmallint, varinteger, varsingle, vardouble, varqword,
    vardecimal, varshortint, varbyte, varword, varlongword, varint64,
    vardate, varcurrency: Result := True;
  end;
end;

function TExcelWrite.IsFloatingPointType(BasicType: Integer): Boolean;
begin
  Result := False;
  case BasicType of
    varsingle, vardouble, vardecimal, vardate, varcurrency: Result := True;
  end;
end;

function TExcelWrite.IsLargeFile: Boolean;
begin
  Result := (FWorkbook.ActiveWorksheet.Cols.Count > 100) or (FWorkbook.ActiveWorksheet.Rows.Count > 1000);
end;

function TExcelWrite.ChangeFileExtForOutputFileType(filename: String; aType: TOutputFileType): String;
var
  aExt: String;
begin
  aExt := FileExtensionForOutputFileType(aType);
  Result := ChangeFileExt(filename, aExt);
end;

 function TExcelWrite.PleaseWaitDlg(action: String ):TPleaseWait;
 begin
   if WaitDlg <> nil then
     WaitDlg.Free;
   WaitDlg := TPleaseWait.Create(nil);
   WaitDlg.Action := action;
   WaitDlg.Show;
   Result := WaitDlg;
 end;

procedure TExcelWrite.MergeCells(aRect: TRect; HAlign: TAlign; VAlign: TAlign);
var
  sheet: TsWorksheet;
begin
  if not IsExcelOutput then Exit;
  sheet := FWorkbook.GetWorksheetByIndex(0);
  sheet.MergeCells(aRect.Top, aRect.Left, aRect.Bottom, aRect.Right);
  case HAlign of
    aCenter: sheet.WriteHorAlignment(aRect.Top, aRect.Left, haCenter);
    aLeft: sheet.WriteHorAlignment(aRect.Top, aRect.Left, haLeft);
    aRight: sheet.WriteHorAlignment(aRect.Top, aRect.Left, haRight);
    aNone: sheet.WriteHorAlignment(aRect.Top, aRect.Left, haDefault);
  end;
  case VAlign of
    aTop: sheet.WriteVertAlignment(aRect.Top, aRect.Left, vaTop);
    aCenter: sheet.WriteVertAlignment(aRect.Top, aRect.Left, vaCenter);
    aBottom: sheet.WriteVertAlignment(aRect.Top, aRect.Left, vaBottom);
    aNone: sheet.WriteVertAlignment(aRect.Top, aRect.Left, vaDefault);
  end;
end;

function OutputFileTypeForFileExt(FileExt: String): TOutputFileType;
begin
  if SameText(FileExt, '.xlsx') then
    Result := ExportExelXML
  else if SameText(FileExt, '.xls') then
    Result := ExportExcel
  else if SameText(FileExt, 'ods') then
    Result := ExportODS
  else if SameText(FileExt, '.csv') then
    Result := ExportCSV
  else if SameText(FileExt, '.txt') or SameText(FileExt, '.text') then
    Result := ExportText
  else if SameText(FileExt, 'meg') then
    Result := ExportMega
  else if SameText(FileExt, 'fas') or SameText(FileExt, 'fasta') or SameText(FileExt, 'fst') or SameText(FileExt, 'fa') then
    Result := ExportFasta
  else
    Result := ExportInvalid;
end;

procedure RunAProgram(const theProgram: String);
begin
  try
    OpenDocument(theProgram);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when trying to open a document: ' + E.Message);
  end;
end;

procedure TExcelWrite.ColorCells(aRect: TRect; Color: TColor; BorderType : TCellBorderIndex = xlBorderAll; Sheet : Integer = 0);
var
  Col, Row: integer;
  aSheet: TsWorksheet;
  cellBorders: TsCellBorders;
  border: TsCellBorder;
begin
   if not IsExcelOutput then
    Exit;
   aSheet := FWorkbook.GetWorksheetByIndex(sheet);
   cellBorders := GetCellBorder(BorderType);
   for Col := aRect.Left to aRect.Right do
     for Row := aRect.Top to aRect.Bottom do
     begin
       aSheet.WriteBorders(Row, Col, cellBorders);
       for border in cellBorders do
         aSheet.WriteBorderStyle(Row, Col, border, lsMedium, Color);
     end;
end;

function TExcelWrite.AddBlankCell(highlight: TColor = clWhite): Boolean;
begin
  Result := True;
  if (not IsExcelOutput) then
    Exit;
  if OutputLinePos > MAX_COLS then
  begin
    Result := False;
    Exit;
  end;
  OutputLine[OutputLinePos] := EmptyStr;
  HighlightLine[OutputLinePos] := highlight;
  Inc(OutputLinePos);
end;

function TExcelWrite.AddBlankCells(NumBlankCells: Integer; highlight: TColor = clWhite): Boolean;
var
  i: Integer;
begin
  Assert(NumBlankCells >= 0);
  Result := True;
  if NumBlankCells > 0 then
    for i := 0 to NumBlankCells - 1 do
      Result := Result and AddBlankCell;
end;

function TExcelWrite.LastCellWriteXY(worksheet:integer=0): TRect;
begin
  Result.Left :=  self.OutputLinePos-1;   //Holds next column
  Result.Top :=  OutputLineLevel[worksheet]-1; //Holds next row
  if Result.Left = 0 then
    Result.Top := Result.Top-1;
  Result.Bottom := Result.Top;
  Result.Right := Result.Left;
end;

{ TExcelWriteThread }

constructor TExcelWriteThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;


procedure TExcelWriteThread.Execute;
var
  Status: String;
begin

  try
    try
      //if Assigned(OnProgress) then
      //  Writer.OnProgress := OnProgress;
      Writer.SaveFileNoAutoFit(FileName);
      Status := 'Success';
    except
      on E: Exception do
      begin
        Status := 'Excel writer encountered an error: ' + E.Message;
      end;
    end;
  finally
    if Assigned(Writer) then
      Writer.Free;
    CallBack(Self, Status);
  end;
end;

end.
