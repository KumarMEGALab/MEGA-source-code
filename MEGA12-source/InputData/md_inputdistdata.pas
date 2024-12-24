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

unit MD_InputDistData;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

Uses
  MLongintList, MOtuInfo, MegaConsts, MegaVerConsts, Classes, MDistPack,
  MegaErrUtils, ErrorMessages_HC, SysUtils, MegaUtils, MPleaseWait, ExcelWrite,
  Math, Graphics;




Type
TResultsWindowSetupOptions = class
  public
  MyNoOfGps : Integer;
  GpNames : TStringList;
  DistanceMatrix : PDistanceMatrix;
  Caption : AnsiString;
  DistArray : ArrayOfDouble;
  Constructor create;
  Destructor Destroy; override;
end;

Type

{ TDistExportOptions }

TDistExportOptions = class
  Precision : Integer;
  OutputFormat : TOutputFileType; // (ExportExcel, ExportCSV, ExportText, ExportMega, ExportFasta, ExportNone, ExportInvalid);
  WithStdErr : Boolean;
  OutputAsCols : Boolean;
  LowerLeft : Boolean;
  OppositeSides : Boolean;
  FEntriesPerLine : Integer;
  PrevMatrixFormat : Integer;
  FIsColumnar : Boolean;
  AllowNegative : Boolean;
  SaveExcelFileTo : String;
  FTitle : String;
  FDAcronym : String;
  FVAcronym  : String;
  AllowStandardError : Boolean;
  NoOfRows : Integer;
  constructor create;
  function IsSpreadsheetFormat: Boolean;
end;

Type

{ TD_InputDistdata }

TD_InputDistdata = class
  private
    function GetMaxRowNameLen(RowNames: TStringList): LongInt;
    {$IFNDEF VISUAL_BUILD}
    procedure WriteOutDistanceMatrix(DistInfo: TResultsWindowSetupOptions);
    {$ENDIF}

  Public
    function WriteColumnarData(ExportOptions: TDistExportOptions; FDistMat : PDistanceMatrix; RowNames : TStringList): TStringList;
    procedure PrepareDataForAnalysis(ADistMat: PDistanceMatrix; MyUsedOtuInfos: TList; NTaxa: Integer);
    function WritePairwiseData(ExportOptions: TDistExportOptions; FDistMat: PDistanceMatrix; RowNames : TStringList): TStringList;
    function CellContent(ARow, ACol: Integer; var InvalidNum: Boolean): Double;

    function GetAvgGps(MyComputeType: TDistType): TResultsWindowSetupOptions;
    function GetAvgAll: Double;

    procedure ComputeMaximumDistance;
    function GetDistance(i, j: Integer): Double;
    function GetGpName(Index: Integer): AnsiString;
    function GetTaxaName(Index: LongInt): AnsiString;
    function GetTaxaUsed(Index: LongInt): Boolean;
    procedure Initialize;
    procedure UpdateDispTaxaList;
    function GetColName(Index: LongInt):AnsiString;  // just based on taxon name

    function WriteExportTofile(ExportOptions : TDistExportOptions):TStringList;
    procedure SaveSession(Filename : String);
 public
  DispTaxaList:  TLongintList;   // this is for display

  FNoOfTaxa:     LongInt;      // No. of seqs
  FOtuInfos:     TAllOtuInfo;  // "Changes are global"

  MaxD: Double;                  // largest distance of any taxa currently in use
  function FindTaxon(searchQuery: String; startIndex: Integer; searchForward: Boolean): Integer;
  property TaxaName[Index: LongInt]:AnsiString  read GetTaxaName;
  property TaxaUsed[Index: LongInt]:Boolean read GetTaxaUsed;
  property GpName[Index: LongInt]:   AnsiString  read GetGpName;
  property Distance[i,j: LongInt]:Double read GetDistance; // i and j are FOtuInfos indices
  property NoOfTaxa: LongInt      read FNoOfTaxa write FNoOfTaxa;
  property OtuInfos: TAllOtuInfo  read FOtuInfos write FOtuInfos;   // "Changes are global"
  property ColName[input : Integer]: AnsiString read GetColName;

  constructor create;
  destructor Destroy; override;
end;

var
  D_InputDistData : TD_InputDistData;

implementation

uses
  MVS_DistDataExplorer,{$IFDEF VISUAL_BUILD}Mega_Main{$ELSE}MD_MegaMain, MegaUtils_NV{$ENDIF};

{ TDistExportOptions }

constructor TDistExportOptions.create;
begin
  Precision := 4;
  OutputFormat :=  ExportText; // (ExportExcel, ExportCSV, ExportText, ExportMega, ExportFasta, ExportNone, ExportInvalid);
  WithStdErr := False;
  OutputAsCols := False;
  LowerLeft := True;
  OppositeSides := True;
  //FEntriesPerLine := 0;
  PrevMatrixFormat := 0;
  FIsColumnar := False;
  AllowNegative := True;
  SaveExcelFileTo := EmptyStr;
  FTitle := EmptyStr;
  FDAcronym := EmptyStr;
  FVAcronym  := EmptyStr;
  AllowStandardError := False;
  NoOfRows := 0;
end;

function TDistExportOptions.IsSpreadsheetFormat: Boolean;
begin
  Result := ((OutputFormat = ExportExelXML) or
             (OutputFormat = ExportExcel) or
             (OutputFormat = ExportODS) or
             (OutputFormat = ExportCSV));
end;

constructor TD_InputDistdata.create;
begin
  FNoOfTaxa  := -1;
  FOtuInfos  := nil;
  DispTaxaList     := nil;

end;

procedure TD_InputDistdata.ComputeMaximumDistance;
var
  j, i : integer;
  d : double;
begin
    maxD := 0;
    for i:= 1 to FNoOfTaxa-1 do
      for j:=0 to i-1 do
      begin
        d := Distance[i,j];
        if d < 0 then
          Continue;
        if abs(d) > maxD then
          maxD := abs(d);
      end;
end;

function TD_InputDistdata.GetAvgAll: Double;
var
  TheSum, TheAvg, TheDist: Double;
  Comps, i, j: Integer;
begin
  TheSum := 0;
  Comps := 0;
  // Only called for Taxon-pairwise comparisons
  for i:=1 to FNoOfTaxa-1 do
  begin
    if not FOtuInfos[i].IsUsed then
      Continue;
    for j:= 0 to i-1 do
    begin
      if not FOtuInfos[j].IsUsed then
        Continue;
      TheDist := Distance[i,j];
      if TheDist >= 0 then
      begin
        TheSum := TheSum + (TheDist/MaxD);
        Inc(Comps);
      end;
    end;
  end;
  TheAvg := (TheSum/Comps)*MaxD;
  Result := TheAvg;
end;

procedure TD_InputDistdata.Initialize;
begin
 DispTaxaList := TLongIntList.Create;
 UpdateDispTaxaList;
 ComputeMaximumDistance;
end;

destructor TD_InputDistdata.Destroy;
begin
inherited;
  FreeAndNil(DispTaxaList);

 { if FOtuInfos <> nil then
  begin
    // clean up taxa specific data  // !!BOOKMARK
    for i:=0 to FNoOfTaxa-1 do
    begin
      if FOtuInfos[i].Data <> nil then
      begin
        if FOtuInfos[i].Data <> nil then
          FreeMem(FOtuInfos[i].Data);
        FOtuInfos[i].Data := nil;
      end;
    end;
    FreeAndNil(FOtuInfos);
  end;  }
end;

function TD_InputDistdata.GetMaxRowNameLen(RowNames: TStringList): LongInt;
var
  i: Integer;
begin
  Result := -1;
  for i:=0 to RowNames.Count-1 do
    if Length(RowNames.Strings[i]) > Result then
      Result := Length(RowNames.Strings[i]);
end;

function TD_InputDistdata.WritePairwiseData(ExportOptions: TDistExportOptions;
  FDistMat: PDistanceMatrix; RowNames: TStringList): TStringList;
var
  OutList: TStringList = nil;
  AddToDispStr,DispStr: String;
  xD, X, Y, xId: Integer;
  i, j, h, g, MatFrags, fromX, toX: Integer;
  EntryLen : Integer; // str length of the dist or std err value
  EntrySize: Integer; // total str length needed for entrylen + space for '[]'
  EntryPerLine: Integer;
  aMaxRowNameLen: Integer;
  MatDside: Integer = 0;
  MatSEside: Integer = 0;
  MaxD: Double;
  D: PDistanceMatrix;
  TheD, TheS, TheX, TempDistance: Double;
  InvalidNum: Boolean;
  Wait: TPleaseWait;
  FStdErrMat : PDistanceMatrix = nil;
  DistXls : TExcelWrite = nil;

  function GetValueToWrite(ARow, ACol: integer; IsDist: Boolean): Double;
  begin
    Result := -1.0;
    InvalidNum := False;
    if ARow = ACol then Exit;

    if IsDist = True then
    begin
      if ARow > ACol then  Result := FDistMat[ARow,ACol]
      else                 Result := FDistMat[ACol,ARow];
    end
    else
    begin
      if ARow > ACol then  Result := FStdErrMat[ARow,ACol]
      else                 Result := FStdErrMat[ACol,ARow];
    end;
    InvalidNum := (Result < 0) and (not ExportOptions.AllowNegative);
  end;
begin

  // first initialize temporary variables
  D := FDistMat;
  if D = nil then  RaiseErrorMessage(HC_Unexpected_Error, 'No Distance Data Given, unable to export'); //We are exporting the distance matrix, if we don't have it then exit
  if RowNames = nil then RaiseErrorMessage(HC_Unexpected_Error, 'Names not provided, unable to export'); //We need to write what names are used to know what the distances are assocated with else the data is useless
  aMaxRowNameLen := GetMaxRowNameLen(RowNames);
  Result := TStringList.Create; //The TStringList we return if output is not excel containing the results to display

  Wait := TPleaseWait.Create(nil);  //Dialog informing user of what we're doing
  Wait.Action := 'Exporting and Formatting to Excel';
  Wait.PercentDone := 0;
  Wait.Show;

  If ExportOptions.IsSpreadsheetFormat then //If we're not using excel theres no need to create it and slow down the process
  begin
    DistXls := TExcelWrite.Create(nil, 'Distance Data');
    DistXls.IsXLS := True; //Let ExcelWrite know that we are exporting to Excel
  end;

  //The length of the Otu #Id everyTime
  xId := Trunc(log10(VS_DistDataExplorer.NoOfSelTaxa));
  if xId < 0 then xId := 0;
  Inc(xId);

  // setup side
  If ExportOptions.LowerLeft  then
    MatDSide := 1
  else
    MatDSide := 2;

  EntryPerLine := ExportOptions.FEntriesPerLine;

  try try
    OutList := TStringList.Create;
    if ExportOptions.OutputFormat = ExportMega then
    begin
      OutList.Add('#mega');
      OutList.Add('!Title: '+ExportOptions.FTitle+';');
      // make format string
      DispStr := '!Format'+' DataType=Distance';
      case MatDside of
        1:  DispStr := DispStr + ' DataFormat=LowerLeft';
        2:  DispStr := DispStr + ' DataFormat=UpperRight';
      // 3:  DispStr := DispStr + 'DataFormat=Column ';
      end;
      DispStr := DispStr+' NTaxa='+IntToStr(RowNames.Count);
      DispStr := DispStr + ';';
      OutList.Add(DispStr);

    end
    else if ExportOptions.OutputFormat = ExportText then
      if ExportOptions.FTitle <> '' then   //if theres no title theres no reason to say 'title:'
        OutList.Add('Title: '+ExportOptions.FTitle)
    else if ExportOptions.OutputFormat = ExportCSV then //
    begin
      // this information should be presented at the end of the CSV file
    end
    else if ExportOptions.OutputFormat = ExportExcel then
    begin
      // this information should be sent to another page of the XL file
    end;

    if ExportOptions.OutputAsCols then
    begin

      If ExportOptions.IsSpreadsheetFormat then
      begin
        DistXls.Add('Species 1');
        DistXls.Add('Species 2');
        DistXls.Add('Dist');
        DistXls.WriteLine();

          for h:= 1 to RowNames.Count-1 do
            begin
              for g:=0 to h-1 do
              begin
                DistXls.Add(RowNames.Strings[g]);
                DistXls.Add(RowNames.Strings[h]);
                TempDistance := GetValueToWrite(h, g, true);
                if InvalidNum then
                  DistXls.Add('n/c')
                else
                  DistXls.AddP(TempDistance, ExportOptions.Precision); // Distance
                if MatSESide <> 0 then
                begin
                TempDistance :=  GetValueToWrite(h, g, false);
                  if InvalidNum then
                    DistXls.Add('n/c')
                  else
                    DistXls.AddP(TempDistance, ExportOptions.Precision);  // Standard error
                end;
                DistXls.WriteLine(0, 'A', BuildNumberFormat(ExportOptions.Precision));
                OutList.add(StrToStrWidth(RowNames.Strings[g], aMaxRowNameLen + 1) + StrToStrWidth(RowNames.Strings[h], aMaxRowNameLen + 1) + FloatToStrWidth(GetValueToWrite(h, g, true), ExportOptions.Precision+1+Length(FloatToStr(Floor(GetValueToWrite(h, g, true)))), ExportOptions.Precision));
              end;
              if RowNames.Count = 1 then // Avoids the div by 0 error.
                Wait.PercentDone := 100
              else
                Wait.PercentDone := (h * 100) div RowNames.Count-1;
            end;
      end; //end excel if
    OutList.Add('');
    OutList.Add(StrToStrWidth('Species 1', aMaxRowNameLen+1) + StrToStrWidth('Species 2', aMaxRowNameLen+1) + StrToStrWidth('Distance', aMaxRowNameLen));

    for h:= 1 to RowNames.Count-1 do
    begin
        for g := 0 to h-1 do
         OutList.add(StrToStrWidth(RowNames.Strings[g], aMaxRowNameLen + 1) + StrToStrWidth(RowNames.Strings[h], aMaxRowNameLen + 1) + FloatToStrWidth(GetValueToWrite(h, g, true), ExportOptions.Precision+1+Length(FloatToStr(Floor(GetValueToWrite(h, g, true)))), ExportOptions.Precision));
    end;
    end
    else
    begin
      OutList.Add(EmptyStr);

    // writing taxa names on top
    for i:=0 to RowNames.Count-1 do
    begin
      DispStr := '['+IntToStrWidth(i+1, xId)+'] '+'#'+OtuNameToMegaStr(RowNames.Strings[i]);
      OutList.Add(DispStr);
    end;
    OutList.Add(EmptyStr);

    // find maxD and maxS;
    // Fix them to use the actual display value
    MaxD:=0;
{    if D <> nil then
    begin   }
      for i:=0 to RowNames.Count-1 do
        for j:=0 to RowNames.Count-1 do
        begin
          TheX := GetValueToWrite(i,j,True);
          if (not InvalidNum) and (MaxD < abs(TheX)) then
            MaxD := abs(TheX);
        end;

    // all distances are written in x.y format
    //where, x = noofchar before decimal, y is after decimal
    xD := 0;
  //  if D <> nil then
   // begin
      if MaxD > 0.0 then xD := Trunc(log10(MaxD));
      if xD < 0 then  xD := 0;
      Inc(xD);   // atleast one before decimal

    X := Xd;
    if ExportOptions.AllowNegative then
      Inc(X);

    if ExportOptions.Precision = 0 then
    begin
      Y := 0;  EntryLen := X;  // x
    end
    else
    begin
      Y := ExportOptions.Precision;
      EntryLen := X+Y+1;  // x.y
    end;


      EntrySize := EntryLen;                         // no baggage

    // now it will start
    fromX    := 0;
    toX      := 0;

    MatFrags := 0;
    while True do
    begin
      if toX = RowNames.Count-1 then
        Break;
      fromX := MatFrags*EntryPerLine;
      toX   := (MatFrags+1)*EntryPerLine -1;
      Inc(MatFrags);

      if toX >= RowNames.count then
        toX := RowNames.count-1;
      if toX <= fromX then
        toX := RowNames.Count-1;

      // we write the top row of the matrix
      DispStr := '['+BlankString(xId+1)+' ';     // write the top row of Matrix  (eg. [        1     2     3     4     5     6     7     8     9    10    11    12    13 ]  )
      for i:= fromX to toX do
        DispStr := DispStr + IntToStrWidth(i+1,EntrySize)+' '; // it should be centered, ideally
      DispStr := DispStr + ']';
      OutList.Add(DispStr);

      // now write matrix
      for i:= 0 to RowNames.count-1 do
      begin
        // first find if not both sides then whether to skip or not
        if (MatDSide = MatSESide) or (MatSESide < 1) then
        begin
          if (i < fromX) and (MatDSide = 1) then  // lowerleft
            Continue
          else if (i > toX) and (MatDSide = 2) then  // Upperright
            Continue;
        end;

        DispStr := '['+IntToStrWidth(i+1, xId)+']  ';
        If ExportOptions.IsSpreadsheetFormat then
          DistXls.Add(OtuNameToMegaStr(RowNames.Strings[i]));
          
        for j:=fromX to toX do
        begin
          if (MatDSide > 0) and (MatSESide > 0) then // two matrices
          begin
            if (MatDSide = MatSESide) then
            begin
              if i = j then
              begin
                DispStr := DispStr + BlankString(EntrySize)+' ';
                If ExportOptions.IsSpreadsheetFormat then
                  DistXls.AddBlankCell;
              end
              else if (MatDSide = 1) and (i<j) then
                Break
              else if (MatDSide = 2) and (i>j) then
              begin
                DispStr := DispStr + BlankString(EntrySize)+ ' ';  // to fill the gap
                If ExportOptions.IsSpreadsheetFormat then
                  DistXls.AddBlankCell;
              end
              else
              begin
                TheD := GetValueToWrite(i, j, True);
                TheS := GetValueToWrite(i, j, False);
                AddToDispStr := EmptyStr;
                if InvalidNum then
                begin
                  AddToDispStr := AddToDispStr + StrToStrWidth('?',EntryLen);
                  If ExportOptions.IsSpreadsheetFormat then
                    DistXls.Add('?');
                end
                else
                begin
                  AddToDispStr := AddToDispStr + FloatToStrWidth(TheD, EntryLen, Y); // Y = FPrecision
                  If ExportOptions.IsSpreadsheetFormat then
                    DistXls.AddP(TheD, Y);
                end;


                AddToDispStr := AddToDispStr + ' [';
                if InvalidNum then
                begin
                  AddToDispStr := AddToDispStr + StrToStrWidth('?',EntryLen);
                  If ExportOptions.IsSpreadsheetFormat then
                    DistXls.Add('?');
                end
                else
                begin
                  AddToDispStr := AddToDispStr + FloatToStrWidth(TheS, EntryLen, Y);
                  If ExportOptions.IsSpreadsheetFormat then
                    DistXls.AddP(TheS, 1, clWhite, clBlue);
                end;


                AddToDispStr := AddToDispStr + '] ';
                DispStr := DispStr + AddToDispStr;
              end;
            end
            else  // MatDSide <> MatSESide
            begin
              if i = j then
              begin
                DispStr := DispStr + BlankString(EntrySize)+' ';
                If ExportOptions.IsSpreadsheetFormat then
                  DistXls.Add(EmptyStr);
              end
              else
              begin
                if (MatDSide =1) and (MatSESide = 2) then
                begin
                  if i > j then TheX := GetValueToWrite(i,j,True)
                           else TheX := GetValueToWrite(i,j,False);
                  if InvalidNum then
                  begin
                    AddToDispStr := StrToStrWidth('?',EntryLen) + ' ';
                    If ExportOptions.IsSpreadsheetFormat then
                      DistXls.Add('?');
                  end
                  else
                  begin
                    AddToDispStr := FloatToStrWidth(TheX,EntryLen,Y) + ' ';
                  end;
                  if i < j then
                  begin
                    AddToDispStr := '['+AddToDispStr+']';
                    If ExportOptions.IsSpreadsheetFormat then
                      DistXls.AddP(TheX, Y, clWhite, clBlue);

                  end
                  else
                  begin
                    AddToDispStr := ' '+AddToDispStr+' ';
                    If ExportOptions.IsSpreadsheetFormat then
                      DistXls.AddP(TheX, Y);

                  end;
                end
                else if (MatDSide =2) and (MatSESide = 1) then
                begin
                  if i > j then TheX := GetValueToWrite(i,j,False)
                           else TheX := GetValueToWrite(i,j,True);
                  if InvalidNum then
                  begin
                    AddToDispStr := StrToStrWidth('?',EntryLen) + ' ';
                    If ExportOptions.IsSpreadsheetFormat then
                      DistXls.Add('?');
                  end
                  else
                  begin
                    AddToDispStr := FloatToStrWidth(TheX,EntryLen,Y) + ' ';
                  end;
                  //if i < j then

                  if i > j then
                  begin
                    AddToDispStr := '['+AddToDispStr+']';
                    If ExportOptions.IsSpreadsheetFormat then
                      DistXls.AddP(TheX, Y, clWhite, clBlue);

                  end
                  else
                  begin
                    AddToDispStr := ' '+AddToDispStr+' ';
                    If ExportOptions.IsSpreadsheetFormat then
                      DistXls.AddP(TheX, Y);

                  end;
                end;
                DispStr := DispStr + AddToDispStr;
              end;
            end;
          end
          else // only one matrix to deal with; it has to be DistMat
          begin
            if i = j then
            begin
              DispStr := DispStr + BlankString(EntrySize)+ ' ';
              If ExportOptions.IsSpreadsheetFormat then
                DistXls.Add(EmptyStr);  // Nothing is ever on a Diagional, so just add a blank cell.
              Continue;
            end
            else if (MatDSide = 1) and (i<j) then
              Break
            else if (MatDSide = 2) and (i>j) then
            begin
              DispStr := DispStr + BlankString(EntrySize)+' ';  // to fill the gap
              If ExportOptions.IsSpreadsheetFormat then
                DistXls.Add(EmptyStr);
            end
            else
            begin
              TheX := GetValueToWrite(i,j,True);

              if InvalidNum then
              begin
                AddToDispStr := StrToStrWidth('?',EntryLen) + ' ';
                If ExportOptions.IsSpreadsheetFormat then
                  DistXls.Add('?');
              end
              else
              begin
                AddToDispStr := FloatToStrWidth(TheX, EntryLen, Y)+' ';
                If ExportOptions.IsSpreadsheetFormat then
                  DistXls.AddP(TheX, Y);
              end;
              DispStr := DispStr + AddToDispStr;
            end;
          end;  // if only one matrix
        end; // all entries in a row
        OutList.Add(DispStr);
        If ExportOptions.IsSpreadsheetFormat then
          DistXls.WriteLine(0, 'A', BuildNumberFormat(Y));
      end;  // end of all rows in a block
      OutList.Add(EmptyStr);
      If ExportOptions.IsSpreadsheetFormat then
        DistXls.WriteLine();
    end; // end for all Blocks
   end; // End of "Square" export type
   Wait.PercentDone := 100;
   if (ExportOptions.OutputFormat <> ExportExcel) and
      (ExportOptions.OutputFormat <> ExportExelXML) and
      (ExportOptions.OutputFormat <> ExportODS) and
      (ExportOptions.OutputFormat <> ExportCSV) then
   begin
     {$IFDEF VISUAL_BUILD}
     Result.Text := OutList.Text;
     {$ELSE}
     OutList.SaveToFile(ExportOptions.SaveExcelFileTo);
     {$ENDIF}
   end
   else if ExportOptions.OutputFormat = ExportCSV then
   begin
     {$IFDEF VISUAL_BUILD}
     Result.Text := DistXls.GetCsvText;
     {$ELSE}
     DistXLS.SaveFile(ExportOptions.SaveExcelFileTo, ExportCSV);
     {$ENDIF}
     DistXls.Free;
   end;
   OutList := nil;
   If (ExportOptions.OutputFormat = ExportExcel) or
      (ExportOptions.OutputFormat = ExportExelXML) or
      (ExportOptions.OutputFormat = ExportODS)  then
   begin
     DistXls.SaveFile(ExportOptions.SaveExcelFileTo, ExportOptions.OutputFormat);
     DistXls.Free;
   end;
   FreeAndNil(Wait);
  Except
    On E: Exception do
    begin
      If ExportOptions.IsSpreadsheetFormat then
      begin
        DistXls.HadError;
        DistXls.Free;
      end;
      E.HelpContext := HC_Unexpected_Error;
      ShowErrorMessage(E);
      if Wait <> nil then FreeAndNil(Wait);
    end;
  end;
  finally
    if OutList <> nil then OutList.Free;
  end;
end;


procedure TD_InputDistdata.UpdateDispTaxaList;
var
  i: LongInt;
begin
  if VS_DistDataExplorer.DispSelTaxaItem then
    VS_DistDataExplorer.NoOfSelTaxa := FOtuInfos.ConstructSelTaxaList(DispTaxaList)
  else
  begin
    DispTaxaList.Clear;
    for i:=0 to FNoOfTaxa - 1 do
       DispTaxaList.Add(i);
  end;
end;

function TD_InputDistdata.GetTaxaName(Index: LongInt): AnsiString;
begin
  Index := DispTaxaList[Index];
  if Index < 0 then  Result := ''
  else               Result := FOtuInfos[Index].Name;
end;

function TD_InputDistdata.GetDistance(i, j: Integer): Double;
var
  xI, xJ: LongInt;
begin
  xI := i; // this is because the i-th entry in FOtuInfos; not ith id
  xJ := FOtuInfos[j].id; // this is because it is the xJ the original entry
  Result := (PArrayOfDouble(FOtuInfos[xI].Data))[xJ];
end;

function TD_InputDistdata.CellContent(ARow, ACol: Integer;
  var InvalidNum: Boolean): Double;
begin
  Result := InvalidDistValue;
  InvalidNum := False;

  if (ARow >= DispTaxaList.Count) or (ACol >= DispTaxaList.Count) then
    Exit;
  if ARow <= ACol then Exit;
  ARow := DispTaxaList[ARow];
  ACol := DispTaxaList[ACol];

  Result :=  Distance[ARow, ACol];
  if Result <= InvalidDistValue then
    InvalidNum := True;
end;


// Subsetting data for analysis
procedure TD_InputDistdata.PrepareDataForAnalysis(ADistMat: PDistanceMatrix;
  MyUsedOtuInfos: TList; NTaxa: Integer);
var
  i, j, xJ: Integer;
  OneArray: PArrayOfDouble;
begin
  NTaxa := VS_DistDataExplorer.NoOfSelTaxa;
  // Now generate the matrices to send
  if ADistMat = nil then
    RaiseErrorMessage(HC_Unexpected_Error, 'Caller function did not send enough memory.');
  for i:=1 to NTaxa-1 do
    for j:= 0 to i-1 do
    begin
      OneArray := TOtuInfo(MyUsedOtuInfos[i]).Data; // all pairwise distance from i
      xJ       := TOtuInfo(MyUsedOtuInfos[j]).id;
      ADistMat[i,j] := OneArray[xJ];  // gets the correct jth entry (maps)
    end;
end;


function TD_InputDistdata.GetColName(Index: LongInt): AnsiString;
begin
  Result := IntToStr(Index+1);
end;

function TD_InputDistdata.GetAvgGps(MyComputeType: TDistType
  ): TResultsWindowSetupOptions;
var
  MyDComps: PDistanceMatrix;
  MyUsedOtuInfos : TList;
  MyMeanGpDComps : ArrayOfDouble; // within gp means
  d: Double;
  i, j, NextI, NextJ, GpI, GpJ, Temp: Integer;
  MyNoOfGps, MyNoOfTaxa: Integer;
  MyGpIds:      array of Integer;
begin
  MyUsedOtuInfos:= TList.Create;  //Create the list to pass and use in the data section
  try
    CurrentAllOtuInfos.UsedOtuInfos(MyUsedOtuInfos, True); // to enforce gp membership, fills out MyUsedOtuInfos

    Result := TResultsWindowSetupOptions.Create;  //Create the object that will hold the information to send back to visual

    Result.DistanceMatrix      := nil;
    MyDComps := nil;
    Result.DistArray      := nil;
    MyMeanGpDComps := nil;



    // does not do double matrix; only for the viewed stuff


      MyGpIds   := nil;

      // now we get the number of Otus used
      MyNoOfTaxa := MyUsedOtuInfos.Count;
      MyNoOfGps  := CountNoOfGpsInOtuInfos(MyUsedOtuInfos);

      // check if we have enough groups/Taxa
      case MyComputeType of
        gdWithinGroupMean:
          if MyNoOfGps < 1 then
            RaiseErrorMessage(HC_Not_Enough_Groups_Selected, 'At least one group must be defined and selected.');
        gdBetweenGroupMean,
        gdNetGroupMean:
          if MyNoOfGps < 2 then
            RaiseErrorMessage(HC_Not_Enough_Groups_Selected, 'At least two groups must be defined and selected.');
      end;

      if MyNoOfTaxa < 2 then
        RaiseErrorMessage(HC_Not_Enough_Taxa_Selected, 'At least two taxa need to be selected.');


    // things are alright
    //try 
      // allocate memories first
      if (MyComputeType = gdBetweenGroupMean) or (MyComputeType = gdNetGroupMean) then
      begin
        Result.DistanceMatrix      := NewDistMatrix(MyNoOfGps, False);
         MyDComps := newDistMatrix(MyNoOfGps, False);
         for i:= 1 to MyNoOfGps-1 do
           for j:=0 to i-1 do
           begin
             Result.DistanceMatrix[i][j]      := 0;
             MyDComps[i][j] := 0;
           end;
      end;

      if (MyComputeType = gdWithinGroupMean) or (MyComputeType = gdNetGroupMean) then
      begin
        SetLength(Result.DistArray, MyNoOfGps);
        SetLength(MyMeanGpDComps, MyNoOfGps);
        for i:=0 to MyNoOfGps-1 do
        begin
          Result.DistArray[i] := 0;
          MyMeanGpDComps[i] := 0;
        end;
      end;

      // setup group Ids for all groups
      SetLength(MyGpIds,MyNoOfTaxa);
      GetGpIdForOtuInfos(MyUsedOtuInfos, MyGpIds);

      // now addup stuff
      for i:= 1 to FNoOfTaxa-1 do
      begin
        if not FOtuInfos[i].IsUsed then
          continue;
        NextI := MyUsedOtuInfos.IndexOf(Pointer(FOtuInfos[i]));
        if NextI < 0 then
          continue;
        for j:= 0 to i-1 do
        begin
          if not FOtuInfos[j].IsUsed then
            Continue;
          NextJ := MyUsedOtuInfos.IndexOf(Pointer(FOtuInfos[j]));
          if NextJ < 0 then
            continue;
          GpI := MyGpIds[NextI];
          GpJ := MyGpIds[NextJ];

          if GpI < GpJ then
          begin
            Temp := GpJ;
            GpJ := GpI;
            GpI := Temp;
          end;

          d := Distance[i,j];
          if d < 0 then
            Continue;

          // make GpI > GpJ
          // update between group stuff
          if (GpI <> GpJ) and ((MyComputeType = gdBetweenGroupMean) or (MyComputeType = gdNetGroupMean)) then
          begin
            Result.DistanceMatrix[GpI][GpJ] := Result.DistanceMatrix[GpI][GpJ] + d;
            MyDComps[GpI][GpJ] := MyDComps[GpI][GpJ] + 1;
          end
          else if (GpI = GpJ) and ((MyComputeType = gdWithinGroupMean) or (MyComputeType = gdNetGroupMean)) then
          begin
            Result.DistArray[GpI] := Result.DistArray[GpI] + d;
            MyMeanGpDComps[GpI] := MyMeanGpDComps[GpI] + 1;
          end;
        end;// end for all i
      end; // end for all j

      // now we have to derive the estimates
      if (MyComputeType = gdBetweenGroupMean) or (MyComputeType = gdNetGroupMean) then
      begin
        for GpI := 1 to MyNoOfGps-1 do
          for GpJ := 0 to GpI-1 do
            if MyDComps[GpI][GpJ] >= 1 then
              Result.DistanceMatrix[GpI][GpJ] := Result.DistanceMatrix[GpI][GpJ]/MyDComps[GpI][GpJ]
            else
              Result.DistanceMatrix[GpI][GpJ] := InvalidDistValue;
      end;
      if (MyComputeType = gdWithinGroupMean) or (MyComputeType = gdNetGroupMean) then
      begin
        for GpI := 0 to MyNoOfGps-1 do
          if MyMeanGpDComps[GpI] >= 1 then
            Result.DistArray[GpI] := Result.DistArray[GpI]/MyMeanGpDComps[GpI]
          else
            Result.DistArray[GpI] := InvalidDistValue;
      end;

      // Now derive estimate for the net group mean
      if (MyComputeType = gdNetGroupMean) then
      begin
        for GpI := 1 to MyNoOfGps-1 do
          for GpJ := 0 to GpI-1 do
            if (MyDComps[GpI][GpJ] >= 0) and
               (Result.DistArray[GpI] > 0) and (Result.DistArray[GpJ] > 0) then
              Result.DistanceMatrix[GpI][GpJ] := Result.DistanceMatrix[GpI][GpJ] - (Result.DistArray[GpI] + Result.DistArray[GpJ])/2
            else
              Result.DistanceMatrix[GpI][GpJ] := InvalidDistValue;
      end;


      Result.MyNoOfGps := MyNoOfGps; //this is the best estimate FNoOfGps;


      Result.GpNames := TStringList.Create;
      for i:=0 to FNoOfTaxa-1 do  //Go through all the taxa
      begin
        if not FOtuInfos[i].IsUsed then
          Continue;
        NextI := MyUsedOtuInfos.IndexOf(FOtuInfos[i]); //Taxa is used so get its index of the used OTUs
        if NextI < 0 then continue; //not valid
        if MyGpIds[NextI] <> Result.GpNames.Count then
          Continue;
        Result.GpNames.Add(FOtuInfos[i].GpName);
      end;

      case MyComputeType of
        gdBetweenGroupMean: Result.Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Avg. distance between groups';
        gdNetGroupMean    : Result.Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Avg. net distance between groups';
        gdWithinGroupMean : Result.Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Avg. distance within groups';
      end;

    {$IFNDEF VISUAL_BUILD}
    WriteOutDistanceMatrix(Result);   //Make this so that when passed a distane object (one already exists but under a different name) it writes it out to the next available file
    {$ENDIF}
    //None of these need to be exported and were only temporary local variables
    if MyDComps <> nil then FreeDistMatrix(MyDComps, MyNoOfGps);
    MyMeanGpDComps := nil; // open array
    MyGpIds := nil;  // deallocates memory properly for dynamic array (MEGA2.1)
  finally
    MyUsedOtuInfos.Free;
  end;
end;

{$IFNDEF VISUAL_BUILD}
procedure TD_InputDistData.WriteOutDistanceMatrix(DistInfo : TResultsWindowSetupOptions);
var
  ExportAs : TDistExportOptions;
  i : integer;
begin
    ExportAs := TDistExportOptions.Create;
    ExportAs.OutputFormat := D_MegaMain.OutputFormat;
    ExportAs.SaveExcelFileTo := NextAvailableFilenameNV(GetFileOutputExtension);
    ExportAs.LowerLeft := true;
    ExportAs.NoOfRows := NoOfTaxa;
    ExportAs.FTitle := DistInfo.Caption;

    //If we only have a distance array we will need to transform it into a distance matrix to export it
    if DistInfo.DistanceMatrix = nil then
    begin
      DistInfo.DistanceMatrix := NewDistMatrix(DistInfo.MyNoOfGps, false);
      for i := 0 to DistInfo.MyNoOfGps-1 do
        DistInfo.DistanceMatrix[0][i] := DistInfo.DistArray[i];
      WriteColumnarData(ExportAs, DistInfo.DistanceMatrix, DistInfo.GpNames)
    end
    else
      WritePairwiseData(ExportAs, DistInfo.DistanceMatrix, DistInfo.GpNames);
end;
{$ENDIF}

destructor TResultsWindowSetupOptions.Destroy;
begin
    if DistanceMatrix <> nil then FreeDistMatrix(DistanceMatrix, MyNoOfGps);
    DistArray := nil;  // open array
    FreeAndNil(GpNames);
    inherited destroy;
end;

constructor TResultsWindowSetupOptions.Create;
begin
  GpNames := nil;
end;


function TD_InputDistdata.GetTaxaUsed(Index: LongInt): Boolean;
begin
  Result := False;
  if (Index < 0) or (Index >= DispTaxaList.Count) then
    Exit;
  Index := DispTaxaList[Index];
  if Index < 0 then Result := False
  else              Result := FOtuInfos[Index].IsUsed;
end;

function TD_InputDistdata.GetGpName(Index: Integer): AnsiString;
begin
  Index := DispTaxaList[Index];
  if Index < 0 then  Result := ''
  else               Result := FOtuInfos[Index].GpName;
end;



function TD_InputDistdata.WriteExportTofile(ExportOptions: TDistExportOptions): TStringList;
var
  MyD: PDistanceMatrix = nil;
  CurEntries, i, j, NextI, NextJ: Integer;
  AStr : String;
  RowNames : TStringList = nil;
begin
  RowNames := TStringList.Create;
  try

    //Adds all the used taxa and assocated Group to the dialog's RowName property
    NextI := 0;
    for i:= 0 to FNoOfTaxa-1 do
    begin
      if not FOtuInfos[i].IsUsed  then
        Continue;
      AStr := FOtuInfos[i].Name;
      if FOtuInfos[i].GeographicalInfo.LabelNeedsCommandString then
        aStr := AStr + FOtuInfos[i].GeographicalInfo.MegFileCommandString;
      RowNames.Add(AStr);
      Inc(NextI);
    end;

    // Now generate the distance matrix to send and MyD distance matrix
    MyD  := NewDistMatrix(VS_DistDataExplorer.NoOfSelTaxa, False); // it can also be used as array
    NextI := 1;
    for i:=1 to FNoOfTaxa-1 do
    begin
      if not FOtuInfos[i].IsUsed then
        Continue;
      NextJ := 0;
      CurEntries := 0;
      for j:= 0 to i-1 do
      begin
        if not FOtuInfos[j].IsUsed then
          Continue;
        MyD[NextI,NextJ] := Distance[i, j];
        Inc(NextJ);
        Inc(CurEntries);
      end;
      if CurEntries > 0 then  // to ensure that each NextI increment is proper
        Inc(NextI);
    end;

    if ExportOptions.FIsColumnar then
      Result := WriteColumnarData(ExportOptions, MyD, RowNames)
    else
      Result := WritePairwiseData(ExportOptions, MyD, RowNames);
  finally
    if Assigned(RowNames) then
      RowNames.Free;
    if MyD <> nil then
      FreeDistMatrix(MyD, VS_DistDataExplorer.NoOfSelTaxa);
  end;
end;




function TD_InputDistdata.WriteColumnarData(ExportOptions: TDistExportOptions;
  FDistMat: PDistanceMatrix; RowNames: TStringList): TStringList;
var
  OutList   : TStringList = nil;
  DispStr: String;
  xD, xS, X, Y: Integer;
  i, EntryLen: Integer;
  MaxD: Double;
  D: PDistanceMatrix = nil;
  S: PDistanceMatrix = nil;
  TheD, TheS, TheX: Double;
  InvalidNum: Boolean;
  MaxRowNameLen: LongInt;
  Wait: TPleaseWait = nil;
  DistXls: TExcelWrite = nil;

  function  GetMaxRowNameLen: LongInt;
  var
    j: Integer;
  begin
    Result := -1;
    for j:=0 to RowNames.Count-1 do
      if Length(RowNames[j]) > Result then
        Result := Length(RowNames[j]);
  end;


  function GetValueToWrite(ARow: Integer; IsDist: Boolean): Double;
  begin
    InvalidNum := False;
    Result := FDistMat[ARow,0];
    InvalidNum := (Result < 0) and (not ExportOptions.AllowNegative);
  end;
  
begin
  try
    try
      Result := TStringList.Create;
      Wait := TPleaseWait.Create(nil);
      Wait.Action := 'Exporting Distance Data';
      Wait.PercentDone := 0;
      Wait.Show;

      If ExportOptions.IsSpreadsheetFormat then //If we're not using excel theres no need to create it and slow down the process
      begin
        DistXls := TExcelWrite.Create(nil, 'Distance Data');
        DistXls.IsXLS := True; //Let ExcelWrite know that we are exporting to Excel
      end;
      D := FDistMat;

      // Fix them to use the actual display value
      // get maximum d value
      MaxD:=0;
      for i:=0 to RowNames.Count - 1 do
      begin
        TheX := GetValueToWrite(i,True);
        if (not InvalidNum) and (MaxD < abs(TheX)) then
          MaxD := abs(TheX);
      end;
      Wait.PercentDone := 9;
      OutList := TStringList.Create;
      OutList.Add('Title: ' + ExportOptions.FTitle);
      OutList.Add(EmptyStr);
      Wait.PercentDone := 20;

      // all distances are written in x.y format
      //where, x = noofchar before decimal, y is after decimal
      xD := 0;
      if D <> nil then
      begin
        if MaxD > 0.0 then xD := Trunc(log10(MaxD));
        if xD < 0 then  xD := 0;
        Inc(xD);   // atleast one before decimal
      end;

      xS := 0;
      if xD>xS then
        X := xD
      else
        X := xS;

      if ExportOptions.AllowNegative then
        Inc(X);

      if ExportOptions.Precision = 0 then
      begin
        Y := 0;
        EntryLen := X;  // x
      end
      else
      begin
        Y := ExportOptions.Precision;
        EntryLen := X+Y+1;  // x.y
      end;
      Wait.PercentDone := 20;
      // we must compute the maximum RowNameLen;
      MaxRowNameLen := GetMaxRowNameLen+1; // +1 for [

      DispStr := BlankString(MaxRowNameLen)+' '+ StrToStrWidth(ExportOptions.FDAcronym, EntryLen);  // x.y
      if DistXls <> nil then
        DistXls.Add(ExportOptions.FDAcronym);
      OutList.Add(DispStr);
      if DistXls <> nil then
        DistXls.WriteLine(0, 'B'); // Start at B because we need to be one over since there are names also.
      for i:=0 to RowNames.Count-1 do
      begin
        DispStr := StrToStrWidth(RowNames[i], MaxRowNameLen)+' ';
        if DistXls <> nil then
          DistXls.Add(RowNames[i]);
        TheD := GetValueToWrite(i, True);
        if InvalidNum then
        begin
           DispStr := DispStr + StrToStrWidth('n/c',EntryLen) + ' ';
           if DistXls <> nil then
             DistXls.Add('n/c', clRed, clSilver);
        end
        else
        begin
           DispStr := DispStr + FloatToStrWidth(TheD, EntryLen, Y);
           if DistXls <> nil then
             DistXls.AddP(TheD, Y);  // PLACEHOLD
        end;

        if S <> nil then
        begin
          TheS := GetValueToWrite(i, False);
          if InvalidNum then
          begin
             DispStr := DispStr + StrToStrWidth('n/c',EntryLen) + ' ';
             if DistXls <> nil then
               DistXls.Add('n/c', clRed, clSilver);
          end
          else
          begin
             DispStr := DispStr + FloatToStrWidth(TheS,EntryLen, Y);
             if DistXls <> nil then
               DistXls.AddP(TheS, Y);
          end;

        end;
        OutList.Add(DispStr);
        if DistXls <> nil then
          DistXls.WriteLine();
        Wait.PercentDone := (i*100) div RowNames.Count + 20;
      end; // end for all Blocks
      //DistXls.ApplyFormat(2, 1, 255, DistXls.OutputLineLvl[0], FEPrecision);  // Not needed, if we already rounded the numbers
      if not (ExportOptions.OutputFormat = ExportExcel) or (ExportOptions.OutputFormat = ExportCSV) then
      {$IFDEF VISUAL_BUILD}
      Result.Text := OutList.Text
      {$ELSE}
      OutList.SaveToFile(ExportOptions.SaveExcelFileTo)
      {$ENDIF}
      else if (ExportOptions.OutputFormat = ExportCSV) then
      begin
      {$IFDEF VISUAL_BUILD}
        Result.text := DistXls.GetCsvText;
      {$ELSE}
        DistXls.SaveFile(ExportOptions.SaveExcelFileTo, ExportCSV);
        {$ENDIF}
      end;
      if DistXls <> nil then
        DistXls.SaveFile(ExportOptions.SaveExcelFileTo, ExportOptions.OutputFormat);
    except
      on E:Exception do
        ShowErrorMessage(E);
    end;
  finally
    if Assigned(Wait) then
      Wait.Free;
    if Assigned(DistXls) then
      DistXls.Free;
    if Assigned(OutList) then
      OutList.Free;
  end;
end;




procedure TD_InputDistdata.SaveSession(Filename: String);
var
  i, j: integer;
  b : Byte = 0;
  data : TFileStream;
  BufferString: AnsiString;
begin
  try
    try
      data := TFileStream.Create(filename, fmCreate);

      //#MDS DATA SESSION HEADERS
      BufferString := '#BAD'; //File is type Mega Data Session (#MDS)
      data.Write(BufferString[1], 4);
      i := MSDX_SESSION_VERSION; //Version
      data.Write(i, 4);
      i := VerifiedTargetPlatform;
      data.Write(i, SizeOf(i));

      i := 2; //Distance data
      data.Write(i, 4);

      {$IFDEF VISUAL_BUILD}
      BufferString := MegaForm.GetDataTitle;
      {$ELSE}
      BufferString := D_MegaMain.DataTitle;
      {$ENDIF}
      i := length(BufferString);
      data.Write(i, 4);
      data.Write(BufferString[1], i);
      {$IFDEF VISUAL_BUILD}
      BufferString := MegaForm.GetDataDescr;
      {$ELSE}
      BufferString := D_MegaMain.DataDescription;
      {$ENDIF}
      i := length(BufferString);
      data.Write(i, 4);
      if BufferString <> EmptyStr then
        data.Write(BufferString[1], i);

      //WRITE OUT THE FOTUINFOS
      FOtuInfos.WriteToFile(data, DataDist);

     //WRITE OUT DISP TAXA LIST
      i := DispTaxaList.Count;
      data.Write(i, 4);
      for j:=0 to DispTaxaList.Count-1 do
      begin
        i := DispTaxaList[j];
        data.Write(i, 4);
      end;

      //**** VISUAL STATUS ****//
      //SHOW ONLY SELECTED TAXA NAMES?
      with VS_DistDataExplorer do
      begin
        data.Write(DispSelTaxaItem, 1);

        //SHOW GROUP NAMES?
        data.Write(DispShowGpNames, 1);

        //DISPLAY AT WHAT PRECISION?
        data.Write(FPrecision, 4);

        //THE AMOUNT OF CHARACTERS LEFT OF THE DECIMAL POINT FOR THE DISTANCE WITH THE GREATEST LENGTH
        data.Write(LeftOfDecimalLen, 4);

        //WRITE OUT THE FONT
        //fontname
        BufferString := CurFont.name;
        i := Length(BufferString);
        data.Write(i, 4);
        if i > 0 then
          data.Write(BufferString[1], i);

        //character set
          i := CurFont.CharSet;
          data.Write(i, 4);

        //font size
        i:= CurFont.Size;
        data.Write(i, 4);
        
        //font style (bold, italics, etc.)
        {$IFNDEF FPC}
        b := Byte(CurFont.Style);
        {$ENDIF}
        data.Write(b, 4);
      end;


      i := 1396985123;
      data.Write(i, 4);

      data.Seek(soFromBeginning, 0);
      i := 1396985123;
      data.Write(i, 4);
    except
      on E : Exception do
        Raise(Exception.Create('Unable to save Distance Data Input Session: ' + E.Message));
    end;
  finally
    FreeAndNil(data);
  end;
end;

function TD_InputDistdata.FindTaxon(searchQuery: String; startIndex: Integer; searchForward: Boolean): Integer;
var
  i: Integer = -1;
  index: Integer = -1;
  aName: String = '';
  query: String = '';
begin
  Result := -1;
  if startIndex >= DispTaxaList.Count then
    Exit;
  if DispTaxaList.Count > 0 then
  begin
    query := LowerCase(searchQuery);
    if searchForward then
    begin
      for i := startIndex to DispTaxaList.Count - 1 do
      begin
        index := DispTaxaList[i];
        aName := LowerCase(FOtuInfos[index].Name);
        if Pos(query, aName) > 0 then
          Exit(i);
      end;
    end
    else
    begin
      for i := startIndex downto 0 do
      begin
        index := DispTaxaList[i];
        aName := LowerCase(FOtuInfos[index].Name);
        if Pos(query, aName) > 0 then
          Exit(i);
      end;
    end;
  end;
end;

end. //this ends the unit
