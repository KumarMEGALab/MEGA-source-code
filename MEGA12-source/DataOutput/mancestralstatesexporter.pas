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

unit mancestralstatesexporter;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$INTERFACES CORBA}

interface

uses
  {$IFDEF VISUAL_BUILD}
  MTreeBox,
  {$ENDIF}
  Classes, SysUtils, Types, ExcelWrite, MComputeParsimInfo, MegaUtils,
  mancestralstatesexportheader, MegaConsts, Graphics, MTreeData,
  MTreeDataAdapter, MSimpleTreeNode, MLTreeAnalyzer, Dialogs, MTreeList,
  MPleaseWait, MMatrixExport, MLTree, MLongintList;

type
  IExportsChangesList = interface
    ['{3096CE8A-3C50-4414-B32E-6B986EC0C5B6}']
    function ExportChangesList(ExportType : TExportType; Destination: String; includedSites: TIntArray): Boolean;
  end;

  TAncestralStatesExporter = class abstract (TObject, IExportsChangesList)
    public
      function ExportChangesList(ExportType : TExportType; Destination: String; includedSites: TIntArray): Boolean; virtual; abstract;
  end;

  PString = ^AnsiString;
  PArrayOfParsString = ^TArrayOfString;
  TParsAncStateProc = procedure(AncState: PArrayOfParsString; SiteIndex : integer; tree: TTreeData) of object;

  {$IFDEF VISUAL_BUILD}
  { TMLAncestralStatesExporter }

  TMLAncestralStatesExporter = class(TAncestralStatesExporter)
    protected
      FAncStates: TAncStateRecArray;
      MLAnalyzer: TMLTreeAnalyzer;
      Tree: TTreeBox;
      ShowOption: String;
      procedure GetAncStateProbWYSIWYG(var stateOut: AnsiString; var ProbabilityOUT: double);
      function GetAncestralStates(node, site: Int64): Boolean;
    public
      constructor Create(aMlAnalyzer: TMLTreeAnalyzer; aTreeBox: TTreeBox; aShowOption: String);
      destructor Destroy; override;
      function ExportChangesList(ExportType : TExportType; Destination: String; includedSites: TIntArray): Boolean; override;
      function ExportMostProbableToFasta(isAminoAcidData: Boolean): TStringList;
  end;
  {$ENDIF}

  { TParsimonyAncestralStatesExporter }

  TParsimonyAncestralStatesExporter = class(TAncestralStatesExporter)
    private
      FLog: TStringList;
      FNewickString: String;
      FTextWriter: TStringList;
      FExcelWrite: TExcelWrite;
      FHeader: TAncStatesExportHeader;
      FParsimInfo: TComputeParsimInfo;
      FNumNodes: Integer;
      FNumTaxa: Integer;
      FAncStateStr: T2DStringArray;
      FParsAncStateStr: T2DStringArray;
      FSiteNumberCellColor: TColor;
      FTreeData: TTreeData;
      FTree: TSimpleTreeNodeArray;
      FTreeAdapter: TSimpleTreeDataAdapter;
      FMLAnalyzer: TMLTreeAnalyzer;
      csvList: TStringList;
      FTreeList: TTreeList;
      FUseExtendedCharacterFormat: Boolean;
      procedure Generate;
      procedure GeneratePars(UseExtendedChars: Boolean);
      procedure ProcessDataForExcelWriter(includedSites: TIntArray);
      procedure ProcessDataForTextWriter(includedSites: TIntArray);
      function ExportToExcelFile(Destination: String; FileType: TOutputFileType; includedSites: TIntArray): Boolean;
      procedure ExportToTextEditor(includedSites: TIntArray);
      function ExportToTextFile(Filename: String; includedSites: TIntArray): Boolean;
      function LengthOfLongestLabel: Integer;
      function LengthOfLongestSiteColumn: Integer;
      procedure SetNewickString(AValue: String);
      procedure ProcessChangesForExcelWriter(SaveLocation: String; ExportType: TExportType; includedSites: TIntArray);
      procedure ProcessChangesForTextWriter(SaveLocation: String; ExportType: TExportType);
      procedure SetSiteNumberCellColor(const Value: TColor);
      procedure GetAncStateProbWYSIWYG(node, site: integer; tree: TMLTreeAnalyzer; var stateOut: AnsiString; var ProbabilityOUT: double);
      procedure ExportStatesToFastaList(includedSites: TIntArray);
      function ExportStatesToFastaFile(filename: String; includedSites: TIntArray): Boolean;
    public
      constructor Create(ParsimInfo: TComputeParsimInfo; Header: TAncStatesExportHeader; NumTaxa: Integer; NumNodes: Integer; useExtFormat: Boolean); overload;
      constructor Create(Header: TAncStatesExportHeader; ParsimInfo: TComputeParsimInfo; aTreeList: TTreeList; aTreeData: TTreeData; NumTaxa: Integer; NumNodes: Integer; MLAnalyzer: TMLTreeAnalyzer; useExtFormat: Boolean); overload;
      destructor Destroy; override;
      function ExportChangesList(ExportType: TExportType; Destination: String; includedSites: TIntArray): Boolean; override;
      function ExportStates(ExportType : TExportType; Destination: String; includedSites: TIntArray): Boolean;
      property SiteNumberCellColor: TColor read FSiteNumberCellColor write SetSiteNumberCellColor;
      property NewickString: String read FNewickString write SetNewickString;
      property UseExtendedCharacterFormat: Boolean read FUseExtendedCharacterFormat write FUseExtendedCharacterFormat;
  end;


implementation

uses
  {$IFDEF VISUAL_BUILD}
  MEditorForm, Mega_Main,
  {$ELSE}
  MD_MegaMain,
  {$ENDIF}
  LCLIntf, mstringbuilder,
  MGlobalSettings, Math;

{$IFDEF VISUAL_BUILD}
{ TMLAncestralStatesExporter }

{ WYSIWYG refers to what the user is viewing in the Tree Explorer - most probable, hide ambiguous, etc...}
procedure TMLAncestralStatesExporter.GetAncStateProbWYSIWYG(var stateOut: AnsiString; var ProbabilityOUT: double);
var
  i: integer;
begin
  stateOut := EmptyStr;
  ProbabilityOUT := -1;
  if SameText(showoption, SHOW_ALL) or (Trim(showOption) = EmptyStr) then
  begin
    for i := Low(FAncStates) to High(FAncStates) do
    begin
      if FAncStates[i].Prob >= 0.05 then  //If the state is at least 5% possible, add the state to the list of possible states
        stateOUT := stateOUT + FAncStates[i].Name;
    end;
    ProbabilityOUT := FAncStates[Low(FAncStates)].Prob;  //The probability is that of the most probable state /////CHECK THIS WITH DR. KUMAR/////
  end
  else if SameText(showoption, SHOW_MOST_PROBABLE) then
  begin
    stateOut := FAncStates[Low(FAncStates)].Name;
    ProbabilityOUT := FAncStates[Low(FAncStates)].Prob;
  end
  else if SameText(showoption, HIDE_AMBIGUOUS) then
  begin
    if FAncStates[Low(FAncStates)].Prob >= 0.95 then  //If its 95% likely to be just one of them then show it otherwise not
    begin
      stateOut := FAncStates[Low(FAncStates)].Name;
      ProbabilityOUT := FAncStates[Low(FAncStates)].Prob;
    end;
  end
  else
    raise Exception.Create('missing ancestral states option');
end;

function TMLAncestralStatesExporter.GetAncestralStates(node, site: Int64): Boolean;
begin
  Result := MLAnalyzer.GetAncStateProb(node, site, FAncStates);
end;

constructor TMLAncestralStatesExporter.Create(aMlAnalyzer: TMLTreeAnalyzer; aTreeBox: TTreeBox; aShowOption: String);
begin
  MLAnalyzer := aMLAnalyzer;
  Tree := aTreeBox;
  ShowOption := Trim(aShowOption);
end;

destructor TMLAncestralStatesExporter.Destroy;
begin
  MLAnalyzer := nil;
  inherited Destroy;
end;

function TMLAncestralStatesExporter.ExportChangesList(ExportType: TExportType; Destination: String; includedSites: TIntArray): Boolean;
var
  ArrowStr: WideString;
  NodeName: AnsiString;
  numcols: integer;
  PadToWidth : Array of integer;
  TempSep: TStringList = nil;
  j: integer;
  differencesfound : boolean;
  node, site, i, DataOffsetX, DataOffsetY:integer; //Node is the current node we are on, site is the current site we are on, line is used as a temporary variable to loop throught a TStringList
  MaxProbability, AncestorMaxProbability: double;    //Hold the probability of the most probable state at a node and site, holds the probability of a node and site we want to see if is larger
  MostProbableState: AnsiString = '';
  AncestorMostProbableState: AnsiString;  //Holds the 1 letter representation of the most probable state so far for a node and site, Holds the location of where to save exported file to users machine
  StringListResults: TStringList = nil;  //Transports probability results, Holds the data to export to excel, csv, text, etc
  MatrixExport : TMatrixExport = nil;  //Does the formating, exporting and opening of the file
  PleaseWait : TPleaseWait = nil;
  ExcelComponent : TExcelWrite = nil;
  ProbColor: TColor;
  Yellow: TColor;
  Red: TColor;
  Blue: TColor;
  Orange: TColor;
  DarkGrey: TColor;
  White: TColor;
  aRect : TRect;
  TextOutput: TStringList = nil;
begin
  Yellow       := RGB(232, 255,  26);
  Red          := RGB(230,   48, 26);
  Blue         := RGB(148, 191, 255);
  Orange       := RGB(255, 212, 148);
  DarkGrey     := RGB( 20,  20,  20);
  White        := RGB(250, 250, 250);

  try
    PleaseWait := TPleaseWait.Create(nil); { move this to TTreeViewForm?}
    PleaseWait.Action := 'Creating changes list...';
    PleaseWait.show;

    StringListResults := TStringList.Create;
    ExcelComponent := TExcelWrite.Create(nil, 'Changes List');
    ProbColor := White;

    //if ExportType in [EXexcelDisp, EXexcelSave, EXexcelXmlDisp, EXexcelXmlSave] then
    begin
      ExcelComponent.IsXLS := True;  //If the user choose an excel file type we must indicate it here
      //Write the legend for the first tab (yellow < 90% likely, red < 50% likely, white > 90% likely)
      ExcelComponent.Add('Probability of inference < 0.9 (90%)', Yellow);
      ExcelComponent.WriteLine();
      aRect := ExcelComponent.LastCellWriteXY();
      aRect.Right := aRect.Right + 4;
      ExcelComponent.MergeCells(aRect);
      ExcelComponent.Add('Probability of inference < 0.5 (50%)', Red);
      ExcelComponent.WriteLine();
      aRect := ExcelComponent.LastCellWriteXY();
      aRect.Right := aRect.Right + 4;
      ExcelComponent.MergeCells(aRect);
    end;

    ExcelComponent.AddBlankCell;
    ExcelComponent.Add('Taxa', Orange);
    ExcelComponent.WriteLine();
    aRect := ExcelComponent.LastCellWriteXY();
    aRect.Left := 1;
    aRect.Right := min(30, Tree.NoOfNodes-2) - aRect.left;
    ExcelComponent.MergeCells(aRect, aCenter);
    aRect.Right := Tree.NoOfNodes - aRect.Left;
    aRect.Left := aRect.Right;
    ExcelComponent.MergeCells(aRect);

    // add a row of descendent names/ids
    ExcelComponent.AddBlankCell;
    for node := 1 to Tree.NoOfNodes - 2 do
    begin
      if Tree.AncestorNodeNumber[node] = Tree.NoOfNodes then
        continue;
      NodeName := Tree.OTUName[node];
      if NodeName = EmptyStr then
        NodeName := IntToStr(node);
      ExcelComponent.Add(NodeName, Orange, 16777215, 90); //Draw Node Name vertically
    end;
    ExcelComponent.WriteLine;

    // add a row of arrows
    ExcelComponent.AddBlankCell;
    for node := 1 to Tree.NoOfNodes - 2 do
    begin
      if Tree.AncestorNodeNumber[node] = Tree.NoOfNodes then
        continue;
      ArrowStr := #$2192;
      ExcelComponent.Add(ArrowStr, Orange, 16777215, 90);
    end;
    ExcelComponent.WriteLine();

    // add a row of ancestor node ids
    ExcelComponent.AddBlankCell;
    for node := 1 to Tree.NoOfNodes - 2 do
    begin
      if Tree.AncestorNodeNumber[node] = Tree.NoOfNodes then
        continue;
      ExcelComponent.Add(IntToStr(Tree.AncestorNodeNumber[node]), Orange, 16777215, 90); //Draw Node Name vertically
    end;

    DifferencesFound := False;
    ExcelComponent.WriteLine;

    ExcelComponent.Add('Site', Blue);
    for node := 1 to Tree.NoOfNodes - 3 do
      ExcelComponent.AddBlankCell(Orange);
    ExcelComponent.WriteLine;

    DataOffsetX := (ExcelComponent.LastCellWriteXY.Left);
    DataOffsetY := (ExcelComponent.LastCellWriteXY.Top );

    if MLAnalyzer.Model.SeqDataType = DNA then
      SetLength(FAncStates, 4)
    else
      SetLength(FAncStates, 20);
    for site := 1 to Tree.MaxSiteIndex + 1 do
    begin
      if (site mod 1000) = 0 then
        PleaseWait.PercentDone := Round(site/(Tree.MaxSiteIndex + 1)*100);

      ExcelComponent.Add(includedSites[site - 1], Blue);
      For node := 1 to Tree.NoOfNodes - 2 do
      begin
        if Tree.AncestorNodeNumber[node] = Tree.NoOfNodes then
          continue;
        MaxProbability := 0;
        GetAncestralStates(node - 1, site);
        GetAncStateProbWYSIWYG(MostProbableState, MaxProbability);
        if (MostProbableState = EmptyStr) and (not (MaxProbability > 0)) then
          MostProbableState := '-'
        else if (MostProbableState = EmptyStr) then
          MostProbableState := '?';

        GetAncestralStates(Tree.AncestorNodeNumber[node] - 1, site);
        AncestorMostProbableState := '-';
        AncestorMaxProbability := FAncStates[0].Prob;
        if (AncestorMaxProbability > 0) then
          AncestorMostProbableState := FAncStates[0].Name;

        if AncestorMaxProbability < 0.50 then
          ProbColor := Red
        else if AncestorMaxProbability < 0.90 then
          ProbColor := Yellow
        else
          ProbColor := White;

        if MostProbableState <> AncestorMostProbableState then
        begin
          DifferencesFound := True;

          ExcelComponent.Add(WideString(AncestorMostProbableState + ' ' + ArrowStr + ' ' + MostProbableState), ProbColor);
        end
        else
          ExcelComponent.AddBlankCell;
      end;
      if DifferencesFound then
      begin
        ExcelComponent.WriteLine();
        DifferencesFound := False;
      end
      else
        ExcelComponent.Empty;
    end;
    //Draw the black line for the first tab to seperate site # and data
    aRect.Top := DataOffsetY+1;
    aRect.Bottom := DataOffsetY + Tree.MaxSiteIndex-1;
    aRect.Left := 0;
    aRect.Right := aRect.Left;
    ExcelComponent.ColorCells(aRect, DarkGrey, xlBorderRight);

    //Draw black line for first tab to sepearate node info and data
    aRect.Top := DataOffsetY;
    aRect.Bottom := aRect.Top;
    aRect.Left := DataOffsetX+1;
    aRect.Right := Tree.NoOfNodes - 2 + aRect.Left - 2;
    ExcelComponent.ColorCells(aRect, DarkGrey, xlBorderBottom);

  if ExportIsWorkbookDisplay(ExportType) and (ExportType <> EXcsvDisp) then
  begin
    ExcelComponent.SaveFile(Destination, ExcelExportToFileType(ExportType));
    RunAProgram(Destination)
  end
  else if ExportType = EXcsvDisp then
  begin
    TextOutput := ExcelComponent.GetCsvList;
    OpenStringList(TextOutput, 'Changes List', True);
  end
  else if ExportIsWorkbookSave(ExportType) then
    Result := ExcelComponent.SaveFile(Destination, ExcelExportToFileType(ExportType))
  else if (ExportType = EXtext) or (ExportType = EXtextSave) then
  begin
    TempSep := TStringList.Create;
    TextOutput := ExcelComponent.GetAsTabDelimitedList;
    NumCols := 0;

    for i := 0 to TextOutput.Count-1 do
    begin
      splitStr(TextOutput[i], ',', TempSep);
      if TempSep.count > numcols then
      begin
        numcols := TempSep.count;
        SetLength(PadToWidth, TempSep.Count);
      end;
      for j := 0 to TempSep.Count-1 do
        PadToWidth[j] := max(Length(TempSep.Strings[j]), PadToWidth[j]);
    end;

    for i := 0 to TextOutput.Count-1 do
    begin
      splitStr(TextOutput[i], ',', TempSep);
      TextOutput[i] := EmptyStr;
      for j := 0 to TempSep.Count-1 do
        TextOutput[i] := TextOutput[i] + PadStrToWidth(TempSep.Strings[j], PadToWidth[j]+3);
    end;
    TextOutput.Text := StringReplace(TextOutput.Text, '?', '>', [rfReplaceAll]);
    if ExportType = EXtextSave then
      TextOutput.SaveToFile(Destination)
    else
      OpenStringList(TextOutput, 'Changes List', true);
    if Assigned(PleaseWait) then
      PleaseWait.Hide;
    Result := True;
  end;
  finally
    if Assigned(StringListResults) then
      StringListResults.Free;
    if Assigned(MatrixExport) then
      MatrixExport.Free;
    if Assigned(PleaseWait) then
      PleaseWait.Free;
    if Assigned(ExcelComponent) then
      FreeAndNil(ExcelComponent);
    if Assigned(TextOutput) then
      TextOutput.Free;
  end;
end;

function TMLAncestralStatesExporter.ExportMostProbableToFasta(isAminoAcidData: Boolean): TStringList;
var
  node: Integer = -1;
  site: Integer = -1;
  b: TMegaStringBuilder = nil;
  NodeName: String = '';
  tempProbs: TAncStateRecArray = nil;
begin
  try
    Result := TStringList.Create;
    Assert(Assigned(MLAnalyzer));
    Assert(Assigned(Tree));
    b := TMegaStringBuilder.Create;
    if isAminoAcidData then
      SetLength(tempProbs, 20)
    else
      setLength(tempProbs, 4);

    for node := 0 to Tree.NoOfNodes - 2 do
    begin
      b.Clean;
      if Tree.OTUName[node + 1] = EmptyStr then  // an internal node
        NodeName := Format('>%d. %s', [node + 1, Tree.CoordsName[node + 1]])
      else
        NodeName := Format('>%s', [Tree.OTUName[node + 1]]);

      Result.Add(NodeName);

      For site := 1 to MLAnalyzer.NoOfSites do
      begin
        if MLAnalyzer.GetAncStateProb(node, site, tempProbs) then
          b.Add(tempProbs[0].Name)
        else
          b.Add('-');
        if (site mod 80) = 0 then
        begin
          Result.Add(b.GenerateString);
          b.Clean;
        end;
      end;
      Result.Add(b.GenerateString);
    end;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

{$ENDIF}

{ TParsimonyAncestralStatesExporter }

constructor TParsimonyAncestralStatesExporter.Create(ParsimInfo: TComputeParsimInfo; Header: TAncStatesExportHeader; NumTaxa: Integer; NumNodes: Integer; useExtFormat: Boolean);
begin
  FParsimInfo := ParsimInfo;
  FHeader := Header;
  FExcelWrite := TExcelWrite.Create(nil, 'Ancestral States');
  FTextWriter := TStringList.Create;
  FNumTaxa := NumTaxa;
  FNumNodes := NumNodes;
  FSiteNumberCellColor := RGB(153, 255, 255);
  FUseExtendedCharacterFormat := useExtFormat;
  FLog := TStringList.Create;
end;

constructor TParsimonyAncestralStatesExporter.Create(Header: TAncStatesExportHeader; ParsimInfo: TComputeParsimInfo; aTreeList: TTreeList; aTreeData: TTreeData; NumTaxa: Integer; NumNodes: Integer; MLAnalyzer: TMLTreeAnalyzer; useExtFormat: Boolean);
begin
  Create(ParsimInfo, Header, NumTaxa, NumNodes, useExtFormat);
  FTreeData := aTreeData;
  FTreeList := aTreeList;
  FTreeAdapter := TSimpleTreeDataAdapter.Create;
  FTreeAdapter.SetTreeData(FTreeData, FTreeList.isRooted);
  FTree := FTreeAdapter.GetSimpleTreeReference;
  FMLAnalyzer := MLAnalyzer;
end;

destructor TParsimonyAncestralStatesExporter.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  if Assigned(FExcelWrite) then
    FExcelWrite.Free;
  if Assigned(FTextWriter) then
    FTextWriter.Free;
  SetLength(FAncStateStr, 0);
  inherited;
end;

function TParsimonyAncestralStatesExporter.ExportStates(ExportType: TExportType; Destination: String; includedSites: TIntArray): Boolean;
var
  aList: TStringList = nil;
  debug: String = '';
begin
  Result := False;
  Generate;
  case ExportType of
    EXInvalid, EXnone: raise Exception.Create('Invalid export type for ancestral states');
    EXfasta:
      begin
        {$IFDEF VISUAL_BUILD}
        try
          Result := True;
          ExportStatesToFastaList(includedSites);
          try
            OpenStringList(FTextWriter, 'Ancestral States.fas');
          except
            on E:Exception do
            begin
              {$IFDEF DEBUG}
              debug := ExceptionCallStack(E);
              ShowMessage(debug);
              {$ENDIF}
            end;
          end;

          if FLog.Count > 0 then
          begin
            try
              OpenStringList(FLog, 'Ancestral States Info.txt');
            except
              on E:Exception do
              begin
                {$IFDEF DEBUG}
                debug := ExceptionCallStack(E);
                ShowMessage(debug);
                {$ENDIF}
              end;
            end;
          end;
        finally
          if Assigned(aList) then
            aList.Free;
        end;
        {$ELSE}
        raise Exception.Create('FASTA export of ancestral states is available in MEGA-GUI');
        {$ENDIF}
      end;
    EXFastaSave:
    begin
      Result := ExportStatesToFastaFile(Destination, includedSites);
      if FLog.Count > 0 then
        FLog.SaveToFile(ChangeFileExt(Destination, '.log'));
    end;
    EXtext:
      begin
        {$IFDEF VISUAL_BUILD}
        ExportToTextEditor(includedSites);
        Result := True;
        {$ELSE}
        Result := ExportToTextFile(Destination, includedSites);
        {$ENDIF}
      end;
    EXtextSave: Result := ExportToTextFile(Destination, includedSites);
    EXexcelDisp, EXexcelXmlDisp, EXodsDisp, EXcsvDisp:
      begin
        Result := ExportToExcelFile(Destination, ExcelExportToFileType(ExportType), includedSites);
        if Result then
          RunAProgram(Destination);
      end;
    EXexcelSave, EXexcelXmlSave, EXodsSave, EXcsvSave:
      begin
        Result := ExportToExcelFile(Destination, ExcelExportToFileType(ExportType), includedSites);
      end;
  end;
end;

function TParsimonyAncestralStatesExporter.ExportChangesList(ExportType: TExportType; Destination: String; includedSites: TIntArray): Boolean;
begin
  Result := False;
  GeneratePars(FUseExtendedCharacterFormat);
  ProcessChangesForExcelWriter(Destination, ExportType, includedSites);
  Result := True;
end;

procedure TParsimonyAncestralStatesExporter.ProcessChangesForExcelWriter(SaveLocation: String; ExportType: TExportType; includedSites: TIntArray);
var
  ArrowStr: WideString = '';
  NodeName: AnsiString = '';
  differencesfound : boolean = False;
  node, site, i, DataOffsetX, DataOffsetY:integer; //Node is the current node we are on, site is the current site we are on, line is used as a temporary variable to loop throught a TStringList
  MaxProbability, AncestorMaxProbability : double;    //Hold the probability of the most probable state at a node and site, holds the probability of a node and site we want to see if is larger
  MostProbableState, AncestorMostProbableState : String;  //Holds the 1 letter representation of the most probable state so far for a node and site, Holds the location of where to save exported file to users machine
  StringListResults: TStringList = nil;  //Transports probability results, Holds the data to export to excel, csv, text, etc
  ProbColor: TColor;
  Yellow: TColor;
  Red: TColor;
  Blue: TColor;
  Orange: TColor;
  DarkGrey: TColor;
  White: TColor;
  aRect : TRect;
  LoopFrom, LoopTo: Integer;
  status: Boolean = False;
begin
  Yellow       := RGB(232, 255,  26);
  Red          := RGB(230,   48, 26);
  Blue         := RGB(148, 191, 255);
  Orange       := RGB(255, 212, 148);
  DarkGrey     := RGB( 20,  20,  20);
  White        := RGB(250, 250, 250);
  ProbColor    := White;
  try
    try
      StringListResults := TStringList.Create;
      FExcelWrite.IsXLS := True;
      FExcelWrite.Add('Probability of inference < 0.9 (90%)', Yellow);
      FExcelWrite.WriteLine();
      aRect := FExcelWrite.LastCellWriteXY();
      aRect.Right := aRect.Right +4;
      FExcelWrite.MergeCells(aRect);
      FExcelWrite.Add('Probability of inference < 0.5 (50%)', Red);
      FExcelWrite.WriteLine();
      aRect := FExcelWrite.LastCellWriteXY();
      aRect.Right := aRect.Right +4;
      FExcelWrite.MergeCells(aRect);

      FExcelWrite.Add(' ');
      FExcelWrite.Add('Taxa', Orange);
      FExcelWrite.WriteLine();
      aRect := FExcelWrite.LastCellWriteXY();
      aRect.Left := 1;
      aRect.Right := min(30, FNumNodes - 3) - aRect.left - 1 ;
      FExcelWrite.MergeCells(aRect, aCenter);
      aRect.Right := FNumNodes - aRect.Left;
      aRect.Left := aRect.Right;
      FExcelWrite.MergeCells(aRect);

      // add a row of descendent names/ids
      FExcelWrite.Add(' ');
      for node:=0 to FNumNodes-3 do
      begin
        if FTree[node].Ancestor.NodeIndex = FNumNodes-1 then
          continue;
        NodeName := FTreeList.OTUName[node];
        if NodeName = EmptyStr then
          NodeName := IntToStr(node+1);
        FExcelWrite.Add(NodeName, Orange, 16777215, 90); //Draw Node Name vertically
      end;
      FExcelWrite.WriteLine();

      // add a row of arrows
      FExcelWrite.Add(' ');
      for node:=0 to FNumNodes-3  do
      begin
       if FTree[node].Ancestor.NodeIndex = FNumNodes-1 then
          continue;
          ArrowStr := #$2192;
        FExcelWrite.Add(ArrowStr, Orange, 16777215, 90); //Draw Node Name vertically
      end;
      FExcelWrite.WriteLine();

      // add a row of ancestor node ids
      FExcelWrite.Add(' ');
      for node:=0 to FNumNodes-3 do
      begin
        if FTree[node].Ancestor.NodeIndex = FNumNodes-1 then
          continue;
        FExcelWrite.Add(IntToStr(FTree[node].Ancestor.NodeIndex+1), Orange, 16777215, 90); //Draw Node Name vertically
      end;

      DifferencesFound := False;
      FExcelWrite.WriteLine();

      FExcelWrite.Add('Site', Blue);
      for node:=0 to FNumNodes-3 do
        FExcelWrite.Add(EmptyStr,Orange);
      FExcelWrite.WriteLine();

      DataOffsetX := (FExcelWrite.LastCellWriteXY().Left);
      DataOffsetY := (FExcelWrite.LastCellWriteXY().Top );

      for site:=1 to FParsimInfo.NoOfSites-1 do
      begin
        FExcelWrite.Add(includedSites[site - 1], Blue);
        LoopFrom := 0;
        LoopTo := FNumNodes-3;
        for node:=LoopFrom to LoopTo do
        begin
          if FTree[node].Ancestor.NodeIndex = FNumNodes-1 then
            continue;
          MaxProbability := 0;  //since we're at a new site, reset probability
          StringListResults.Clear;  //Also reset the object that obtains probability results
          if FMLAnalyzer = nil then
          begin
            for i := 0 to Min(MAX_COLS - 1, FParsimInfo.NoOfSites - 1) do
              begin
                if (site + i) >= (FParsimInfo.NoOfSites) then
                  break;
                MostProbableState := FParsAncStateStr[Node][site-1];
              end;
          end
          else
              GetAncStateProbWYSIWYG(node, site, FMLAnalyzer, MostProbableState, MaxProbability);
          if (MostProbableState = EmptyStr) and (not (MaxProbability > 0)) then // GS - added 1-09-2012 so that we don't export invalid changes
            MostProbableState := '-'
          else if (MostProbableState = EmptyStr) then
            MostProbableState := '?';
          if FMLAnalyzer = nil then
          begin
            for i := 0 to Min(MAX_COLS - 1, FParsimInfo.NoOfSites - 1) do
              begin
                if (site + i) >= (FParsimInfo.NoOfSites) then
                  break;
                AncestorMostProbableState := FParsAncStateStr[FTree[node].Ancestor.NodeIndex][site-1];
              end;
          end
          else
          begin
            StringListResults.Clear;
            status := FMLAnalyzer.GetAncStateProb(FTree[node].AncSpeciesIndex, site, StringListResults);
          end;
          if FMLAnalyzer <> nil then
          begin
            AncestorMaxProbability := 0;
            AncestorMostProbableState := '-';
            if status then
              AncestorMaxProbability := StrToFloat(Copy(StringListResults[0], 3, 20)) //Extract the probability as float
            else
              AncestorMaxProbability := 0;
            if (AncestorMaxProbability > 0) then
              AncestorMostProbableState := Copy(StringListResults[0], 0, 1);  //Extract state (i.e. A, T, C, G)
          end;
          if FMLAnalyzer <> nil then
          begin
            if CompareValue(AncestorMaxProbability, 0.50, FP_CUTOFF) < 0 then
              ProbColor := Red
            else if CompareValue(AncestorMaxProbability, 0.90, FP_CUTOFF) < 0 then
              ProbColor := Yellow
            else
              ProbColor := White;
          end;

          if MostProbableState <> AncestorMostProbableState then
          begin
            DifferencesFound := True;
            FExcelWrite.Add(WideString(AncestorMostProbableState + ' ' + ArrowStr + ' ' + MostProbableState), ProbColor);
          end
          else
            FExcelWrite.AddBlankCell;
          end;
            if DifferencesFound then
            begin
              FExcelWrite.WriteLine();
              DifferencesFound := False;
            end
            else
              FExcelWrite.Empty;
      end;
      //Draw the black line for the first tab to seperate site # and data
      aRect.Top := DataOffsetY+1;
      aRect.Bottom := DataOffsetY + FParsimInfo.NoOfSites;
      aRect.Left := 0;
      aRect.Right := aRect.Left;
      FExcelWrite.ColorCells(aRect, DarkGrey, xlBorderRight);

      //Draw black line for first tab to sepearate node info and data
      aRect.Top := DataOffsetY;
      aRect.Bottom := aRect.Top;
      aRect.Left := DataOffsetX+1;
      aRect.Right := FNumNodes-4 + aRect.Left;
      FExcelWrite.ColorCells(aRect, DarkGrey, xlBorderBottom);

    if not ExportIsTextEditorDisplay(ExportType) then
      FExcelWrite.SaveFile(SaveLocation, ExcelExportToFiletype(ExportType));
    If ExportIsWorkbookDisplay(ExportType) then
      RunAProgram(SaveLocation);
    if ExportIsTextEditorDisplay(ExportType) then
    begin
       csvList := FExcelWrite.GetCsvList;
       ProcessChangesForTextWriter(SaveLocation, ExportType);
    end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(StringListResults) then
      FreeAndNil(StringListResults);
    if Assigned(csvList) then
      csvList.Free;
    if Assigned(FExcelWrite) then
      FreeAndNil(FExcelWrite);
  end;
end;

procedure TParsimonyAncestralStatesExporter.ProcessChangesForTextWriter(SaveLocation: String; ExportType: TExportType);
var
  TempSep: TStringList;
  numcols: integer;
  PadToWidth : Array of integer = nil;
  i, j: Integer;
begin
    TempSep := TStringList.Create;
    NumCols := 0;
    for i := 0 to csvList.Count-1 do
    begin
      splitStr(csvList[i], ',', TempSep);
      if TempSep.count > numcols then
      begin
        numcols := TempSep.count;
        SetLength(PadToWidth, TempSep.Count);
      end;
      for j := 0 to TempSep.Count-1 do
        PadToWidth[j] := max(Length(TempSep.Strings[j]), PadToWidth[j]);
    end;
    for i := 0 to csvList.Count-1 do
    begin
      splitStr(csvList[i], ',', TempSep);
      csvList[i] := EmptyStr;
      for j := 0 to TempSep.Count-1 do
        csvList[i] := csvList[i] + PadStrToWidth(TempSep.Strings[j], PadToWidth[j]+3);
    end;
    csvList.Text := StringReplace(csvList.Text, '?', '>', [rfReplaceAll]);
  if ExportType = EXtext then
  begin
  {$IFDEF VISUAL_BUILD}
    OpenStringList(csvList, 'Changes List', true);
  {$ENDIF}
  {$IFNDEF VISUAL_BUILD}
    csvList.SaveToFile(SaveLocation);
  {$ENDIF}
  end
  else csvList.SaveToFile(SaveLocation);
end;

procedure TParsimonyAncestralStatesExporter.GetAncStateProbWYSIWYG(node, site: integer; tree: TMLTreeAnalyzer; var stateOut: AnsiString; var ProbabilityOUT: double);
var
  Probability: double = 0;
  State: AnsiString = '';
  i: integer = 0;
  TempList : TStringList = nil;
  status: Boolean = False;
begin
  stateOut := EmptyStr;
  ProbabilityOUT := -1;
  try
    TempList :=  TStringList.Create;
    status := tree.GetAncStateProb(node, site, TempList);
    for i := 0 to TempList.Count - 1 do
    begin
      State := TempList.Names[i];
      if status then
        Probability := StrToFloat(TempList.values[TempList.Names[i]])
      else
        Probability := 0;

      if CompareValue(Probability, 0.05, FP_CUTOFF) >= 0 then  //If the state is at least 5% possible, add the state to the list of possible states
        stateOUT := stateOUT + State;
    end;
    if status then
      ProbabilityOUT := StrToFloat(TempList.Values[TempList.Names[0]])  //The probability is that of the most probable state /////CHECK THIS WITH DR. KUMAR/////
    else
      ProbabilityOUT := 0;
  finally
    if Assigned(TempList) then
      TempList.Free;
  end;
end;

procedure TParsimonyAncestralStatesExporter.ExportStatesToFastaList(includedSites: TIntArray);
var
  positionAfterSubsetting: Integer = -1;
  positionBeforeSubsetting: Integer = -1;
  logNeedsSubsettingExplanation: Boolean = False;
  logFmtStr: String = '';
  i, Node: integer;
  States: String;
  b: TMegaStringBuilder = nil;
  recoded: String = '';
  nSites: Integer = 0;

  function GenerateLogFmtStr: String;
  var
    sitesStr: String = '';
    nodesStr: String = '';
  begin
    sitesStr := IntToStr(FParsimInfo.NoOfSites);
    nodesStr := IntToStr(FNumNodes);
    Result := Format('%s for node %%%d.0n at site %%%d.0n', ['%s', Length(nodesStr), Length(sitesStr)]);
  end;

  procedure LogMsg(aSite: Integer; aNode: Integer; aMsg: String);
  begin
    if includedSites[aSite - 1] <> aSite then
    begin
      FLog.Add(Format(logFmtStr + ' (%.0n)', [aMsg, aNode*1.0, aSite*1.0, includedSites[aSite - 1]*1.0]));
      if not logNeedsSubsettingExplanation then
      begin
        positionBeforeSubsetting := includedSites[aSite - 1];
        positionAfterSubsetting := aSite;
      end;
      logNeedsSubsettingExplanation := True;
    end
    else
      FLog.Add(Format(logFmtStr, [aMsg, aNode*1.0, aSite*1.0]));
  end;

  function TryRecodeAmbiguousNucStates(s: String): String;
  begin
    Result := s;
    if Length(Result) = 1 then
      Exit;

    if (s = 'AG') or (s = 'GA') then
      Result := 'R'
    else if (s = 'CT') or (s = 'TC') then
      Result := 'Y'
    else if (s = 'GT') or (s = 'TG') or (s = 'GU') or (s = 'UG') then
      Result := 'K'
    else if (s = 'CG') or (s = 'GC') then
      Result := 'S'
    else if (s = 'AT') or (s = 'TA') or (s = 'AU') or (s = 'UA') then
      Result := 'W'
    else if (s = 'AC') or (s = 'CA') then
      Result := 'M'
    else if not s.Contains('A') then
      Result := 'B'
    else if not s.Contains('C') then
      Result := 'D'
    else if not s.Contains('G') then
      Result := 'H'
    else if (not s.Contains('T')) and (not s.Contains('U')) then
      Result := 'V';
    if Result <> s then
      LogMsg(i + 1, Node + 1,Format('ambiguous bases were recoded from %6s to %s', [ s, Result]));
  end;

begin
  FTextWriter.Clear;
  FLog.Clear;
  logFmtStr := GenerateLogFmtStr;

  try
    b := TMegaStringBuilder.Create;
    FHeader.ReplaceSpacesInNodeLabels;
    for Node := 0 to FNumNodes - 1 do
    begin
      b.Clean;
      b.Add('>');
      if Node < FNumTaxa then
        b.Add(FHeader[Node].NodeLabel)
      else
      begin
        if (FHeader[Node].NodeLabel = EmptyStr) or (FHeader[Node].NodeLabel = '-') then
          b.Add(Format('%d. (%s . %s)', [FHeader[Node].NodeId, FHeader[Node].Child1, FHeader[Node].Child2]))
        else
          b.Add(FHeader[Node].NodeLabel);
      end;
      FTextWriter.Add(b.GenerateString);
      b.Clean;
      nSites := 0;
      for i := 0 to FParsimInfo.NoOfSites - 1 do
      begin
        States := FAncStateStr[Node][i];
        if Length(States) = 1 then
          b.Add(States)
        else
        begin
          if FParsimInfo.IsNucData and UseExtendedCharacterFormat then
          begin
            recoded := TryRecodeAmbiguousNucStates(states);
            if recoded <> EmptyStr then
              b.Add(recoded[1])
            else
              b.Add(states);
          end
          else
          begin
            if Length(states) > 1 then
            begin
              b.Add(states[1]);
              LogMsg(i + 1, Node + 1, Format('equally parsimonious states %6s found. %s was kept and %s discarded', ['(' + states + ')', states[1], copy(states, 2, Length(states))]));
            end
            else
              b.Add(states);
          end;
        end;

        inc(nSites);
        if nSites = 80 then
        begin
          FTextWriter.Add(b.GenerateString);
          b.Clean;
          nSites := 0;
        end;
      end;
      FTextWriter.Add(b.GenerateString);
    end;
    if logNeedsSubsettingExplanation then
    begin
      FLog.Insert(0, Format('Note: indices are given for site positions before and after subsetting, e.g. "%.0n (%.0n)" indicates ', [positionAfterSubsetting*1.0, positionBeforeSubsetting*1.0]));
      FLog.Insert(1, Format('      position %.0n after data subsetting with %.0n being the original position in the full sequence alignment.', [positionAfterSubsetting*1.0, positionBeforeSubsetting*1.0]));
      FLog.Insert(2, EmptyStr);
    end;
  finally
    if Assigned(b) then
      b.Free;
  end;
end;

function TParsimonyAncestralStatesExporter.ExportStatesToFastaFile(filename: String; includedSites: TIntArray): Boolean;
begin
  Result := False;
  ExportStatesToFastaList(includedSites);
  FTextWriter.SaveToFile(filename);
  Result := FileExists(filename);
end;

function TParsimonyAncestralStatesExporter.ExportToExcelFile(Destination: String; FileType: TOutputFileType; includedSites: TIntArray): Boolean;
begin
  Result := False;
  FExcelWrite.IsXLS := True;
  ProcessDataForExcelWriter(includedSites);
  FExcelWrite.SaveFile(Destination, FileType);
  Result := FileExists(Destination);
end;

procedure TParsimonyAncestralStatesExporter.ExportToTextEditor(includedSites: TIntArray);
begin
  {$IFDEF VISUAL_BUILD}
  ProcessDataForTextWriter(includedSites);
  OpenStringList(FTextWriter, 'Ancestral States', true);
  {$ENDIF}
end;

function TParsimonyAncestralStatesExporter.ExportToTextFile(Filename: String; includedSites: TIntArray): Boolean;
begin
  ProcessDataForTextWriter(includedSites);
  FTextWriter.SaveToFile(Filename);
  Result := FileExists(Filename);
end;

procedure TParsimonyAncestralStatesExporter.Generate;
var
  AncStates: PArrayOfLongint;
  Site: Integer;
  Node: Integer;
  i: Integer;
begin
  try
    GetMem(AncStates, SizeOf(LongInt)*FNumNodes);
    SetLength(FAncStateStr, FNumNodes);
    for i := 0 to FNumNodes - 1 do
      SetLength(FAncStateStr[i], FParsimInfo.NoOfSites);
    for Site := 0 to FParsimInfo.NoOfSites - 1 do
    begin
      FParsimInfo.ComputeSiteMPAncStates(Site, AncStates^);
      if FParsimInfo.IsNucData then
        for Node := 0 to FNumNodes - 1 do
          FAncStateStr[Node][Site] := ParsimMapToNucStr(AncStates[Node])
      else
        for Node := 0 to FNumNodes - 1 do
          FAncStateStr[Node][Site] := ParsimMapToAminoStr(AncStates[Node]);
    end;
  finally
    FreeMemAndNil(AncStates);
  end;
end;

procedure TParsimonyAncestralStatesExporter.GeneratePars(UseExtendedChars: Boolean);
var
  AStates: PArrayOfLongint;
  Site: Integer;
  Node: Integer;
  i: Integer;
begin
  try
    GetMem(AStates, SizeOf(LongInt)*FNumNodes);
    SetLength(FParsAncStateStr, FNumNodes);
    for i := 0 to FNumNodes - 1 do
      SetLength(FParsAncStateStr[i], FParsimInfo.NoOfSites);
    for Site := 0 to FParsimInfo.NoOfSites - 1 do
    begin
      FParsimInfo.ComputeSiteMPAncStates(Site, AStates^);
      if FParsimInfo.IsNucData then
        for Node := 0 to FNumNodes - 1 do
        begin
          if UseExtendedChars then
            FParsAncStateStr[Node][site] := ParsimMapToNuc(AnsiChar(Chr(AStates[Node])))
          else
            FParsAncStateStr[Node][site] := ParsimMapToNucStr(AStates[Node])
        end
      else
        for Node := 0 to FNumNodes - 1 do
        begin
          FParsAncStateStr[Node][site] := ParsimMapToAminoStr(AStates[Node]);

        end;
    end;
  finally
    FreeMemAndNil(AStates);
  end;
end;

function TParsimonyAncestralStatesExporter.LengthOfLongestLabel: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FHeader.Count > 0 then
    for i := 0 to FHeader.Count - 1 do
      if Length(FHeader[i].NodeLabel) > Result then
        Result := Length(FHeader[i].NodeLabel);
end;

function TParsimonyAncestralStatesExporter.LengthOfLongestSiteColumn: Integer;
var
  Node, Site: Integer;
begin
  Result := Length(IntToStr(FParsimInfo.NoOfSites));
  if Length(FAncStateStr) > 0 then
  begin
    for Node := 0 to Length(FAncStateStr) - 1 do
    begin
      if Length(FAncStateStr[Node]) > 0 then
      begin
        for Site := 0 to Length(FAncStateStr[Node]) - 1 do
          if Length(FAncStateStr[Node][Site]) > Result then
            Result := Length(FAncStateStr[Node][Site]);
      end;
    end;
  end;
end;

procedure TParsimonyAncestralStatesExporter.SetNewickString(AValue: String);
begin
  if FNewickString=AValue then Exit;
  FNewickString:=AValue;
end;

procedure TParsimonyAncestralStatesExporter.ProcessDataForExcelWriter(includedSites: TIntArray);
const
  MAX_COLS = 60;
var
  i, Site, Node: integer;
  Cells : TRect;
  States: AnsiString;
  TempInt: Integer;
  HeaderRect: TRect;
begin
  {$IFDEF VISUAL_BUILD}
  FExcelWrite.Add('Inferred Ancestral States');
  FExcelWrite.WriteLine;
  Cells := FExcelWrite.LastCellWriteXY;
  Cells.Right := Cells.Right + Min(MAX_COLS, FParsimInfo.NoOfSites) - 1;
  FExcelWrite.MergeCells(Cells, aCenter);
  FExcelWrite.BoldCells(Cells);

  FExcelWrite.AddBlankCell;
  FExcelWrite.AddBlankCell;
  FExcelWrite.AddBlankCell;
  FExcelWrite.AddBlankCell;
  FExcelWrite.Add('Site No.');
  FExcelWrite.WriteLine;
  Cells := FExcelWrite.LastCellWriteXY;
  Cells.Left := 4;
  Cells.Right := Cells.Right + Min(MAX_COLS, FParsimInfo.NoOfSites) - 1;
  FExcelWrite.MergeCells(Cells, aCenter);

  Site := 0;
  while Site < FParsimInfo.NoOfSites do
  begin
    FExcelWrite.Add('Index');
    FExcelWrite.Add('Label');
    FExcelWrite.Add('Des 1');
    FExcelWrite.Add('Des 2');
    for i := 0 to Min(MAX_COLS - 1, FParsimInfo.NoOfSites - 1) do
    begin
      if (Site + i + 1) <= FParsimInfo.NoOfSites then
        FExcelWrite.Add(includedSites[Site + i], FSiteNumberCellColor)
      else
        break
    end;

    FExcelWrite.WriteLine;
    HeaderRect := FExcelWrite.LastCellWriteXY;
    HeaderRect.Left := 0;
    HeaderRect.Right := HeaderRect.Right + (i - 1) + 4;
    FExcelWrite.ColorCells(HeaderRect, RGB(0,0,0), xlBorderTop);
    FExcelWrite.ColorCells(HeaderRect, RGB(0,0,0), xlBorderBottom);
    FExcelWrite.AlignCells(HeaderRect, aCenter);

    for Node := 0 to FNumNodes - 1 do
    begin
      FExcelWrite.Add(FHeader[Node].NodeId);
      FExcelWrite.Add(FHeader[Node].NodeLabel);
      if TryStrToInt(FHeader[Node].Child1, TempInt) then
        FExcelWrite.Add(TempInt)
      else
        FExcelWrite.Add(FHeader[Node].Child1);
      if TryStrToInt(FHeader[Node].Child2, TempInt) then
        FExcelWrite.Add(TempInt)
      else
        FExcelWrite.Add(FHeader[Node].Child2);
      for i := 0 to Min(MAX_COLS - 1, FParsimInfo.NoOfSites - 1) do
      begin
        if (Site + i) >= (FParsimInfo.NoOfSites) then
          break;
        States := FAncStateStr[Node][Site + i];
        FExcelWrite.Add(States);
      end;
      FExcelWrite.WriteLine;
      Cells := FExcelWrite.LastCellWriteXY;
      Cells.Left := 4;
      Cells.Right := Cells.Right + i - 1 + 4;
      FExcelWrite.AlignCells(Cells, aCenter);
      HeaderRect.Bottom := HeaderRect.Top + FNumNodes;
      HeaderRect.Left := 3;
      HeaderRect.Right := 3;
      FExcelWrite.ColorCells(HeaderRect, RGB(0,0,0), xlBorderRight);
    end;
    FExcelWrite.AddBlankCell;
    FExcelWrite.WriteLine;
    inc(Site, MAX_COLS);
  end;
  {$ENDIF}
end;

procedure TParsimonyAncestralStatesExporter.ProcessDataForTextWriter(includedSites: TIntArray);
const
  MAX_COLS = MaxInt;
  PADDING = 4;
var
  DesColWidth, IndexColWidth, LabelColWidth, StatesColWidth: String;
  i, Site, Node: integer;
  States: AnsiString;
  Line: String;
begin
  DesColWidth := IntToStr(Max(Length('Des_2'), Length(IntToStr(FNumNodes))) + PADDING);
  IndexColWidth := IntToStr(Max(Length('Index'), Length(IntToStr(FNumNodes))) + PADDING);
  LabelColWidth := IntToStr(LengthOfLongestLabel + PADDING);
  StatesColWidth := IntToStr(LengthOfLongestSiteColumn + PADDING);
  FTextWriter.Add('Ancestral States For All Sites');
  {$IFDEF VISUAL_BUILD}
  FTextWriter.Add('Filename: ' + ExtractFileName(MegaForm.DataFileName));
  {$ELSE}
  FTextWriter.Add('Filename: ' + ExtractFilename(D_MegaMain.DataFileName));
  {$ENDIF}
  FTextWriter.Add('No of Taxa: ' + IntToStr(FNumTaxa));
  FTextWriter.Add('No of Sites: ' + IntToStr(FParsimInfo.NoOfSites));
  if Trim(FNewickString) <> EmptyStr then
    FTextWriter.Add('Newick: ' + FNewickString);
  FTextWriter.Add(' ');
  Site := 0;
  while Site < FParsimInfo.NoOfSites do
  begin
    Line := Format('%:-' + IndexColWidth + 's', ['Index']);
    Line := Line + Format('%:-' + LabelColWidth + 's', ['Label']);
    Line := Line + Format('%:-' + DesColWidth + 's', ['Des_1']);
    Line := Line + Format('%:-' + DesColWidth + 's', ['Des_2']);
    for i := 0 to Min(MAX_COLS - 1, FParsimInfo.NoOfSites - 1) do
    begin
      if (Site + i + 1) <= FParsimInfo.NoOfSites then
        Line := Line + Format('%:-' + StatesColWidth + 'd', [includedSites[Site + i]])
      else
        break
    end;
    FTextWriter.Add(Line);

    FHeader.ReplaceSpacesInNodeLabels;
    for Node := 0 to FNumNodes - 1 do
    begin
      Line := Format('%:-' + IndexColWidth + 'd', [FHeader[Node].NodeId]);
      Line := Line + Format('%:-' + LabelColWidth + 's', [FHeader[Node].NodeLabel]);
      Line := Line + Format('%:-' + DesColWidth + 's', [FHeader[Node].Child1]);
      Line := Line + Format('%:-' + DesColWidth + 's', [FHeader[Node].Child2]);
      for i := 0 to Min(MAX_COLS - 1, FParsimInfo.NoOfSites - 1) do
      begin
        if (Site + i) >= (FParsimInfo.NoOfSites) then
          break;
        States := FAncStateStr[Node][Site + i];
        Line := Line + Format('%:-' + StatesColWidth + 's', [States]);
      end;
      FTextWriter.Add(Line);
    end;
    FTextWriter.Add(' ');
    inc(Site, MAX_COLS);
  end;
end;

procedure TParsimonyAncestralStatesExporter.SetSiteNumberCellColor(const Value: TColor);
begin
  FSiteNumberCellColor := Value;
end;

end.
