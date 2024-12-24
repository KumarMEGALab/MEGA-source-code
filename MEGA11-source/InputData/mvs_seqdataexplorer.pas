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

unit MVS_SeqDataExplorer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
    Classes,
    Graphics,
    MegaConsts,
    {$IFDEF VISUAL_BUILD}
    MV_SeqDataExplorer,
    {$ENDIF}
    MLongintList,
    Types
    ;

type

  { TVS_SeqDataExplorer }

  TVS_SeqDataExplorer = class(TObject)
  public
    function IsHighlighted(ASite: Integer): Boolean;
    procedure Initialize;
    {$IFDEF VISUAL_BUILD}
    procedure ApplyToVisual(visual : TV_SeqDataExplorer);
    {$ENDIF}

    procedure UpdateCodeTable;
    procedure RefreshDataGrid;
  public

    Constructor Create;
    Destructor Destroy; override;  // override necessary, otherwise this doesn't free correctly.
  private
    CoveragePercentage   : Integer;
    FTaxaGpsDlgOrderList : TLongIntList;
    Labelled: Boolean;
    function GetTaxaGpsDlgOrder(Index: LongInt):Integer;
    procedure SetLabelled(AValue: Boolean);
    procedure SetTaxaGpsDlgOrder(Index: LongInt; Value: LongInt);

    procedure SetFourFold(const Value: Boolean);
    procedure SetParsimInfo(const Value: Boolean);
    procedure SetSingleton(const Value: Boolean);
    procedure SetTwoFold(const Value: Boolean);
    procedure SetVarible(const Value: Boolean);
    procedure SetZeroFold(const Value: Boolean);
    procedure SetConstant(const Value: Boolean);
    procedure SetSpecial(const Value: Boolean);
    procedure ClearHighlightItems;

  Public
    CurStatusBarStatistics:     TStringList;

    allTaxaChecked:             Boolean;
    allTaxaCheckDown:           Boolean;
    FIsResizing:                Boolean;
    targetedResizingCol:        Integer;

    DispSelTaxaItem:            Boolean;
    DispTaxaNamesItem:          Boolean;
    DispGpNamesItem:            Boolean;
    DispBirdsEyeViewItem:       Boolean;
    DispColorItem:              Boolean;

    DispUseIdenSymbolItem:      Boolean;

    DispResultsOnlyItem:        Boolean;

    Constant:               Boolean;
    Variable:               Boolean;
    ParsimInfo:             Boolean;
    Singleton:              Boolean;
    ZeroFold:               Boolean;
    TwoFold:                Boolean;
    FourFold:               Boolean;
    Special:                Boolean;

    StatUseOnlyMarkedSitesItem:  Boolean;
    StatUseOnlyUnMarkedSitesItem:  Boolean;
    StatAllSelSitesItem:         Boolean;

    StatDispInXLItem:           Boolean;
    StatDispInXlsItem:          Boolean;
    StatDispInOdsItem:          Boolean;
    StatDispInCSVItem:          Boolean;
    StatDispInTextItem:         Boolean;


    NoOfSelTaxa:       LongInt;      // no of sequences currently selected
    MaxTaxaNamePixels: LongInt;      // Pixels needed to show taxa names
    MaxGpNamePixels:   LongInt;      // Pixels needed to show gps
    MaxSpNamePixels:   LongInt;      // Pixels needed to show species
    MaxPopNamePixels:  LongInt;      // Pixels needed to show populations

    IsTranslated:  Boolean;        // True if Translated Seq displayed

    CurFocusRow:   LongInt;
    CurFocusCol:   LongInt;


    CurAttrDisp:   TSiteAttrType;  // var/parsim-info/2-fold/etc to be highlighted

    HighlightItem: TObject;        // to keep pointer

    HighlightColor:TColor;

    NoOfSitesUsed: LongInt;
    NoOfCodingSitesUsed:LongInt;

    CurColorRowRange : TPoint;  // just to show Koichiro style color for rows and columns
    CurColorSite : Integer;
    CurCol      : Integer;
    CurRow      : Integer;

    CurSearchStr: AnsiString;
    CurMotifStr : AnsiString;
    CurGroupStr : AnsiString;

    HideNameItem   : Boolean;
    HideSeqItem    : Boolean;
    HideMotifItem  : Boolean;
    ResultsOnlyItem: Boolean;

    CurFont: TFont;

    LastKMerSearch: AnsiString; // The motif for the last K-Mer search a user did
    LastKMerWindow: Integer; // Same as above, but the window size (k-window = max # ambiguous allowed)

     property TaxaGpsDlgOrder[Index: LongInt]: LongInt read GetTaxaGpsDlgOrder write SetTaxaGpsDlgOrder;
     property   ConstantItem:   Boolean read Constant  write SetConstant;
     property   VariableItem:   Boolean read Variable  write SetVarible;
     property   ParsimInfoItem: Boolean read ParsimInfo Write SetParsimInfo;
     property   LabelledItem:   Boolean read Labelled   write SetLabelled;
     property   SingletonItem:  Boolean read Singleton  Write SetSingleton;
     property   ZeroFoldItem:   Boolean read ZeroFold   Write SetZeroFold;
     property   TwoFoldItem:    Boolean read TwoFold    write SetTwoFold;
     property   FourFoldItem:   Boolean read FourFold   write SetFourFold;
     property   SpecialItem:    Boolean read Special    write SetSpecial;
     property   CoveragePerc:   Integer read CoveragePercentage write CoveragePercentage;
  public
    ApplyingToVisual: Boolean;
    function IsXls: Boolean;
    function DisplaySpreadsheetExport: Boolean;
    procedure NotifySearchCompletion(NumOfRes: Integer);
    procedure JumpTo(Top, Left: Integer);
  end;

var
  VS_SeqDataExplorer : TVS_SeqDataExplorer;

implementation

uses
  MD_InputSeqData;

constructor TVS_SeqDataExplorer.Create;
begin
  MaxGpNamePixels   := -1;
  MaxTaxaNamePixels := -1;

  IsTranslated := False;
  CurFocusRow  := -1;
  CurFocusCol  := -1;

  HighlightItem :=nil;
  CurAttrDisp   := megNone;
  HighlightColor := clYellow;

  CurColorRowRange := Point(-1, -1);
  CurColorSite := -1;

  CurStatusBarStatistics := TStringList.Create;

  CurFont := TFont.Create;

  FTaxaGpsDlgOrderList := TLongIntList.Create;
  DispTaxaNamesItem := True;
end;

procedure TVS_SeqDataExplorer.Initialize;
var
  i : integer;
begin
  NoOfSelTaxa := D_InputSeqData.FOtuInfos.NoOfSelOtus;
  for i:=0 to 10 do
    CurStatusBarStatistics.Add('');

  {$IFDEF VISUAL_BUILD}
  CurStatusBarStatistics.OnChange := V_SeqDataExplorer.UpdateStatusBar;  //make sure we don't call the update status bar before we initialize it
  {$ENDIF}

  CurAttrDisp := megNone;

  {$IFDEF VISUAL_BUILD}
    StatDispInXLItem := V_SeqDataExplorer.ActionStatDispInXL.Checked;
    StatDispInXlsItem := V_SeqDataExplorer.ActionStatDispInXls.Checked;
    StatDispInOdsItem := V_SeqDataExplorer.ActionStatDispInOds.Checked;
    StatDispInCSVItem := V_SeqDataExplorer.ActionStatDispInCSV.Checked;
    StatDispInTextItem := V_SeqDataExplorer.ActionStatDispInText.Checked;

    StatUseOnlyMarkedSitesItem := V_SeqDataExplorer.ActionHighlightedSites.Checked;
    StatAllSelSitesItem := V_SeqDataExplorer.ActionAllSelectedSites.Checked;

    DispTaxaNamesItem := V_SeqDataExplorer.ActionDispTaxaNames.Checked;
    DispGPNamesItem := V_SeqDataExplorer.ActionDispGroupNames.Checked;
    DispUseIdenSymbolItem := V_SeqDataExplorer.ActionUseIdenticalSymbol.Checked;
    DispSelTaxaItem := V_SeqDataExplorer.ActionShowOnlySelected.Checked;
    // Now do all kinds of default settings
    V_SeqDataExplorer.AdjustEnabledStatus;
  {$ELSE}
    StatDispInXLItem := True;
    StatDispInXlsItem := False;
    StatDispInOdsItem := False;
    StatDispInCSVItem := False;
    StatDispInTextItem := False;

    StatUseOnlyMarkedSitesItem := False;
    StatAllSelSitesItem := True;

    DispTaxaNamesItem := True;
    DispGPNamesItem := True;
    DispUseIdenSymbolItem := True;
    DispSelTaxaItem := False;
    DispResultsOnlyItem := False;
  {$ENDIF}
      // status bar is setup
  CurStatusBarStatistics.Strings[stCurSite] := '';



end;

procedure TVS_SeqDataExplorer.UpdateCodeTable;
begin
  D_InputSeqData.CodonInfo.CodeTable := D_InputSeqData.FCodeTable;
  D_InputSeqData.JobsPending := D_InputSeqData.JobsPending + [svUpdateAASeq, svUpdateDegeneracy];
  CurAttrDisp := megNone;
  if IsTranslated then
    D_InputSeqData.DoTranslation; // force it now
  {$IFDEF VISUAL_BUILD}
    {$IFNDEF CALTEST}
    V_SeqDataExplorer.HighlightSitesClick(nil);
    V_SeqDataExplorer.DataGrid.Invalidate;
    {$ENDIF CALTEST}
  {$ENDIF}
end;

// Checkes if the site is highlighted
function TVS_SeqDataExplorer.IsHighlighted(ASite: Integer): Boolean;
begin
  Result := CurAttrDisp in D_InputSeqData.SiteAttrArray[ASite];
end;

{$IFDEF VISUAL_BUILD}
procedure TVS_SeqDataExplorer.ApplyToVisual(visual: TV_SeqDataExplorer);
begin
  ApplyingToVisual := True;
  if (visual.ActionTranslate.Checked <> isTranslated) then
  begin
    visual.ActionTranslateExecute(nil);
  end;

  if (visual.ActionShowOnlySelected.Checked <> DispSelTaxaItem) then
    visual.ActionShowOnlySelectedExecute(nil);

  if (visual.ActionUseIdenticalSymbol.Checked <> DispUseIdenSymbolItem) then
    visual.ActionUseIdenticalSymbolExecute(nil);

  if (visual.ActionDispTaxaNames.Checked <> DispTaxaNamesItem) then
    visual.ActionDispTaxaNames.Execute;

  if (visual.ActionDispGroupNames.Checked <> DispGpNamesItem) then
    visual.ActionDispGroupNames.Execute;

  if (visual.ActionDispBirdsEyeView.Checked <> DispBirdsEyeViewItem) then
    visual.ActionDispBirdsEyeViewExecute(nil);

  if (visual.ActionColorCells.Checked <> DispColorItem) then
    visual.ActionColorCellsExecute(nil);

    ApplyingToVisual := False;

  if (visual.ActionHighlightConserved.Checked <> ConstantItem) then
    visual.ActionHighlightConservedExecute(nil);

  if (visual.ActionHighlightVariableSites.Checked <> VariableItem) then
    visual.ActionHighlightVariableSitesExecute(nil);

  if (visual.ActionHighlightLabelledSites.Checked <> LabelledItem) then
    visual.ActionHighlightLabelledSitesExecute(nil);

  if (visual.ActionHighlightParsimInfoSites.Checked <> ParsimInfoItem) then
    visual.ActionHighlightParsimInfoSitesExecute(nil);

  if (visual.ActionHighlightSingletonSites.Checked <> SingletonITem) then
    visual.ActionHighlightSingletonSitesExecute(nil);

  if (visual.ActionHighlight0FoldDegenSites.Checked <> ZeroFoldItem) then
    visual.ActionHighlight0FoldDegenSitesExecute(nil);

  if (visual.ActionHighlight2FoldDegenSites.Checked <> TwoFoldItem) then
    Visual.ActionHighlight2FoldDegenSitesExecute(nil);

  if (visual.ActionHighlight4FoldDegenSites.Checked <> FourFoldItem) then
    visual.ActionHighlight4FoldDegenSitesExecute(nil);

  if (visual.ActionHighlightedSites.Checked <> StatUseOnlyMarkedSitesItem) then
    visual.ActionHighlightedSitesExecute(nil);

  if (visual.ActionAllSelectedSites.Checked <> StatAllSelSitesItem) then
    visual.ActionAllSelectedSitesExecute(nil);

  if (visual.ActionStatDispInXl.Checked <> StatDispInXLItem) then
    visual.ActionStatDispInXlExecute(nil);

  if (visual.ActionStatDispInOds.Checked <> StatDispInOdsItem) then
    visual.ActionStatDispInOdsExecute(nil);

  if (visual.ActionStatDispInCSV.Checked <> StatDispInCSVItem) then
    visual.ActionStatDispInCsvExecute(nil);

  if (visual.ActionStatDispInText.Checked <> StatDispInTextItem) then
    visual.ActionStatDispInTextExecute(nil);

  if (visual.ActionHideMotifSearchResults.Checked <> HideSeqItem) then
    visual.ActionHideMotifSearchResultsExecute(nil);

  if CurFont <> nil then
  begin
    visual.DataGrid.Font := CurFont;
    visual.DataGrid.Canvas.Font := CurFont;
    visual.UpdateGridSizes;
    visual.UpdateFixedColWidth;
  end;

  // update the currently selected cell by row and column
  visual.UpdateRowColumn(CurRow, CurCol);
end;
{$ENDIF}

procedure TVS_SeqDataExplorer.SetConstant(const Value: Boolean);
begin
  if NOT ApplyingToVisual then
  begin
    ClearHighlightItems;
    Constant := Value;
  end;
end;

procedure TVS_SeqDataExplorer.SetFourFold(const Value: Boolean);
begin
  if NOT ApplyingToVisual then
  begin
    ClearHighlightItems;
    FourFold := Value;
  end;
end;

procedure TVS_SeqDataExplorer.SetParsimInfo(const Value: Boolean);
begin
  if NOT ApplyingToVisual then
  begin
    ClearHighlightItems;
    ParsimInfo := Value;
  end;
end;

procedure TVS_SeqDataExplorer.SetSingleton(const Value: Boolean);
begin
  if NOT ApplyingToVisual then
  begin
    ClearHighlightItems;
    Singleton := Value;
  end;
end;

procedure TVS_SeqDataExplorer.SetTwoFold(const Value: Boolean);
begin
  if NOT ApplyingToVisual then
  begin
    ClearHighlightItems;
    TwoFold := Value;
  end;
end;

procedure TVS_SeqDataExplorer.SetVarible(const Value: Boolean);
begin
  if NOT ApplyingToVisual then
  begin
    ClearHighlightItems;
    Variable := Value;
  end;
end;

procedure TVS_SeqDataExplorer.SetZeroFold(const Value: Boolean);
begin
  if NOT ApplyingToVisual then
  begin
    ClearHighlightItems;
    ZeroFold := Value;
  end;
end;

procedure TVS_SeqDataExplorer.SetSpecial(const Value: Boolean);
begin
  if NOT ApplyingToVisual then
  begin
    ClearHighlightItems;
    Special := Value;
  end;
end;

function TVS_SeqDataExplorer.GetTaxaGpsDlgOrder(Index: LongInt): Integer;
begin
  Result := -1;
  if FTaxaGpsDlgOrderList <> nil then
  begin
     if FTaxaGpsDlgOrderList.Count > index then
       Result := FTaxaGpsDlgOrderList.items[index]
     else
       Result := index;
    end;
end;

procedure TVS_SeqDataExplorer.SetLabelled(AValue: Boolean);
begin
  if not ApplyingToVisual then
  begin
    ClearHighlightItems;
    Labelled := AValue;
  end;
end;

procedure TVS_SeqDataExplorer.SetTaxaGpsDlgOrder(Index: LongInt; Value: LongInt
  );
begin
  while FTaxaGpsDlgOrderList.Count <= Index do
    FTaxaGpsDlgOrderList.Add(0);
  FTaxaGpsDlgOrderList[index] := Value;

end;

destructor TVS_SeqDataExplorer.Destroy;
begin
  CurStatusBarStatistics.Free;
  FTaxaGpsDlgOrderList.Free;
  if Assigned(CurFont) then
    CurFont.Free;
  inherited destroy;
end;

procedure TVS_SeqDataExplorer.RefreshDataGrid;
begin
{$IFDEF VISUAL_BUILD}
  V_SeqDataExplorer.Enabled := true;
  {$IFNDEF CALTEST}
  V_SeqDataExplorer.DataGrid.Invalidate;
  {$ENDIF CALTEST}
{$ENDIF}
end;

procedure TVS_SeqDataExplorer.NotifySearchCompletion(NumOfRes: Integer);
begin
{$IFDEF VISUAL_BUILD}
  V_SeqDataExplorer.NotifySearchCompletion(NumOfRes);
  {$ENDIF}
end;

procedure TVS_SeqDataExplorer.JumpTo(Top, Left: Integer);
begin
{$IFDEF VISUAL_BUILD}
  V_SeqDataExplorer.JumpTo(Top, Left);
  {$ENDIF}
end;

procedure TVS_SeqDataExplorer.ClearHighlightItems;
begin
  Constant:=False;
  Variable:=False;
  ParsimInfo:=False;
  Singleton:=False;
  ZeroFold:=False;
  TwoFold:=False;
  FourFold:=False;
  Special:=False;
end;

function TVS_SeqDataExplorer.IsXls: Boolean;
begin
  Result := (StatDispInXLItem or StatDispInXlsItem or StatDispInCSVItem or StatDispInOdsItem)
end;

function TVS_SeqDataExplorer.DisplaySpreadsheetExport: Boolean;
begin
  Result := (StatDispInXlsItem or StatDispInXlItem or StatDispInOdsItem or StatDispInCSVItem);
end;

end.
