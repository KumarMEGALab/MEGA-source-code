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

unit MDisplayMatrixDlg;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}
  uMegaBrowser, PrintersDlgs,
  {$ENDIF}
  mworkflow_element,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, Grids, ComCtrls, Buttons, ActnList, MLegendGenerator,
  MD_InputDistData, MegaConsts, MDistPack, ExcelWrite, MegaErrUtils,
  ErrorMessages_HC, MPleaseWait, MGlobalSettings, MOutputMatrixDlg,
  LCLType, LCLIntf, IniPropStorage, StdCtrls,
  mstringbuilder, Types;

type
  TMatrixExportOptions = (meMega, meCsv, meExcel, meText);
  TMatrixExportOptionsSet = Set of TMatrixExportOptions;

  { TPairwiseDist }

  TPairwiseDist = class(TObject)
    public
      TaxonA: Integer;
      TaxonB: Integer;
      Distance: Double;
      Name: String;
      constructor Create(aTaxonA: Integer; aTaxonB: Integer; aDist: Double; aName: String);
  end;

  TVS_DisplayMatrix = Class;
  {$IFDEF VISUAL_BUILD}
   TDisplayMatrixDlg = Class;
  {$ENDIF}

   { TD_DisplayMatrix }

   TD_DisplayMatrix = class(TPersistent)
     Constructor Create;
   private
     FAnalysisSummary: TStringList;
     function WriteColumnarData(ExportOptions : TDistExportOptions; MyD : PDistanceMatrix; MySe : PDistanceMatrix; RowNames : TStringList):TStringList;
     function WritePairwiseData(ExportOptions : TDistExportOptions; MyD : PDistanceMatrix; MySe : PDistancematrix; RowNames : TStringList):TStringList;
     function GetMaxRowNameLen(RowNames : TStringList): LongInt;

   public
     FigureGenerator : TLegendGenerator;

     procedure LinkVisualStatusClass(vs: TVS_DisplayMatrix);
     procedure CreateInvalidDistanceSummary(
       OffenseCount: ArrayOfInteger; var Result : TStringList);
     procedure MoveRow(FromIndex, ToIndex : Integer);
     function FindInCurSet(aId: Integer): Integer;
     function CellContent(ARow, ACol: Integer; var InvalidNum, IsStdErrDisp: Boolean): Double;
     function  Get1xNDistToDisplay(x,y: Integer; var InvalidNum: Boolean): Extended; // always gets x < y
     function WriteExportToFile(ExportOptions : TDistExportOptions):TStringList;
     destructor Destroy; override;

     public
       CurSet: PArrayOfInt; // Current ordering of taxa being displayed
       FAllowNeg :Boolean;
       FComputationType: TDistType;
       FDAcronym:   String;
       FDescription: TStringList;
       FDistArray:    ArrayOfDouble;
       FDistMat:   PDistanceMatrix;
       FFullName:  String;
       FNoOfRows: Integer;
       FNoOfCols: Integer;
       FGpNames: TStringList;
       FSpNames: TStringList;
       FPopNames: TStringList;
       FHasTestResult: Boolean;
       FIsSelTest : Boolean;
       FNoOfGps: Integer;
       FNoOfTaxa: Integer;
       FOnlyDistance: Boolean;
       FStdErrArray:  ArrayOfDouble;
       FStdErrMat: PDistanceMatrix;
       FTaxonNames: TStringList;
       FTitle: String;
       FVAcronym:   String;
       IsLowerLeft: Boolean;
       MaxD: Double;
       MinD: Double;
       FMeanStdErr: Double;
       VS_DisplayMatrix : TVS_DisplayMatrix;
     public
       procedure CreateListOfInvalidDistances(var MoreResults : Boolean; var StartAt : Integer; var Offenses : ArrayOfInteger; var Result : TStringList);
       function Get1xNStdErrToDisplay(x, y: Integer;
         var InvalidNum: Boolean): Extended;
       function getAvgAll: Double;
       function GetColName(Index: Integer): String;
       function GetGpName(Index: Integer): String;
       function GetSpName(Index: Integer): String;
       function GetPopName(Index: Integer): String;
       procedure getGpsAvg(MyComputeType : TDistType; var MyGpNameList : TStringList; var MyD : PDistanceMatrix; var MyDComps : PDistanceMatrix;var  MyMeanGpD : ArrayOfDouble);
       function GetHasInvalidData: Boolean;
       function GetNoOfCols: Integer;
       function GetNoOfRows: Integer;
       function GetNoOfTaxa: Integer;
       function GetNxNDistToDisplay(x, y: Integer;
         var InvalidNum: Boolean): Extended;
       function GetNxNStdErrToDisplay(x, y: Integer;
         var InvalidNum: Boolean): Extended;
       function GetRowGpName(Index: Integer): String;
       function GetRowName(Index: Integer): String;
       function GetTaxonName(Index: Integer): String;
       procedure Initialize;
       procedure SetDescription(AValue: TStringList);
       procedure SetGpName(Index: Integer; Value: String);
       procedure SetSpName(Index: Integer; Value: String);
       procedure SetPopName(Index: Integer; Value: String);
       procedure SetNoOfGps(Value: Integer);
       procedure SetNoOfTaxa(Value: Integer);
       procedure SetTaxonName(Index: Integer; const Value: String);
       procedure SortItem;

     published
       property isDataLowerLeft : Boolean read IsLowerLeft;
       property HasInvalidData : Boolean read GetHasInvalidData;

     Public
       property AllowNeg :Boolean read FAllowNeg write FAllowNeg;
       property ComputationType: TDistType read FComputationType write FComputationType; // this fixes matrix type

       property NoOfRows: Integer read GetNoOfRows;
       property NoOfCols: Integer read GetNoOfCols;

       property RowName[Index: Integer]:String read GetRowName;
       property RowGpName[Index: Integer]:String read GetRowGpName;
       property ColName[Index: Integer]:String read GetColName;
       property NoOfTaxa: Integer read GetNoOfTaxa write SetNoOfTaxa;
       property TaxonName[Index: Integer]: String read GetTaxonName write SetTaxonName;

       property NoOfGps: Integer read FNoOfGps write SetNoOfGps;
       property GpName[Index: Integer]: String read GetGpName write SetGpName;
       property SpName[Index: Integer]: String read GetSpName write SetSpName;
       property PopName[Index: Integer]: String read GetPopName write SetPopName;

       property Title: String read FTitle write FTitle;
       property Description: TStringList write SetDescription;

       property HasTestResult: Boolean read FHasTestResult write FHasTestResult;
       property OnlyDistance: Boolean read FOnlyDistance write FOnlyDistance;

       property DAcronym: String read FDAcronym write FDAcronym;
       property VAcronym: String read FVAcronym write FVAcronym;

       property FullName: String read FFullName write FFullName;
         // n x n matrices
       property DistMat: PDistanceMatrix read FDistMat write FDistMat;
       property StdErrMat: PDistanceMatrix read FStdErrMat write FStdErrMat;
         // 1 x n vectors
       property DistArray:    ArrayOfDouble  read FDistArray write FDistArray;
       property StdErrArray:  ArrayOfDouble  read FStdErrArray write FStdErrArray;
         // point variables
      // property MeanDist:   Double  write FMeanDist;
      property MeanStdErr: Double  read FMeanStdErr write FMeanStdErr;

      property IsSelTest: Boolean read FIsSelTest write FIsSelTest;
      property AnalysisSummary: TStringList read FAnalysisSummary write FAnalysisSummary;
   end;

   { TVS_DisplayMatrix }

   TVS_DisplayMatrix = class(TObject)

     FPrecision: Integer;
     FIsResizing: Boolean;
     LeftOfDecimalLen: Integer;     // x.y ; length of x
     MaxRowNamePixelLen: Integer; // maximum # of pixels for names
     ShouldClose: Boolean;          // I guess it is used for something
     IsFormInit : Boolean;          // makes sure that the active initializes only once
     CurStatusBar : TStringList;


     Constructor Create;
     Destructor Destroy; override;

     procedure DecPrecision;
     property Precision: Integer read FPrecision write FPrecision;
     procedure IncPrecision;
   end;

   {$IFDEF VISUAL_BUILD}
  { TDisplayMatrixDlg }

  TDisplayMatrixDlg = class(TForm, IObservedForm)
    ActionSortByGroup: TAction;
    ActionSortAlphabetical: TAction;
    ActionMoveToBottom: TAction;
    ActionMoveToTop: TAction;
    ActionSortByDistToFocalOtu: TAction;
    ActionRestoreInputOrder: TAction;
    DataGrid: TDrawGrid;
    FindFirstAction: TAction;
    FindNextAction: TAction;
    FIndPreviousAction: TAction;
    GridHorizScrollbar: TScrollBar;
    GridPanel: TPanel;
    GridVertScrollbar: TScrollBar;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    SortByDistItem: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    ActionToggleSearchTools: TAction;
    HideSearchToolsAction: TAction;
    Button1: TButton;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    NamesColumnPopupMenu: TPopupMenu;
    SearchEdit: TEdit;
    Label1: TLabel;
    MenuItem2: TMenuItem;
    SearchPanel: TPanel;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    WindowsMenuItem: TMenuItem;
    ShowAnalysisSummaryAction: TAction;
    IniPropStorage1: TIniPropStorage;
    MenuItem1: TMenuItem;
    ShowCaptionAction: TAction;
    ExportTextAction: TAction;
    ExportToMegaAction: TAction;
    ExportToCsvAction: TAction;
    ExportExcelAction: TAction;
    ExportDataAction: TAction;
    DecPrecisionAction: TAction;
    IncPrecisionAction: TAction;
    ShowPairNamesAction: TAction;
    ShowPairNameBtn: TToolButton;
    DecPrecisionBtn: TToolButton;
    IncPrecisionBtn: TToolButton;
    ExportDataBtn: TToolButton;
    ExportXlBtn: TToolButton;
    ExportCsvBtn: TToolButton;
    ExportMegaBtn: TToolButton;
    ExportTxtBtn: TToolButton;
    CaptionBtn: TToolButton;
    UpperRightAction: TAction;
    LowerLeftAction: TAction;
    ActionList1: TActionList;
    FontDialog: TFontDialog;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    DisplayMenu: TMenuItem;
    FontItem: TMenuItem;
    AverageMenu: TMenuItem;
    HelpMenu: TMenuItem;
    miGenerateCaption: TMenuItem;
    AvgAllItem: TMenuItem;
    BetweenGpsAvgItem: TMenuItem;
    UpperRightBtn: TToolButton;
    LowerLeftBtn: TToolButton;
    WeightedAvgItem: TMenuItem;
    NetBetweenGpsAvgItem: TMenuItem;
    WithinGpsAvgItem: TMenuItem;
    N2: TMenuItem;
    N6: TMenuItem;
    ShowGpNamesItem: TMenuItem;
    ShowTaxaNamesItem: TMenuItem;
    N1: TMenuItem;
    SortGpItem: TMenuItem;
    SortByNameItem: TMenuItem;
    SortOriginalItem: TMenuItem;
    SortSeqItem: TMenuItem;
    N7: TMenuItem;
    ShowPairNameItem: TMenuItem;
    QuitItem: TMenuItem;
    N5: TMenuItem;
    ShowListofInvalidDistancesItem: TMenuItem;
    N4: TMenuItem;
    SaveToFileItem: TMenuItem;
    N3: TMenuItem;
    ShowDescriptionItem: TMenuItem;
    ShowTitleItem: TMenuItem;
    Panel1: TPanel;

    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    ToolButton1: TToolButton;
    ToolButton6: TToolButton;
    ToolButton3: TToolButton;
    MainToolBar: TToolBar;
    {$IFDEF VISUAL_BUILD}
    PrinterSetupDialog: TPrinterSetupDialog;
    procedure ActionMoveToBottomExecute(Sender: TObject);
    procedure ActionMoveToTopExecute(Sender: TObject);
    procedure ActionRestoreInputOrderExecute(Sender: TObject);
    procedure ActionSortAlphabeticalExecute(Sender: TObject);
    procedure ActionSortByDistToFocalOtuExecute(Sender: TObject);
    procedure ActionSortByGroupExecute(Sender: TObject);
    procedure DataGridClick(Sender: TObject);
    procedure DataGridColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure DataGridContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure DataGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure FindFirstActionExecute(Sender: TObject);
    procedure FindNextActionExecute(Sender: TObject);
    procedure FIndPreviousActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure DataGridDrawCell(Sender: TObject; ACol, ARow: Longint; aRect: TRect; State: TGridDrawState);
    function FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean
      ): Boolean;
    procedure FormResize(Sender: TObject);
    procedure HideSearchToolsActionExecute(Sender: TObject);
    procedure MainMenu1DrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure MainMenu1MeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
    procedure GridHorizScrollbarChange(Sender: TObject);
    procedure GridHorizScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure GridVertScrollbarChange(Sender: TObject);
    procedure GridVertScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure SearchEditChange(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ShowAnalysisSummaryActionExecute(Sender: TObject);

    procedure SortItemClick(Sender: TObject);
    procedure FormatSBtnClick(Sender: TObject);
    procedure SaveToFileItemClick(Sender: TObject);
    procedure FontItemClick(Sender: TObject);
    procedure QuitItemClick(Sender: TObject);
    procedure ShowDescriptionItemClick(Sender: TObject);
    procedure ShowTitleItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ShowPairNameItemClick(Sender: TObject);
    procedure DataGridRowMoved(Sender: TObject; FromIndex,ToIndex: Longint);
    procedure ShowGpNamesItemClick(Sender: TObject);

    procedure AvgAllItemClick(Sender: TObject);
    procedure GpsAvgItemClick(Sender: TObject);
    procedure DecPrecisionBtnClick(Sender: TObject);
    procedure IncPrecisionBtnClick(Sender: TObject);
    procedure ShowTaxaNamesItemClick(Sender: TObject);
    procedure HelpMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miGenerateCaptionClick(Sender: TObject);
    procedure DataGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ShowListofInvalidDistancesItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ActionToggleSearchToolsExecute(Sender: TObject);
    {$ENDIF}
  private
    FMouseGridCoords: TPoint;
    FSearchResult: Integer;
    FPrecision: Integer;
    FScrollStarted: Boolean;
    FSelfScrolling: Boolean;
    FVScrollPos: Integer;
    FHScrollPos: Integer;
    FLastRow: Integer;
    FFontSize: Integer;
    ExportFormatOptions: TMatrixExportOptionsSet;
    disallowMatrixExport: Boolean;
    VS_DisplayMatrix : TVS_DisplayMatrix;
    D_DisplayMatrix : TD_DisplayMatrix;
    {$IFDEF VISUAL_BUILD}
    procedure DataToVisualStatusBar(Sender : TObject);
    procedure UpdateStatusBar(ARow, ACol: Integer);
    procedure UpdateColumnWidths(FirstCol: Boolean; OtherCol: Boolean);
    function  ConstructCol0RowName(Index: LongInt; addRowNumber: Boolean = True): String;
    procedure InitScrollbars(setPosition: Boolean);
    function RowVisible(aRow: Integer): Boolean;
    function ColVisible(aCol: Integer): Boolean;
    function ValidSelectableRow(aRow: Integer): Integer;
    function ValidSelectableCol(aCol: Integer): Integer;
    procedure UnhighlightSearchResult;
    function FindTaxon(searchQuery: String; startRow: Integer; searchForward: Boolean): Integer;
    function SearchLabels(searchQuery: String; startIndex: Integer; searchForward: Boolean): Integer;
    {$ENDIF}
  private
    FStrBuilder: TMegaStringBuilder;
    DispStr: String;
    OutputMatrixDlg: TOutputMatrixDlg;
    {$IFDEF VISUAL_BUILD}
    FFormObservers: TFormObserverList;
    procedure AssignHelpContext;
    procedure ComputeLeftOfDecimalLen;
    function GetAnalysisSummary: TStringList;
    procedure SetAnalysisSummary(AValue: TStringList);
    procedure SetBestPrecisionForDisplay;
  public
    function NumTaxa: Integer;
    function NumGroups: Integer;
    procedure LinkDataClass(DispDlg: TD_DisplayMatrix);
    procedure LinkVisualStatusClass(DispDlgVS: TVS_DisplayMatrix);
    procedure SetExportOptions(Options: TMatrixExportOptionsSet);
    procedure setDisallowMatrix(value: Boolean);
    procedure AddFormObserver(observer: IFormObserver);
    property AnalysisSummary: TStringList read GetAnalysisSummary write SetAnalysisSummary;
    {$ENDIF}
  end;

var
  DisplayMatrixDlg: TDisplayMatrixDlg;
  {$ENDIF}

  function CompareGroups(List: TStringList; Index1, Index2: Integer): Integer;
  function ComparePairwiseDists(Item1: Pointer; Item2: Pointer): Integer;

implementation

{$R *.lfm}

uses
  Math, MegaUtils,
  {$IFDEF VISUAL_BUILD}
  Printers, MEditorForm, MMegaWindowInfo, Mega_Main, htmloptionsdlg, mimageform,
  {$ELSE}
  MD_MegaMain,
  {$ENDIF}
  DataExplorerHelp_HC, MegaVerConsts,
  mhelpfiles, mhelpkeywords, MegaPrivateFiles;


var
  PrevPairNameTextVisible: Boolean = False;
  PrevShowGpNames: Boolean = False;

{ TDisplayMatrixDlg }

function CompareGroups(List: TStringList; Index1, Index2: Integer): Integer;
var
  d1, d2: String;
begin
  d1 := List.Values[List.Names[Index1]];
  d2 := List.Values[List.Names[Index2]];
  Result := AnsiCompareText(d1, d2);
end;

function ComparePairwiseDists(Item1: Pointer; Item2: Pointer): Integer;
var
  d1: TPairwiseDist = nil;
  d2: TPairwiseDist = nil;
begin
  d1 := TPairwiseDist(Item1);
  d2 := TPairwiseDist(Item2);
  Result := CompareValue(d1.Distance, d2.Distance, FP_CUTOFF);
  if Result = 0 then
    Result := CompareText(d1.Name, d2.Name);
end;

{ TPairwiseDist }

constructor TPairwiseDist.Create(aTaxonA: Integer; aTaxonB: Integer; aDist: Double; aName: String);
begin
  TaxonA := aTaxonA;
  TaxonB := aTaxonB;
  Distance := aDist;
  Name := aName;
end;

constructor TD_DisplayMatrix.Create;
begin
  FAnalysisSummary := nil;
  FNoOfTaxa := 0;
  FNoOfGps := 0;
  FTaxonNames  := nil;
  FHasTestResult:= False;
  FOnlyDistance := True;
  FDescription := TSTringList.Create;
  FDistMat     := nil;
  FStdErrMat   := nil;
  FDistArray   := nil;
  FIsSelTest := False;
  FStdErrArray := nil;
  FDAcronym := EmptyStr;
  FVAcronym := EmptyStr;
  CurSet := nil;
  FTaxonNames   := TStringList.Create;
  FGpNames      := TStringList.Create;
  FigureGenerator := TLegendGenerator.Create;
end;

procedure TD_DisplayMatrix.Initialize;
var
  i, j : Integer;
begin
    case FComputationType of
      gdPairwise:
        begin
          GetMem(CurSet, sizeOf(Integer)*FNoOfTaxa);
          for i:= 0 to FNoOfTaxa-1 do
            CurSet[i] := i;
          FOnlyDistance := (FStdErrMat = nil);
        end;
      gdBetweenGroupMean,
      gdNetGroupMean:
        begin
          GetMem(CurSet, sizeOf(Integer)*FNoOfGps);
          for i:= 0 to FNoOfGps-1 do
            CurSet[i] := i;
          FOnlyDistance := (FStdErrMat = nil);
          FAllowNeg := True;
        end;
      gdWithinGroupMean,
      gdOverallMean:   // overall mean is treated like within gp
        begin
          GetMem(CurSet, sizeOf(Integer)*FNoOfGps);
          for i:= 0 to FNoOfGps-1 do
            CurSet[i] := i;
          FOnlyDistance := (FStdErrArray = nil);
        end;
     end;

    case FComputationType of
        gdPairwise:
          begin
            FNoOfRows := FNoOfTaxa;  // always the number of Taxa
            FNoOfCols := FNoOfTaxa;
          end;
        gdBetweenGroupMean,
        gdNetGroupMean:
          begin
            FNoOfRows := FNoOfGps;  // always the number of Gps
            FNoOfCols := FNoOfGps;
          end;
        gdWithinGroupMean,
        gdOverallMean:   // overall mean is treated like within gp
          begin
            FNoOfRows := FNoOfGps;  // always the number of Gps
            if FOnlyDistance then FNoOfCols := 1
                             else FNoOfCols := 2;
          end;
        else
          RaiseErrorMessage(HC_Unexpected_Error, 'Unable to determine the amount of rows and columns that should be shown');
    end;

         // no negative values for prob
    if FHasTestResult then
    begin
      //FAllowNeg := False;
      // otherwise FAllowNeg is always false and output for Sel Tests is incorrect (all '?'): Joel
      FAllowNeg := True //=FIsSelTest; // Change in M3.2 by SK
    end;

    maxD := 0;
    minD := MaxInt;
  case FComputationType of
    gdPairwise,
    gdBetweenGroupMean,
    gdNetGroupMean:
      for i:= 1 to NoOfRows-1 do
        for j:=0 to i-1 do
        begin
          if (FDistMat[i][j] < 0) and (not FAllowNeg) then
           Continue;
          if abs(FDistMat[i][j]) > maxD then
            maxD := abs(FDistMat[i][j]);
          if abs(FDistMat[i][j]) < minD then
            minD := abs(FDistMat[i][j]);
        end;
    gdWithinGroupMean,
    gdOverallMean:
      for i:= 0 to NoOfRows-1 do
      begin
        if (FDistArray[i] < 0) and (not FAllowNeg) then
          Continue;
        if abs(FDistArray[i]) > maxD then
          maxD := abs(FDistArray[i]);
      end;
  end;
    IsLowerLeft := True;
end;

procedure TD_DisplayMatrix.MoveRow(FromIndex, ToIndex : Integer);
var
  TempOtu, i: Integer;
begin
  if FromIndex = ToIndex then Exit;
  FromIndex := FromIndex -1;
  ToIndex   := ToIndex -1;

  TempOtu := CurSet[FromIndex];
  if FromIndex < ToIndex then // move down
  begin
    for i:= FromIndex to ToIndex-1 do
      CurSet[i] := CurSet[i+1];
    CurSet[ToIndex]:= TempOtu;
  end
  else if FromIndex > ToIndex then // Move up
  begin
    for i := FromIndex downto ToIndex+1 do
      CurSet[i] := CurSet[i-1];
    CurSet[ToIndex]:= TempOtu;
  end;
end;

function TD_DisplayMatrix.FindInCurSet(aId: Integer): Integer;
var
  i: Integer = -1;
begin
  Result := -1;
  if NoOfTaxa > 0 then
  begin
    for i := 0 to NoOfTaxa - 1 do
    begin
      if CurSet[i] = aId then
        Exit(i);
    end;
  end
  else if NoOfGps > 0 then
    for i := 0 to NoOfGps - 1 do
    begin
      if CurSet[i] = aId then
        Exit(i);
    end;
end;

function TD_DisplayMatrix.getAvgAll:Double;
var
  TheSum, TheDist: Double;
  Comps, i, j: Integer;
  InvalidNum: Boolean = False;
  Bogus: Boolean = False;
begin
  TheSum := 0;
  Comps := 0;

  // Only called for Taxon-pairwise comparisons
  for i:=1 to NoOfRows-1 do
    for j:= 0 to i-1 do
    begin
      InvalidNum := True;
      if IsLowerLeft then
        TheDist := CellContent(i+1,j+1, InvalidNum, Bogus)  // calls like TDrawGrid
      else
        TheDist := CellContent(j+1,i+1, InvalidNum, Bogus);
      if not InvalidNum then
      begin
        TheSum := TheSum + (TheDist/MaxD);
        Inc(Comps);
      end;
    end;
  Result := (TheSum/Comps)*MaxD;
end;


destructor TD_DisplayMatrix.Destroy;
begin
  if Assigned(FDescription) then
    FDescription.Free;
  if Assigned(FAnalysisSummary) then
    FAnalysisSummary.Free;
  FreeAndNil(FTaxonNames);
  FreeAndNil(FGpNames);
  FreeMemAndNil(CurSet);

  case FComputationType of
    gdPairwise:
      begin
        if FDistMat   <> nil then  FreeDistMatrix(FDistMat, FNoOfTaxa); FDistMat := nil;
        if FStdErrMat <> nil then  FreeDistMatrix(FStdErrMat, FNoOfTaxa); FStdErrMat := nil;
      end;
    gdBetweenGroupMean,
    gdNetGroupMean:
      begin
        if FDistMat   <> nil then  FreeDistMatrix(FDistMat, FNoOfGps); FDistMat := nil;
        if FStdErrMat <> nil then  FreeDistMatrix(FStdErrMat, FNoOfGps); FStdErrMat := nil;
      end;
    gdWithinGroupMean,
    gdOverallMean:
      begin
        FDistArray    := nil;
        FStdErrArray  := nil;
      end;
  end;
  if FigureGenerator <> nil then FigureGenerator.Free;

end;

procedure TD_DisplayMatrix.LinkVisualStatusClass(vs : TVS_DisplayMatrix);
begin
  VS_DisplayMatrix := vs;
end;

procedure TD_DisplayMatrix.getGpsAvg(MyComputeType : TDistType; var MyGpNameList : TStringList; var  MyD : PDistanceMatrix; var  MyDComps : PDistanceMatrix; var  MyMeanGpD : ArrayOfDouble);
var
  i, j : integer;
  MyMeanGpDComps : ArrayOfDouble; // within gp means
  d: Double;
  GpI, GpJ, Temp: Integer;
  InvalidNum: Boolean = False;
  Bogus: Boolean = False;
begin
    if (MyComputeType = gdBetweenGroupMean) or (MyComputeType = gdNetGroupMean) then
    begin
      MyD      := NewDistMatrix(FNoOfGps, False);
      MyDComps := newDistMatrix(FNoOfGps, False);
      for i:= 1 to FNoOfGps-1 do
        for j:=0 to i-1 do
        begin
          MyD[i][j]      := 0;
          MyDComps[i][j] := 0;
        end;
    end;

    if (MyComputeType = gdWithinGroupMean) or (MyComputeType = gdNetGroupMean) then
    begin
      SetLength(MyMeanGpD, FNoOfGps);
      SetLength(MyMeanGpDComps, FNoOfGps);
      for i:=0 to FNoOfGps-1 do
      begin
        MyMeanGpD[i] := 0;
        MyMeanGpDComps[i] := 0;
      end;
    end;

    // now addup stuff
    for i:= 1 to NoOfRows-1 do
      for j:= 0 to i-1 do
      begin
        if Length(RowGpName[i]) < 1 then
          Break;
        if Length(RowGpName[j]) < 1 then // go to next one
          Continue;
        GpJ := MyGpNameList.IndexOf(RowGpName[j]);
        if GpJ < 0 then
        begin
          MyGpNameList.Add(RowGpName[j]);
          GpJ := MyGpNameList.IndexOf(RowGpName[j]);
        end;
        GpI := MyGpNameList.IndexOf(RowGpName[i]);
        if GpI < 0 then
        begin
          MyGpNameList.Add(RowGpName[i]);
          GpI := MyGpNameList.IndexOf(RowGpName[i]);
        end;

        // make GpI > GpJ
        if GpI < GpJ then
        begin
          Temp := GpJ; GpJ := GpI; GpI := Temp;
        end;
        // update between group stuff
        if (GpI <> GpJ) and ((MyComputeType = gdBetweenGroupMean) or (MyComputeType = gdNetGroupMean)) then
        begin
          if IsLowerLeft then  // to make sure we get the right values
            d := CellContent(i+1, j+1, InvalidNum, Bogus)
          else
            d := CellContent(j+1, i+1, InvalidNum, Bogus);

          if not InvalidNum then
          begin
           { if Sender <> WeightedAvgItem then //This was conflicting with the real gdBetweenGroupMean and would need a LOT of work to get it workinng and no-one uses it
            begin  }
              MyD[GpI][GpJ] := MyD[GpI][GpJ] + d;
              MyDComps[GpI][GpJ] := MyDComps[GpI][GpJ] + 1;
            {end
            else // special
            begin
              if IsLowerLeft then  // to make sure we get the right values
                n := CellContent(j+1, i+1, InvalidNum, Bogus)
             else
                n := CellContent(i+1, j+1, InvalidNum, Bogus);
              MyD[GpI][GpJ] := MyD[GpI][GpJ] + n*d;
              MyDComps[GpI][GpJ] := MyDComps[GpI][GpJ] + n;
            end;  }
          end;
        end
        else if (GpI = GpJ) and ((MyComputeType = gdWithinGroupMean) or (MyComputeType = gdNetGroupMean)) then
        begin
          if IsLowerLeft then  // to make sure we get the right values
            d := CellContent(i+1, j+1, InvalidNum, Bogus)
          else
            d := CellContent(j+1, i+1, InvalidNum, Bogus);
          if not InvalidNum then
          begin
            MyMeanGpD[GpI] := MyMeanGpD[GpI] + d;
            MyMeanGpDComps[GpI] := MyMeanGpDComps[GpI] + 1
          end;
        end;
      end;// end for all i and j

    // now we have to derive the estimates
    if (MyComputeType = gdBetweenGroupMean) or (MyComputeType = gdNetGroupMean) then
    begin
      for GpI := 1 to FNoOfGps-1 do
        for GpJ := 0 to GpI-1 do
          if MyDComps[GpI][GpJ] >= 1 then
            MyD[GpI][GpJ] := MyD[GpI][GpJ]/MyDComps[GpI][GpJ]
          else
            MyD[GpI][GpJ] := InvalidDistValue;
    end;
    if (MyComputeType = gdWithinGroupMean) or (MyComputeType = gdNetGroupMean) then
    begin
      for GpI := 0 to FNoOfGps-1 do
        if MyMeanGpDComps[GpI] >= 1 then
          MyMeanGpD[GpI] := MyMeanGpD[GpI]/MyMeanGpDComps[GpI]
        else
          MyMeanGpD[GpI] := InvalidDistValue;
    end;
    // Now derive estimate for the net group mean
    if (MyComputeType = gdNetGroupMean) then
    begin
      for GpI := 1 to FNoOfGps-1 do
        for GpJ := 0 to GpI-1 do
          if (MyDComps[GpI][GpJ] >= 0) and
             (MyMeanGpD[GpI] > 0) and (MyMeanGpD[GpJ] > 0) then
            MyD[GpI][GpJ] := MyD[GpI][GpJ] - (MyMeanGpD[GpI] + MyMeanGpD[GpJ])/2
          else
            MyD[GpI][GpJ] := InvalidDistValue;
    end;
end;

procedure TD_DisplayMatrix.CreateListOfInvalidDistances(var MoreResults : Boolean; var StartAt : Integer; var Offenses : ArrayOfInteger; var Result : TStringList);
var
  AStrList: TStringList;
  i, j, Limit: Integer;
  InvalidNum: Boolean = False;
  Bogus: Boolean = False;
  IsDone: Boolean = False;
  OffenseCount: ArrayOfInteger;
begin
  AStrList := nil;
  try try
    AStrList := TStringList.Create;
    SetLength(OffenseCount, NoOfTaxa);
    SetLength(Offenses, NoOfTaxa);

    if StartAt = 1 then
      For i := 0 to NoOfTaxa-1 do
        OffenseCount[i] := 0;


    IsDone := False;
    Limit := 99;
    for i:=StartAt to NoOfRows-1 do
    begin
      for j:= 0 to i-1 do
      begin
        if IsLowerLeft then
          CellContent(i+1,j+1, InvalidNum, Bogus)  // calls like TDrawGrid
        else
          CellContent(j+1,i+1, InvalidNum, Bogus);

        if InvalidNum then
        begin
          AStrList.Add(IntToStr(i+1)+','+IntToStr(j+1));
          Inc(OffenseCount[j]);
          Inc(OffenseCount[i]);
          if AStrList.Count > Limit then
          begin
              StartAt := i+1;
              IsDone := True;
              break;
          end;
        end;
      end;
      if IsDone then
        break;
    end;

      Result.text := Result.text + AStrList.text;

      for i := 0 to NoOfTaxa-1 do    //add the list of offenses to the previous list
      begin
        Offenses[i] := OffenseCount[i] + Offenses[i];
      end;

    MoreResults := IsDone;
  except
    FreeAndNil(AStrList);
    OffenseCount := nil;
  end
  finally
    FreeAndNil(AStrList);
    OffenseCount := nil;
  end;
end;

procedure TD_DisplayMatrix.CreateInvalidDistanceSummary(OffenseCount : ArrayOfInteger; var Result : TStringList);
var
  i : Integer;
begin
     for i :=(Length(OffenseCount)-1) downto 0 do
        if OffenseCount[i] > 0 then
          Result.Insert(0, IntToStr(OffenseCount[i])+' invalid distances for ['+ IntToStr(i+1)+'] '+ TaxonName[i]);
end;

procedure TD_DisplayMatrix.SortItem;
var
  i: Integer;
begin
  For i:=0 to NoOfRows - 1 do  // simple resetting
    CurSet[i] := i;
end;

function TD_DisplayMatrix.CellContent(ARow, ACol: Integer; var InvalidNum, IsStdErrDisp: Boolean): Double;
var
  TheRow, TheCol: Integer;
begin

  Result := InvalidDistValue; // -214748364, which is the lower limit of the Long type

  InvalidNum := False;

  IsStdErrDisp := False;
  case FComputationType of
    gdPairwise,
    gdBetweenGroupMean,
    gdNetGroupMean:
      begin
        if ARow = ACol then Exit;
        IsStdErrDisp := (IsLowerLeft and (ARow < ACol)) or
                        (not IsLowerLeft and (ARow > ACol));
        if CurSet[ARow-1] > CurSet[ACol-1] then
          begin TheRow := CurSet[ARow-1]; TheCol := CurSet[ACol-1]; end
        else
          begin TheRow := CurSet[ACol-1]; TheCol := CurSet[ARow-1]; end;

        if IsStdErrDisp then
          Result := GetNxNStdErrToDisplay(TheRow, TheCol,InvalidNum)
        else
          Result := GetNxNDistToDisplay(TheRow,  TheCol, InvalidNum);
      end;
    gdWithinGroupMean, gdOverallMean:
      case (ACol-1) of
        0:  Result := Get1xNDistToDisplay(CurSet[ARow-1],     0, InvalidNum);
        1:
            begin
              Result := Get1xNStdErrToDisplay(CurSet[ARow-1], 0, InvalidNum);
              IsStdErrDisp := True;
            end;
        2:  Result := Get1xNDistToDisplay(CurSet[ARow-1],   1, InvalidNum);
        3:  begin
              Result := Get1xNStdErrToDisplay(CurSet[ARow-1], 1, InvalidNum);
              IsStdErrDisp := True;
            end;
      end;
  end;
end;


function TD_DisplayMatrix.Get1xNDistToDisplay(x,y: Integer; var InvalidNum: Boolean): Extended; // always gets x < y
begin
  InvalidNum := False;
  case FComputationType of
    gdWithinGroupMean,
    gdOverallMean:    Result := FDistArray[x];
  else
    Result := InvalidDistValue;
  end;
  InvalidNum := Result <= InvalidDistValue;
end;



function TD_DisplayMatrix.Get1xNStdErrToDisplay(x,y: Integer; var InvalidNum: Boolean): Extended;
begin
  Result := InvalidDistValue;
  InvalidNum := False;
  case FComputationType of
    gdWithinGroupMean,
    gdOverallMean:    Result := FStdErrArray[x];
  end;
  InvalidNum := Result <= InvalidDistValue;
end;


function TD_DisplayMatrix.GetColName(Index: Integer):String;
begin
  Result := EmptyStr;
  case FComputationType of
    gdPairwise,
    gdBetweenGroupMean,
    gdNetGroupMean:       Result := IntToStr(Index+1);
    gdWithinGroupMean,
    gdOverallMean:
      case Index of
        0:  Result := FDAcronym;
        1:  Result := FVAcronym;
      end;
  end;
end;


function TD_DisplayMatrix.GetGpName(Index: Integer): String;
begin
  Result := FGpNames[Index];
end;

function TD_DisplayMatrix.GetSpName(Index: Integer): String;
begin
  Result := FSpNames[Index];
end;

function TD_DisplayMatrix.GetPopName(Index: Integer): String;
begin
  Result := FPopNames[Index];
end;

function TD_DisplayMatrix.GetHasInvalidData : Boolean;
var
  i, j : Integer;
begin
  Result := False;
  case FComputationType of
    gdPairwise,
    gdBetweenGroupMean,
    gdNetGroupMean:
      for i:= 1 to NoOfRows-1 do
        for j:=0 to i-1 do
        begin
          if FDistMat[i][j] <= InvalidDistValue then
            Result := True;
        end;
    gdWithinGroupMean,
    gdOverallMean:
      for i:= 0 to NoOfRows-1 do
      begin
        if FDistArray[i] <= InvalidDistValue then
          Result := True;
      end;
  end;
end;


function TD_DisplayMatrix.GetNoOfCols: Integer;
begin
  Result := FNoOfCols;
end;

function TD_DisplayMatrix.GetNoOfRows: Integer;
begin
  Result := FNoOfRows;
end;

function TD_DisplayMatrix.GetNoOfTaxa: Integer;
begin
  Result := FNoOfTaxa;
end;


function TD_DisplayMatrix.GetNxNDistToDisplay(x,y: Integer; var InvalidNum: Boolean): Extended; // always gets x < y
begin
  InvalidNum := False;
  Result := FDistMat[x][y];
  InvalidNum := Result <= InvalidDistValue;
end;

function TD_DisplayMatrix.GetNxNStdErrToDisplay(x,y: Integer; var InvalidNum: Boolean): Extended;
begin
  InvalidNum := False;
  Result := FStdErrMat[x][y];
  InvalidNum := Result <= InvalidDistValue;
end;

function TD_DisplayMatrix.GetRowGpName(Index: Integer):String;
begin
  Result := EmptyStr;
  if FComputationType = gdPairwise then
    Result := FGpNames[CurSet[Index]];
end;


function TD_DisplayMatrix.GetRowName(Index: Integer):String;
begin
  Result := EmptyStr;
  case FComputationType of
    gdPairwise:         Result := FTaxonNames[CurSet[Index]];
    gdBetweenGroupMean,
    gdNetGroupMean,
    gdWithinGroupMean,
    gdOverallMean:      Result := FGpNames[CurSet[Index]];
  end;
end;

function TD_DisplayMatrix.GetTaxonName(Index: Integer): String;
begin
  Result := FTaxonNames[Index];
end;

procedure TD_DisplayMatrix.SetDescription(AValue: TStringList);
var
  i: Integer;
begin
  FDescription.Clear;
  for i:=0 to AValue.Count-1 do
    FDescription.Add(AValue[i]);
end;

procedure TD_DisplayMatrix.SetGpName(Index: Integer; Value: String);
begin
  FGpNames[Index] := Value;
end;

procedure TD_DisplayMatrix.SetSpName(Index: Integer; Value: String);
begin
  FSpNames[Index] := Value;
end;

procedure TD_DisplayMatrix.SetPopName(Index: Integer; Value: String);
begin
  FPopNames[Index] := Value;
end;

procedure TD_DisplayMatrix.SetNoOfGps(Value: Integer);
var
  i: Integer;
begin
  FNoOfGps := Value;
  if FComputationType = gdPairwise then
    Exit;
  for i:=0 to FNoOfGps-1 do
    FGpNames.Add(EmptyStr);
end;

procedure TD_DisplayMatrix.SetNoOfTaxa(Value: Integer);
var
  i: Integer;
begin
  if FComputationType <> gdPairwise then
    Exit;
  FNoOfTaxa := Value;
  for i:=0 to FNoOfTaxa-1 do
  begin
    FTaxonNames.Add(EmptyStr);
    FGpNames.Add(EmptyStr);
  end;
end;




function TD_DisplayMatrix.WriteExportToFile(ExportOptions : TDistExportOptions):TStringList;
var
  MyD, MySe: PDistanceMatrix;
  i, j: Integer;
  AStr : String;
  RowNames : TStringList;
  InvalidNum: Boolean = False;
  Bogus: Boolean = False;
  caption: TStringList;
begin
 RowNames := TStringList.Create;
 MyD  := nil;
  MySe := nil;
  try try

    for i:= 0 to ExportOptions.NoOfRows-1 do
    begin
      case FComputationType of
       gdPairwise:
       begin
         AStr := FTaxonNames[CurSet[i]];
         if Length(FGpNames[CurSet[i]]) > 0 then
           AStr := AStr + '_{'+FGpNames[CurSet[i]]+'}';
       end;
       gdBetweenGroupMean,
       gdNetGroupMean,
       gdWithinGroupMean:  AStr := FGpNames[CurSet[i]];
      end;
      RowNames.Add(AStr);
    end;

    // Now generate the matrices to send
    case FComputationType of
      gdPairwise,
      gdBetweenGroupMean,
      gdNetGroupMean:
        begin
         if FDistMat <> nil then     MyD      := NewDistMatrix(RowNames.Count, False); // it can also be used as arrays
         if (ExportOptions.AllowStandardError) AND (FStdErrMat <> nil) then   MySe     := NewDistMatrix(NoOfRows, False);
         for i:=1 to RowNames.Count-1 do
           for j:= 0 to i-1 do
           begin
             if IsLowerLeft then MyD[i,j] := CellContent(i+1,j+1, InvalidNum, Bogus)  // calls like TDrawGrid
             else                MyD[i,j] := CellContent(j+1,i+1, InvalidNum, Bogus);
             if MySe <> nil then
             begin
               if IsLowerLeft then MySe[i,j] := CellContent(j+1,i+1, InvalidNum, Bogus)  // calls like TDrawGrid
               else                MySe[i,j] := CellContent(i+1,j+1, InvalidNum, Bogus);
            end;
          end;
         end;
      gdWithinGroupMean, gdOverallMean:  // bug fix, allows matrixes with only 1 number in cell 0, 0 to be exported.  This is more important in NV though since exporting is the only form of output.
         begin
           ExportOptions.FIsColumnar := True;
           if FDistArray <> nil then     MyD      := NewDistMatrix(RowNames.Count, False); // it can also be used as arrays
           if (ExportOptions.AllowStandardError) AND (FStdErrArray <> nil) then   MySe     := NewDistMatrix(NoOfRows, False);
           for i:=0 to RowNames.Count-1 do
           begin
             MyD[i,0] := CellContent(i+1,1, InvalidNum, Bogus);  // calls like TDrawGrid
             if MySe <> nil then
               MySe[i,0] := CellContent(i+1,2, InvalidNum, Bogus);  // calls like TDrawGrid
           end;
         end;
    end;
    if Assigned(FigureGenerator) then
    begin
      FigureGenerator.FlushTemplate(FigureGenerator);
      FigureGenerator.AssignData('ExportingData', 'True');
      FigureGenerator.AssignData('ExportDataUsingStdErr', BoolToStr(ExportOptions.WithStdErr, True));
      FigureGenerator.AssignData('ExportDataLowerLeft',  BoolToStr(ExportOptions.LowerLeft, True));
      FigureGenerator.AssignData('ExportDataAsColumn', BoolToStr(ExportOptions.OutputAsCols, True));
      FigureGenerator.AssignData('ExportDataOppositeSides', BoolToStr(ExportOptions.PrevMatrixFormat = 0, True));
      Caption := TStringList.Create;   //Writes out caption in Excel, CSV, and Text outputs
      try
        FigureGenerator.GenerateLegendAsText(Caption);
      except
        { TODO 1 -oglen :
            setup distance calculation involving groups to create a caption when a distance matrix is used.
            Currently, a caption is only generated when sequence data is used }

        { this is broken for the case where users started with a distance matrix because
          instead of going through the normal process where TAnalysisInfo and everything
          gets setup, someone decided to take a shortcut. So the information needed for
          creating a caption is missing}
        Caption.Clear;
      end;
    end;

    if ExportOptions.FIsColumnar then
      Result := WriteColumnarData(ExportOptions, MyD, MySe, RowNames)  // within gp distances etc
    else
      Result := WritePairwiseData(ExportOptions, MyD, MySe, RowNames); // between pairs

  {$IFNDEF VISUAL_BUILD}
  if (ExportOptions.OutputFormat <> ExportExcel) and (ExportOptions.SaveExcelFileTo <> SKIP_SAVE_FILE) then
    Result.SaveToFile(ExportOptions.SaveExcelFileTo);
  {$ENDIF}
  except
    On E: Exception do
      ShowErrorMessage(E);
  end;
  finally
    if MyD <> nil then
      FreeDistMatrix(MyD, NoOfRows);
    if MySe <> nil then
      FreeDistMatrix(MySe, NoOfRows);
    if Assigned(RowNames) then
      RowNames.Free;
  end;
end;

function TD_DisplayMatrix.GetMaxRowNameLen(RowNames : TStringList): LongInt;
var
  i: Integer;
begin
  Result := -1;
  for i:=0 to RowNames.Count-1 do
    if Length(RowNames.Strings[i]) > Result then
      Result := Length(RowNames.Strings[i]);
end;


function TD_DisplayMatrix.WriteColumnarData(ExportOptions : TDistExportOptions; MyD : PDistanceMatrix; MySe : PDistanceMatrix; RowNames : TStringList):TStringList;
var
  OutList   : TStringList = nil;
  DispStr: String;
  xD, xS, X, Y: Integer;
  i, EntryLen: Integer;
  MaxD, MaxS: Double;
  D, S: PDistanceMatrix;
  TheD, TheS, TheX: Double;
  InvalidNum, UseExcelComponent: Boolean;
  MaxRowNameLen: LongInt;
  Wait: TPleaseWait = nil;
  DistXls : TExcelWrite = nil;
  Caption : TStringList = nil;

  function GetValueToWrite(ARow: Integer; IsDist: Boolean): Double;
  begin
    InvalidNum := False;
    if IsDist = True then
      Result := D[ARow,0]
    else
      Result := S[ARow,0];
    InvalidNum := (Result < 0) and (not ExportOptions.AllowNegative);
  end;

begin
  Result := TStringList.Create;
  Wait := TPleaseWait.Create(nil);
  Wait.Action := 'Exporting Columular Data';
  Wait.PercentDone := 0;
  Wait.Show;


  UseExcelComponent := IsWorkbookFormat(ExportOptions.OutputFormat);
  if UseExcelComponent then
  begin
    DistXls := TExcelWrite.Create(nil);
    DistXls.IsXLS := true;
  end;

  // first initialize temporary variables
  D := MyD;
  S := nil;
  if (MySe <> nil) and (ExportOptions.WithStdErr) then
    S := MySe;
  OutList := nil;

  // Fix them to use the actual display value
  // get maximum d value
  MaxD:=0;
  for i:=0 to RowNames.Count-1 do
  begin
    TheX := GetValueToWrite(i,True);
    if (not InvalidNum) and (MaxD < abs(TheX)) then
      MaxD := abs(TheX);
  end;
  Wait.PercentDone := 5;
  MaxS := 0;
  if (S <> nil) then
  begin
    for i:=0 to RowNames.Count-1 do
    begin
      TheX := GetValueToWrite(i,False);
      if (not InvalidNum) and (MaxS < abs(TheX)) then
        MaxS := abs(TheX);
    end;
  end;
  Wait.PercentDone := 9;
  try try
    OutList := TStringList.Create;
    {OutList.Add('Title: '+FTitle);
    if FDescription.Count > 0 then
    begin
      OutList.Add('Description: ');
      for i:=0 to FDescription.Count-1 do
        OutList.Add('  '+FDescription[i]);
    end;}
    OutList.Add(EmptyStr);
    Wait.PercentDone := 20;
    // find maxD and maxS;
    // Fix them to use the actual display value

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
    if S <> nil then
    begin
      if MaxS > 0.0 then xS := Trunc(log10(MaxS));
      if xS < 0 then xS := 1;
      Inc(xS);
    end;

    if xD>xS then   X := xD
    else            X := xS;

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
    MaxRowNameLen := GetMaxRowNameLen(RowNames)+1; // +1 for [

    DispStr := BlankString(MaxRowNameLen)+' '+
               StrToStrWidth(ExportOptions.FDAcronym, EntryLen);  // x.y
    if UseExcelComponent then
      DistXls.Add(ExportOptions.FDAcronym);
    if S <> nil then
    begin
      DispStr := DispStr+' '+ExportOptions.FVAcronym;
      if UseExcelComponent then
        DistXls.Add(ExportOptions.FVAcronym);
    end;
    OutList.Add(DispStr);
    if UseExcelComponent then
      DistXls.WriteLine(0, 'B'); // Start at B because we need to be one over since there are names also.
    for i:=0 to Rownames.Count-1 do
    begin
      DispStr := StrToStrWidth(RowNames.Strings[i], MaxRowNameLen)+' ';
      if UseExcelComponent then
        DistXls.Add(RowNames.Strings[i]);
      TheD := GetValueToWrite(i, True);
      if InvalidNum then
      begin
         DispStr := DispStr + StrToStrWidth('n/c',EntryLen) + ' ';
         if UseExcelComponent then
           DistXls.Add('n/c', clRed, clSilver);
      end
      else
      begin
         DispStr := DispStr + FloatToStrWidth(TheD, EntryLen, Y);
         if UseExcelComponent then
           DistXls.AddP(TheD, Y);
      end;

      if S <> nil then
      begin
        TheS := GetValueToWrite(i, False);
        if InvalidNum then
        begin
           DispStr := DispStr + StrToStrWidth('n/c',EntryLen) + ' ';
           if UseExcelComponent then
             DistXls.Add('n/c', clRed, clSilver);
        end
        else
        begin
          if not EndsWith(DispStr, ' ') then
            DispStr := DispStr + ' ';
           DispStr := DispStr + FloatToStrWidth(TheS,EntryLen, Y);
           if UseExcelComponent then
             DistXls.AddP(TheS, Y);
        end;

      end;
      OutList.Add(DispStr);
      if UseExcelComponent then
        DistXls.WriteLine();
      Wait.PercentDone := (i*100) div RowNames.Count + 20;
    end; // end for all Blocks

    if (ExportOptions.OutputFormat <> ExportMEGA) and (FigureGenerator <> nil) then
    begin
      Caption := TStringList.Create;
      try
        FigureGenerator.GenerateLegendAsText(Caption);
      except
        Caption.Clear; { temporary bug fix - we crash if done from a distance matrix file}
      end;

      {$IFDEF VISUAL_BUILD}
      if ExportOptions.OutputFormat <> ExportText then
        DistXLs.AddCaptionAsWorksheet(Caption, 'Caption')
      else
      begin
        for i := 0 to Caption.Count-1 do
          OutList.add(Caption.Strings[i]);
      end;
      {$ELSE}
      FigureGenerator.SaveCaptionToFile;
      {$ENDIF}
    end;

    if not IsWorkbookFormat(ExportOptions.OutputFormat) then
      Result.Text := OutList.Text
    else if UseExcelComponent then
    begin
      DistXls.SaveFile(ExportOptions.SaveExcelFileTo, ExportOptions.OutputFormat);
      RunAProgram(ExportOptions.SaveExcelFileTo);
    end;
  Except
    On E: Exception do
    begin
      if UseExcelComponent then
        DistXls.HadError;
      ShowErrorMessage(E);
    end;
  end;
  finally
    if Assigned(OutList) then
      OutList.Free;
    if Assigned(DistXls) then
    DistXls.Free;
    if Assigned(Wait) then
      Wait.Free;
    if Assigned(Caption) then
      Caption.Free;
  end;
end;

function TD_DisplayMatrix.WritePairwiseData(ExportOptions: TDistExportOptions;
  MyD: PDistanceMatrix; MySe: PDistancematrix; RowNames: TStringList
  ): TStringList;
var
  Caption: TStringList = nil;
  OutList: TStringList = nil;
  AddToDispStr,DispStr: String;
  xD, xS, X, Y, xId: Integer;
  i, j, h, g, MatFrags, fromX, toX: Integer;
  EntryLen : Integer; // str length of the dist or std err value
  EntrySize: Integer; // total str length needed for entrylen + space for '[]'
  EntryPerLine, MatDside, MatSEside: Integer;
  MaxD, MaxS: Double;
  D, S: PDistanceMatrix;
  TheD, TheS, TheX: Double;
  InvalidNum: Boolean = False;
  UseExcelComponent: Boolean = True;
  Wait: TPleaseWait = nil;
  DistXls : TExcelWrite = nil;
  RowNameG, RowNameH: String;
  ValToWrite: String = '';
  tempDescription: String = '';

  function GetValueToWrite(ARow, ACol: integer; IsDist: Boolean): Double;
  begin
    Result := -1.0;
    InvalidNum := False;
    if ARow = ACol then Exit;

    if IsDist = True then
    begin
      if ARow > ACol then
        Result := MyD[ARow,ACol]
      else
        Result := MyD[ACol,ARow];
    end
    else
    begin
      if ARow > ACol then
        Result := MySe[ARow,ACol]
      else
        Result := MySe[ACol,ARow];
    end;
    InvalidNum := (Result < 0) and (not ExportOptions.AllowNegative);
  end;

begin
  Wait := TPleaseWait.Create(nil);
  case ExportOptions.OutputFormat of
    ExportExcel, ExportExelXML: Wait.Action := 'Exporting and Formatting to Excel';
    ExportODS  : Wait.Action := 'Exporting to Open Document';
    ExportCSV  : Wait.Action := 'Exporting to CSV';
    ExportText : Wait.Action := 'Exporting and Formatting to Text';
    ExportMEGA : Wait.Action := 'Exporting and Formatting to MEGA';
  end;
  Wait.PercentDone := 0;
  Wait.Show;

  Result := TStringList.Create;
  UseExcelComponent := (ExportOptions.OutputFormat = ExportExcel) or
                       (ExportOptions.OutputFormat = ExportCSV) or
                       (ExportOptions.OutputFormat = ExportExelXML) or
                       (ExportOptions.OutputFormat = ExportODS);

  If UseExcelComponent then //No need to create it if we won't use it
  begin
    DistXls := TExcelWrite.Create(nil, 'MatrixOutput');
    DistXls.IsXLS := True;
  end;

  D := MyD;
  S := MySe;
  OutList := nil;
  if D = nil then
    Exit;

  //The length of the Otu #Id everyTime
  xId := Trunc(log10(RowNames.Count));
  if xId < 0 then
    xId := 0;
  Inc(xId);

  if ExportOptions.LowerLeft then
    MatDSide := 1
  else
    MatDSide := 2;

  MatSESide := 0;
  If ExportOptions.WithStdErr then
    begin
       if MatDSide = 3 then
         MatSESide := 3  // if column then all column
       else if ExportOptions.PrevMatrixFormat = 0 then // opposing sides
       begin
         if MatDSide = 1 then MatSESide := 2 else MatSESide := 1;
       end
       else
         MatSESide := MatDSide;
     end
  else
    begin MatSESide := 0; S := nil; end; // only distances


  // number of entries
  if MatDSide = 3 then
    EntryPerLine := 1
  else
    EntryPerLine := ExportOptions.FEntriesPerLine;

  try try
    OutList := TStringList.Create;
    if ExportOptions.OutputFormat = ExportMega then
    begin
      OutList.Add('#mega');
      OutList.Add('!Title: '+FTitle+';');
      DispStr := '!Format'+' DataType=Distance';
      case MatDside of
        1:  DispStr := DispStr + ' DataFormat=LowerLeft';
        2:  DispStr := DispStr + ' DataFormat=UpperRight';
      // 3:  DispStr := DispStr + 'DataFormat=Column ';
      end;
      DispStr := DispStr+' NTaxa='+IntToStr(RowNames.Count);
      DispStr := DispStr + ';';
      OutList.Add(DispStr);
      if FDescription.Count > 0 then
      begin
        OutList.Add('!Description');
        for i:=0 to FDescription.Count-1 do
        begin
          tempDescription := FDescription[i];
          if Pos('[fsBold]', tempDescription) > 0 then
            Delete(tempDescription, Pos('[fsBold]', tempDescription), 8);
          OutList.Add('  '+tempDescription);
        end;
        OutList.Add(';');
      end;
    end
    else if ExportOptions.OutputFormat = ExportText then
    begin
      OutList.Add('Title: '+FTitle);
      if FDescription.Count > 0 then
      begin
        OutList.Add('Description');
        for i:=0 to FDescription.Count-1 do
        begin
          tempDescription := FDescription[i];
          if Pos('[fsBold]', tempDescription) > 0 then
            Delete(tempDescription, Pos('[fsBold]', tempDescription), 8);
          OutList.Add('  '+tempDescription);
        end;
      end;
    end;

    if ExportOptions.OutputAsCols then
    begin
      If UseExcelComponent then
      begin
        DistXls.Add('Species 1');
        DistXls.Add('Species 2');
        DistXls.Add('Dist');
      end;
      OutList.Add('');
      OutList.Add(StrToStrWidth('Species 1', GetMaxRowNameLen(RowNames)) + StrToStrWidth('Species 2', GetMaxRowNameLen(RowNames)) + StrToStrWidth('Distance', GetMaxRowNameLen(RowNames)));
      if (UseExcelComponent) AND (MatSESide <> 0) then
        DistXls.Add('Std. Err');
      If UseExcelComponent then
        DistXls.WriteLine();
        for h:= 1 to RowNames.Count-1 do
          begin
            for g:=0 to h-1 do
            begin
              If UseExcelComponent then
              begin
                DistXls.Add(RowNames.Strings[g]);
                DistXls.Add(RowNames.Strings[h]);
                //tempVal := GetValueToWrite(h, g, true);
                if InvalidNum then
                  DistXls.Add(' n/c')
                else
                  DistXls.AddP(GetValueToWrite(h, g, true), ExportOptions.Precision); // Distance
                if MatSESide <> 0 then
                  DistXls.AddP(GetValueToWrite(h, g, false), ExportOptions.Precision);  // Standard error
                DistXls.WriteLine(0, 'A', BuildNumberFormat(ExportOptions.Precision));
              end;
              RowNameG := StrToStrWidth(RowNames.Strings[g], GetMaxRowNameLen(RowNames) + 1);
              RowNameH := StrToStrWidth(RowNames.Strings[h], GetMaxRowNameLen(RowNames) + 1);
              try
                ValToWrite := FloatToStrWidth(GetValueToWrite(h, g, true), ExportOptions.Precision+1+Length(FloatToStr(Floor(GetValueToWrite(h, g, true)))), ExportOptions.Precision);
                if InvalidNum then
                  ValToWrite := ' n/c';
              Except on E: Exception do
                ValToWrite := ' n/c'; // When the value has an error.
              end;
              OutList.add(RowNameG + RowNameH + ValToWrite);
            end;
            if RowNames.Count = 1 then // Avoids the div by 0 error.
              Wait.PercentDone := 100
            else
              Wait.PercentDone := (h * 100) div RowNames.Count-1;
          end;
    end
    else
    begin { output as matrix}
      OutList.Add(EmptyStr);
      if UseExcelComponent then
        DistXls.AddBlankCell;
      for i:=0 to RowNames.Count-1 do
      begin
        DispStr := '['+IntToStrWidth(i+1, xId)+'] '+'#'+OtuNameToMegaStr(RowNames.Strings[i]);
        OutList.Add(DispStr);
        If UseExcelComponent then
          DistXls.Add(OtuNameToMegaStr(RowNames.strings[i]));
      end;
      OutList.Add(EmptyStr);
      if UseExcelComponent then
        DistXls.WriteLine;

      // find maxD and maxS;
      // Fix them to use the actual display value
      MaxD:=0;
      if D <> nil then
        for i:=0 to RowNames.Count-1 do
          for j:=0 to RowNames.Count-1 do
          begin
            TheX := GetValueToWrite(i,j,True);
            if (not InvalidNum) and (MaxD < abs(TheX)) then
              MaxD := abs(TheX);
          end;

      // Fix them to use the actual display value
      MaxS := 0;
      if S <> nil then
        for i:=0 to RowNames.Count-1 do
          for j:=0 to RowNames.Count-1 do
          begin
            TheX := GetValueToWrite(i,j,False);
            if (not InvalidNum) and (MaxS < abs(TheX)) then
              MaxS := abs(TheX);
          end;

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
      if S <> nil then
      begin
        if MaxS > 0.0 then xS := Trunc(log10(MaxS));
        if xS < 0 then xS := 1;
        Inc(xS);
      end;

      if xD>xS then   X := xD
      else            X := xS;

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

      if (MatDSide > 0) and (MatSESide > 0) then // two matrices
      begin
         if MatDside = MatSEside then
           EntrySize := EntryLen + 1 + 1+ EntryLen + 1 // EntryLen + ' '+ [ + EntryLen+]
         else
           EntrySize := 1+ EntryLen + 1;                // for max(EntryLen, [+EntryLen+])
      end
      else
        EntrySize := EntryLen;                         // no baggage

      // now it will start
      fromX    := 0;
      toX      := 0;

      MatFrags := 0;
      while True do
      begin
        if toX = RowNames.count-1 then
          Break;
        fromX := MatFrags*EntryPerLine;
        toX   := (MatFrags+1)*EntryPerLine -1;
        Inc(MatFrags);

        if toX >= RowNames.count then
          toX := RowNames.Count-1;
        if toX <= fromX then
          toX := RowNames.Count-1;

        // we write the top row of the matrix
        DispStr := '['+BlankString(xId+1)+' ';     // write the top row of Matrix  (eg. [        1     2     3     4     5     6     7     8     9    10    11    12    13 ]  )
        for i:= fromX to toX do
          DispStr := DispStr + IntToStrWidth(i+1,EntrySize)+' '; // it should be centered, ideally
        DispStr := DispStr + ']';
        OutList.Add(DispStr);

        // now write matrix
        for i:= 0 to RowNames.Count-1 do
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
          If UseExcelComponent then
            DistXls.Add(OtuNameToMegaStr(RowNames.strings[i]));
          for j:=fromX to toX do
          begin
            if (MatDSide > 0) and (MatSESide > 0) then // two matrices
            begin
              if (MatDSide = MatSESide) then
              begin
                if i = j then
                begin
                  DispStr := DispStr + BlankString(EntrySize)+' ';
                  If UseExcelComponent then
                    DistXls.AddBlankCell;
                end
                else if (MatDSide = 1) and (i<j) then
                  Break
                else if (MatDSide = 2) and (i>j) then
                begin
                  DispStr := DispStr + BlankString(EntrySize)+ ' ';  // to fill the gap
                  If UseExcelComponent then
                  begin
                    DistXls.AddBlankCell;
                    if MatDside = MatSEside then
                      DistXls.AddBlankCell;  // Bug Fix for error when MatDside = MatSEside and UpperRight.
                  end;
                end
                else
                begin
                  TheD := GetValueToWrite(i, j, True);
                  TheS := GetValueToWrite(i, j, False);
                  AddToDispStr := EmptyStr;
                  if InvalidNum then
                  begin
                    AddToDispStr := AddToDispStr + StrToStrWidth('?',EntryLen);
                    If UseExcelComponent then
                      DistXls.Add('?');
                  end
                  else
                  begin
                    AddToDispStr := AddToDispStr + FloatToStrWidth(TheD, EntryLen, Y); // Y = FPrecision
                    If UseExcelComponent then
                      DistXls.AddP(TheD, Y);
                  end;

                  AddToDispStr := AddToDispStr + ' [';
                  if InvalidNum then
                  begin
                    AddToDispStr := AddToDispStr + StrToStrWidth('?',EntryLen);
                    If UseExcelComponent then
                      DistXls.Add('?');
                  end
                  else
                  begin
                    AddToDispStr := AddToDispStr + FloatToStrWidth(TheS, EntryLen, Y);
                    If UseExcelComponent then
                      DistXls.AddP(TheS, Y, clWhite, clBlue);
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
                  If UseExcelComponent then
                    DistXls.AddBlankCell;
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
                      If UseExcelComponent then
                        DistXls.Add('?');
                    end
                    else
                    begin
                      AddToDispStr := FloatToStrWidth(TheX,EntryLen,Y) + ' ';
                    end;
                    if i < j then
                    begin
                      AddToDispStr := '['+AddToDispStr+']';
                      If UseExcelComponent then
                        DistXls.AddP(TheX, Y, clWhite, clBlue);
                    end
                    else
                    begin
                      AddToDispStr := ' '+AddToDispStr+' ';
                      If UseExcelComponent then
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
                      If UseExcelComponent then
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
                      If UseExcelComponent then
                        DistXls.AddP(TheX, Y, clWhite, clBlue);
                    end
                    else
                    begin
                      AddToDispStr := ' '+AddToDispStr+' ';
                      If UseExcelComponent then
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
                If UseExcelComponent then
                  DistXls.AddBlankCell;  // Nothing is ever on a Diaglonal, so just add a blank cell here
                Continue;
              end
              else if (MatDSide = 1) and (i<j) then
                Break
              else if (MatDSide = 2) and (i>j) then
              begin
                DispStr := DispStr + BlankString(EntrySize)+' ';  // to fill the gap
                If UseExcelComponent then
                  DistXls.AddBlankCell;
              end
              else
              begin
                TheX := GetValueToWrite(i,j,True);
                if InvalidNum then
                begin
                  AddToDispStr := StrToStrWidth('?',EntryLen) + ' ';
                  If UseExcelComponent then
                    DistXls.Add('?');
                end
                else
                begin
                  AddToDispStr := FloatToStrWidth(TheX, EntryLen, Y)+' ';
                  If UseExcelComponent then
                    DistXls.AddP(TheX, Y);
                end;
                DispStr := DispStr + AddToDispStr;
              end;
            end;  // if only one matrix
          end; // all entries in a row
          OutList.Add(DispStr);
          If UseExcelComponent then
            DistXls.WriteLine(0, 'A', BuildNumberFormat(Y));
        end;  // end of all rows in a block
        OutList.Add(EmptyStr);
        If UseExcelComponent then
          DistXls.WriteLine();
      end; // end for all Blocks
    end; // End of "Square" export type


    if ExportOptions.OutputFormat <> ExportMEGA then
    begin
      Caption := TStringList.Create;
      if Assigned(FigureGenerator) then
        FigureGenerator.GenerateLegendAsText(Caption);
      {$IFDEF VISUAL_BUILD}
      if ExportOptions.OutputFormat <> ExportText then
        DistXls.AddCaptionAsWorksheet(Caption, 'Caption')
      else
      begin
        for i := 0 to Caption.Count-1 do
          OutList.add(Caption.Strings[i]);
      end;
      {$ELSE}
      FigureGenerator.SaveCaptionToFile;
      {$ENDIF}
    end;
   Wait.PercentDone := 100;
   if (ExportOptions.OutputFormat = ExportMega) or (ExportOptions.OutputFormat = ExportText) then
     Result.Text := OutList.Text
   else if ExportOptions.OutputFormat = ExportCSV then
     Result.Text := DistXls.GetCsvText;

   If UseExcelComponent and (ExportOptions.OutputFormat <> ExportCSV) then
     DistXls.SaveFile(ExportOptions.SaveExcelFileTo, ExportOptions.OutputFormat);
  Except
    On E: Exception do
    begin
      If DistXls <> nil then
        DistXls.HadError;
      ShowErrorMessage(E);
    end;
  end;
  finally
    if Assigned(DistXls) then
      DistXls.Free;
    if Assigned(OutList) then
      OutList.Free;
    if Assigned(Wait) then
      Wait.Free;
    if Assigned(Caption) then
      Caption.Free;
  end;
end;



procedure TD_DisplayMatrix.SetTaxonName(Index: Integer; const Value: String);
begin
  if Index > (FTaxonNames.Count-1) then
    raise Exception.Create('Error, Attempting to access an index above the number of elements in FTaxonNames, set the size first.');
  FTaxonNames[Index] := Value;
end;


//----------------------------------------------------------------
//--------- Visual Status -----Visual Status ---------------------
//----------------------------------------------------------------

Constructor TVS_DisplayMatrix.Create;
begin
  CurStatusBar := nil;
  CurStatusBar := TStringList.Create;
  CurStatusBar.Add(' ');
  CurStatusBar.Add(' ');
  ShouldClose := False;
  LeftOfDecimalLen:= 0;
  FPrecision := 5;
  IsFormInit    := False;
  FIsResizing := False;
  {V_DisplayMatrix := TDisplayMatrixDlg.Create(nil);
  V_DisplayMatrix.UnTypedLinkToVisualStatus := Self;
  V_DisplayMatrix.UnTypedLinkToData := DataClass; }

end;


procedure TVS_DisplayMatrix.DecPrecision;
begin
if (FPrecision <= 0) then
    Exit;
  FPrecision := FPrecision - 1;
end;

procedure TVS_DisplayMatrix.IncPrecision;
begin
if (FPrecision >= 10) then
    Exit;
  FPrecision := FPrecision + 1;
end;

destructor TVS_DisplayMatrix.Destroy;
begin
  FreeAndNil(CurStatusBar);
  inherited destroy;
end;

{$IFDEF VISUAL_BUILD}

procedure TDisplayMatrixDlg.FormCreate(Sender: TObject);
begin
  FStrBuilder := TMegaStringBuilder.Create;
  FPrecision := 3;
  FSearchResult := -1;
  SearchPanel.Visible := False;
  FScrollStarted := False;
  FSelfScrolling := False;
  FVScrollPos := 0;
  FHScrollPos := 0;
  FLastRow := 0;
  FFontSize := 10;
  FFormObservers := TFormObserverList.Create;
  HelpContext := HC_Distance_Matrix_Explorer;
  D_DisplayMatrix := nil;
  VS_DisplayMatrix := nil;
  AssignHelpContext;
  DataGrid.ColWidths[0] := 4;
  ShowPairNameItem.checked := PrevPairNameTextVisible;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+': Distance Explorer';
  ExportFormatOptions := [meMega, meCsv, meExcel, meText]; // by default, enable them all
  DataGrid.DefaultRowHeight := DataGrid.Canvas.TextHeight('W') + 4;
  DataGrid.RowCount := 1;
  ImageForm.UpdateImgList(Self);
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

procedure TDisplayMatrixDlg.FormDestroy(Sender: TObject);
begin
  if OutputMatrixDlg <> nil then OutputMatrixDlg.Free;
  OutputMatrixDlg := nil;
  MegaForm.RemoveWindowFromTray(Self);
  FreeAndNil(D_DisplayMatrix);
  FreeAndNil(VS_DisplayMatrix);
  if Assigned(FStrBuilder) then
    FStrBuilder.Free;
end;

procedure TDisplayMatrixDlg.LinkDataClass(DispDlg : TD_DisplayMatrix);
begin
  D_DisplayMatrix := DispDlg;
  InitScrollbars(True);
end;

procedure TDisplayMatrixDlg.LinkVisualStatusClass(DispDlgVS : TVS_DisplayMatrix);
begin
  VS_DisplayMatrix := DispDlgVS;
end;

procedure  TDisplayMatrixDlg.DataToVisualStatusBar(Sender : TObject);
var
  i : integer;
begin
  if StatusBar <> nil then
  begin
    for i:= 0 to VS_DisplayMatrix.CurStatusBar.Count-1 do
      StatusBar.Panels[i].Text := VS_DisplayMatrix.CurStatusBar.Strings[i];
  end;
end;


procedure TDisplayMatrixDlg.FormActivate(Sender: TObject);
begin
  MegaForm.UpdateMenuForPinupItems(WindowsMenuItem, Self);
  if VS_DisplayMatrix.IsFormInit then   //so this is only done once after the form is drawn the first time
    Exit;
  VS_DisplayMatrix.IsFormInit := True;

  case D_DisplayMatrix.FComputationType of   // Disables MEGA button if we aren't going to be able to export to MEGA format.
      gdPairwise:     if D_DisplayMatrix.FNoOfTaxa < 3 then        ExportMEGABtn.Visible := false;
      gdBetweenGroupMean,
      gdNetGroupMean: if D_DisplayMatrix.FNoOfGps < 3 then  ExportMEGABtn.Visible := False;
      gdWithinGroupMean: ExportMEGABtn.Visible := False;
    end;

  ShowGpNamesItem.Checked := True;
  ShowTaxaNamesItem.Checked := True;

  ShowPairNameItem.Checked := True;
  ShowPairNameBtn.Down := True;

  // check if we have all the data

    if (D_DisplayMatrix.FComputationType = gdOverallMean) or (D_DisplayMatrix.FComputationType = gdWithinGroupMean)then
    begin
          ShowPairNameItem.Enabled := False;
          ShowPairNameBtn.Enabled := False;
          LowerLeftBtn.Enabled    := False;
          UpperRightBtn.Enabled    := False;
    end;

    DataGrid.ColCount := D_DisplayMatrix.NoOfCols+1; //add one because of the fixed column with names
    DataGrid.RowCount := D_DisplayMatrix.NoOfRows+1; //Add one because of the fixed row at the top that does not display data

    AverageMenu.Visible := (D_DisplayMatrix.FComputationType = gdPairwise) and (D_DisplayMatrix.FNoOfTaxa > 2) and
                           (not D_DisplayMatrix.FHasTestResult);

    // no negative values for prob
    if D_DisplayMatrix.FHasTestResult then
    begin
      //FAllowNeg := False;
      // otherwise FAllowNeg is always false and output for Sel Tests is incorrect (all '?'): Joel
      LowerLeftBtn.Hint := D_DisplayMatrix.FDAcronym+' in lower left';
      UpperRightBtn.Hint := D_DisplayMatrix.FDAcronym+' in upper right';
    end;

    // otherwise, AllowNeg is as set by the user

    DataGrid.FixedCols := 1;
    DataGrid.FixedRows := 1;


  // show gp names information is dealt with here
  ShowGpNamesItem.Visible  := (D_DisplayMatrix.FNoOfGps > 0) and (D_DisplayMatrix.FComputationType = gdPairwise);
  SortGpItem.Visible       := (D_DisplayMatrix.FNoOfGps > 0) and (D_DisplayMatrix.FComputationType = gdPairwise);




  if D_DisplayMatrix.IsLowerLeft then
    LowerLeftAction.Checked := True
  else
    UpperRightAction.Checked := True;

  // computes the decimal point position
  ComputeLeftOfDecimalLen;
  SetBestPrecisionForDisplay;
  if FPrecision = 0 then
    DecPrecisionAction.Enabled := False
  else if FPrecision = 10 then
    IncPrecisionAction.Enabled := False;
  VS_DisplayMatrix.FPrecision := FPrecision;

  AvgAllItem.Enabled := (D_DisplayMatrix.FComputationType = gdPairwise);
  WithinGpsAvgItem.Visible :=     (D_DisplayMatrix.FNoOfGps > 1) and (D_DisplayMatrix.FComputationType = gdPairwise);
  BetweenGpsAvgItem.Visible :=    (D_DisplayMatrix.FNoOfGps > 1) and (D_DisplayMatrix.FComputationType = gdPairwise);
  NetBetweenGpsAvgItem.Visible := (D_DisplayMatrix.FNoOfGps > 1) and (D_DisplayMatrix.FComputationType = gdPairwise);
  N2.Visible := (WithinGpsAvgItem.Visible or BetweenGpsAvgItem.Visible or NetBetweenGpsAvgItem.Visible);
  if D_DisplayMatrix.FNoOfGps > 0 then
    ShowGpNamesItem.Checked := PrevShowGpNames;
  VS_DisplayMatrix.CurStatusBar.Strings[1] := D_DisplayMatrix.FFullName;
  //StatusBar.SimpleText := D_DisplayMatrix.FFullName;

  UpdateColumnWidths(True, True);
  DataGrid.DefaultRowHeight := DataGrid.Canvas.TextHeight('W') + 4;
  // finally adjust the matrix width and height such that it is not too large
  with DataGrid do
  begin
    // I have added 1 RowCount so as to provide some empty space below
    if VisibleRowCount = RowCount-1 then //
      Self.Height := Self.Height - Height + (RowCount+1)*(RowHeights[0]+GridLineWidth);
    if VisibleColCount = ColCount-1 then
      Self.Width  := Width       - Width  + ColWidths[0] + ColCount*(ColWidths[1]+GridLineWidth);
  end;

  VS_DisplayMatrix.CurStatusBar.OnChange := DataToVisualStatusBar;
  InitScrollbars(False);
  DataGrid.Invalidate;
  AddWindowToTray(Self);
end;

// for displaying the row name
function TDisplayMatrixDlg.ConstructCol0RowName(Index: LongInt; addRowNumber: Boolean = True): String;
begin
  FStrBuilder.Clean;
  if addRowNumber then
  begin
    FStrBuilder.Add(IntToStr(Index));
    FStrBuilder.Add('. ');
  end;

  if ShowTaxaNamesItem.Checked and ShowGpnamesItem.Checked then
  begin
    FStrBuilder.Add(D_DisplayMatrix.RowName[Index-1]);
    if Length(D_DisplayMatrix.RowGpname[Index-1]) > 0 then
    begin
      FStrBuilder.Add(' {');
      FStrBuilder.Add(D_DisplayMatrix.RowGpName[Index-1]);
      FStrBuilder.Add('}');
    end;
  end
  else if ShowTaxaNamesItem.Checked then
    FStrBuilder.Add(D_DisplayMatrix.RowName[Index-1])
  else if ShowGpNamesItem.Checked then
    if Length(D_DisplayMatrix.RowGpname[Index-1]) > 0 then
      FStrBuilder.Add(D_DisplayMatrix.RowGpName[Index-1]);
  Result := FStrBuilder.GenerateString;
end;

procedure TDisplayMatrixDlg.InitScrollbars(setPosition: Boolean);
var
  p: Integer = -1;
  aPosition: Integer = -1;
begin
  if setPosition then
    aPosition := 1
  else
    aPosition := GridHorizScrollbar.Position;
  if DataGrid.ColCount > (DataGrid.VisibleColCount + DataGrid.FixedCols) then
  begin
    GridHorizScrollbar.Visible := True;
    GridHorizScrollbar.SmallChange := 1;
    GridHorizScrollbar.LargeChange := max(DataGrid.VisibleColCount, GridHorizScrollbar.SmallChange);
    if DataGrid.ColCount > DataGrid.VisibleColCount then
      p := max(1, DataGrid.VisibleColCount - 1)
    else
      p := 1;
    GridHorizScrollbar.SetParams(aPosition, 1, DataGrid.ColCount - DataGrid.FixedCols, max(1, p));
  end
  else
    GridHorizScrollbar.Visible := False;

  if setPosition then
    aPosition := 1
  else
    aPosition := GridVertScrollbar.Position;
  if DataGrid.RowCount > (DataGrid.VisibleRowCount + DataGrid.FixedRows) then
  begin
    GridVertScrollbar.Visible := True;
    GridVertScrollbar.SmallChange := 1;
    GridVertScrollbar.LargeChange := max(DataGrid.VisibleRowCount, GridVertScrollbar.SmallChange);
    if DataGrid.RowCount > DataGrid.VisibleRowCount then
      p := max(1, DataGrid.VisibleRowCount - 1)
    else
      p := 1;
    GridVertScrollbar.SetParams(aPosition, 1, DataGrid.RowCount - DataGrid.FixedRows, max(1, p));
  end
  else
    GridVertScrollbar.Visible := False;
end;

function TDisplayMatrixDlg.RowVisible(aRow: Integer): Boolean;
begin
  Result := False;
  if aRow < DataGrid.TopRow then
    Exit;
  if aRow > (DataGrid.TopRow + DataGrid.VisibleRowCount) then
    Exit;
  Result := True;
end;

function TDisplayMatrixDlg.ColVisible(aCol: Integer): Boolean;
begin
  Result := False;
  if aCol < DataGrid.LeftCol then
    Exit;
  if aCol > (DataGrid.LeftCol + DataGrid.VisibleColCount) then
    Exit;
  Result := True;
end;

function TDisplayMatrixDlg.ValidSelectableRow(aRow: Integer): Integer;
begin
  Result := aRow;
  if aRow < DataGrid.FixedRows then
    Result := DataGrid.FixedRows
  else if aRow >= DataGrid.RowCount then
    Result := DataGrid.RowCount - 1;
end;

function TDisplayMatrixDlg.ValidSelectableCol(aCol: Integer): Integer;
begin
  Result := aCol;
  if aCol < DataGrid.FixedCols then
    Result := DataGrid.FixedCols
  else if aCol >= DataGrid.ColCount then
    Result := DataGrid.ColCount - 1;
end;

procedure TDisplayMatrixDlg.UnhighlightSearchResult;
var
  temp: Integer = -1;
begin
  if FSearchResult <> -1 then
  begin
    temp := FSearchResult;
    FSearchResult := -1;
    DataGrid.InvalidateRow(temp + 1);
    DataGrid.InvalidateCol(temp + 1);
  end;
end;

function TDisplayMatrixDlg.FindTaxon(searchQuery: String; startRow: Integer; searchForward: Boolean): Integer;
var
  wrapSearch: Boolean = False;
begin
  Result := -1;
  FSearchResult := SearchLabels(searchQuery, startRow, searchForward);
  Assert(FSearchResult < D_DisplayMatrix.NoOfRows, Format('invalid FSearchResult = %d', [FSearchResult]));
  if FSearchResult = -1 then
  begin
    if searchForward and (startRow <> 0) then
    begin
      wrapSearch := (MessageDlg(Format('Query "%s" not found. Do you want to go to the top and search again?', [searchQuery]), mtConfirmation, [mbYes, mbNo],0) = mrYes);
      if wrapSearch then
        FSearchResult := SearchLabels(SearchEdit.Text, 0, True);
    end
    else if (not SearchForward) and (startRow <> (DataGrid.RowCount - 1)) then
    begin
      wrapSearch := (MessageDlg(Format('Query "%s" not found. Do you want to go to the bottom and search again?', [searchQuery]), mtConfirmation, [mbYes, mbNo],0) = mrYes);
      if wrapSearch then
        FSearchResult := SearchLabels(SearchEdit.Text, DataGrid.RowCount - 2, False);
    end;
  end;

  if (FSearchResult = -1) then
  begin
    if wrapSearch then
      ShowMessage(Format('Query "%s" not found.', [searchQuery]));
    Exit;
  end
  else
  begin
    DataGrid.Row := FSearchResult + 1;
    Invalidate;
  end;

  Result := FSearchResult;
  Assert(Result < D_DisplayMatrix.NoOfRows, Format('invalid FSearchResult = %d', [FSearchResult]));
end;

function TDisplayMatrixDlg.SearchLabels(searchQuery: String; startIndex: Integer; searchForward: Boolean): Integer;
var
  i: Integer = -1;
  aName: String = '';
  query: String = '';
begin
  Result := -1;
  if startIndex >= D_DisplayMatrix.NoOfRows then
    Exit;
  if D_DisplayMatrix.NoOfRows > 0 then
  begin
    query := LowerCase(searchQuery);
    if searchForward then
    begin
      for i := startIndex to D_DisplayMatrix.NoOfRows - 1 do
      begin
        aName := LowerCase(ConstructCol0RowName(i + 1));
        if Pos(query, aName) > 0 then
          Exit(i);
      end;
    end
    else
    begin
      for i := startIndex downto 0 do
      begin
        aName := LowerCase(ConstructCol0RowName(i + 1));
        if Pos(query, aName) > 0 then
          Exit(i);
      end;
    end;
  end;
end;


procedure TDisplayMatrixDlg.SortItemClick(Sender: TObject);
var
  tempDists: TList = nil;
  aDist: TPairwiseDist = nil;
  aInvalidNum: Boolean = False;
  aIsStdErrDisplay: Boolean = False;
  tempDouble: Double = 0;
  i: Integer = -1;
  nRows: Integer = -1;
  OldIndx: Integer = -1;
  TheName: String = '';
  OriginalList: TStringList = nil;
  SortedList: TStringList = nil;
begin
  try
    Assert(FSearchResult < D_DisplayMatrix.NoOfRows, Format('invalid FSearchResult = %d', [FSearchResult]));
   if FSearchResult <> -1 then
     OldIndx := D_DisplayMatrix.CurSet[FSearchResult];
    nRows := D_DisplayMatrix.NoOfRows;
    if Sender = SortByDistItem then
    begin
      TempDists := TList.Create;
      for i := 0 to nRows - 1 do
      begin
        aInvalidNum := False;
        aIsStdErrDisplay := False;
        if i = 0 then
          tempDouble := 0
        else
          tempDouble := D_DisplayMatrix.CellContent(i + 1, 1, aInvalidNum, aIsStdErrDisplay);
        if aInvalidNum then
          tempDouble := InvalidDistValue;
        aDist := TPairwiseDist.Create(D_DisplayMatrix.CurSet[i], D_DisplayMatrix.CurSet[0], tempDouble, ConstructCol0RowName(i + 1, False));
        TempDists.Add(aDist);
      end;
      TempDists.Sort(@ComparePairwiseDists);
      for i := 0 to TempDists.Count - 1 do
      begin
        aDist := TPairwiseDist(TempDists[i]);
        D_DisplayMatrix.CurSet[i] := aDist.TaxonA;
      end;
    end
    else if Sender = SortOriginalItem then
    begin

       For i := 0 to nRows - 1 do
          D_DisplayMatrix.CurSet[i] := i;
    end
    else
    begin
      SortedList := TStringList.Create;
      OriginalList := TStringList.Create;
      If Sender = SortByNameItem then
      begin
        SortedList.Sorted := True;
        SortedList.Duplicates := dupAccept;
        for i := 0 to nRows-1 do
          begin
            OriginalList.Add(D_DisplayMatrix.TaxonName[i]);
            SortedList.Add(D_DisplayMatrix.GetRowName(i));
          end;
          SortedList.Sort();
      end
      else //Sorting by Group Name
      begin
        for i := 0 to nRows-1 do
          begin
            OriginalList.Add(D_DisplayMatrix.TaxonName[i]);
            SortedList.Add(D_DisplayMatrix.TaxonName[i]+'='+D_DisplayMatrix.GpName[i]);

          end;
          SortedList.CustomSort(CompareGroups);
          for i := 0 to SortedList.Count-1 do
            SortedList.Strings[i] := SortedList.Names[i];
       end;

      for i := 0 to nRows - 1 do
      begin
         TheName := SortedList[i];                // Doesn't work if sorted by name twice.  Fix this later.
         D_DisplayMatrix.CurSet[i] := OriginalList.IndexOf(TheName);
      end;
    end;
    if FSearchResult <> -1 then
      FSearchResult := D_DisplayMatrix.FindInCurSet(OldIndx);
    Assert(FSearchResult < D_DisplayMatrix.NoOfRows, Format('invalid FSearchResult = %d', [FSearchResult]));
    DataGrid.Invalidate;
  finally
    begin
      if Assigned(tempDists) then
      begin
        if tempDists.Count > 0 then
          for i := 0 to tempDists.Count - 1 do
            TPairwiseDist(tempDists[i]).Free;
        tempDists.Free;
      end;
      if Assigned(SortedList) then
        SortedList.Free;
      if Assigned(OriginalList) then
        OriginalList.Free;
    end;
  end;
end;


procedure TDisplayMatrixDlg.UpdateColumnWidths(FirstCol: Boolean; OtherCol: Boolean);
var
  i, CurFirstColWidth: Integer;
  AStr: String;
begin
  with DataGrid do
  begin
    CurFirstColWidth := ColWidths[0];
    DefaultRowHeight := Canvas.TextHeight('W') + 4;
    if OtherCol then
      DefaultColWidth := DataGrid.Canvas.TextWidth(Format('%.' + IntToStr(VS_DisplayMatrix.FPrecision + 1) + 'f', [D_DisplayMatrix.MaxD])) + 6;
    if not FirstCol then
    begin
      ColWidths[0] := CurFirstColWidth;  // Have to reset the first col since we just set the DefaultColWidth
      Exit;
    end;

    // otherwise fix up the column width
    VS_DisplayMatrix.MaxRowNamePixelLen := -1;
    for i := 1 to D_DisplayMatrix.NoOfRows do  // as rows are 1 to n
    begin
      AStr := ConstructCol0RowName(i);
      if DataGrid.Canvas.TextWidth(AStr) > VS_DisplayMatrix.MaxRowNamePixelLen then
        VS_DisplayMatrix.MaxRowNamePixelLen := DataGrid.Canvas.TextWidth(AStr);
    end;
    VS_DisplayMatrix.MaxRowNamePixelLen := VS_DisplayMatrix.MaxRowNamePixelLen + 12;
    ColWidths[0] := VS_DisplayMatrix.MaxRowNamePixelLen;
  end;
end;

procedure TDisplayMatrixDlg.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  Action := caFree;
  PrevPairNameTextVisible := ShowPairNameItem.Checked;
  if D_DisplayMatrix.FNoOfGps > 0 then
    PrevShowGpNames := ShowGpNamesItem.Checked;
  if Assigned(FFormObservers) and (FFormObservers.Count > 0) then
    for i := 0 to FFormObservers.Count - 1 do
      FFormObservers[i].RemoveResultsForm(Self);
end;

procedure TDisplayMatrixDlg.DataGridClick(Sender: TObject);
begin
  DataGrid.Invalidate;
end;

procedure TDisplayMatrixDlg.ActionSortByGroupExecute(Sender: TObject);
begin
  SortItemClick(SortGpItem);
end;

procedure TDisplayMatrixDlg.ActionSortAlphabeticalExecute(Sender: TObject);
begin
  SortItemClick(SortByNameItem)
end;

procedure TDisplayMatrixDlg.ActionSortByDistToFocalOtuExecute(Sender: TObject);
begin
  SortItemClick(SortByDistItem);
end;

procedure TDisplayMatrixDlg.ActionMoveToBottomExecute(Sender: TObject);
begin
  if FMouseGridCoords.Y < DataGrid.RowCount then
    DataGridRowMoved(Sender, FMouseGridCoords.Y, D_DisplayMatrix.NoOfRows);
end;

procedure TDisplayMatrixDlg.ActionMoveToTopExecute(Sender: TObject);
begin
  if FMouseGridCoords.Y > 1 then
    DataGridRowMoved(Sender, FMouseGridCoords.Y, 1);
end;

procedure TDisplayMatrixDlg.ActionRestoreInputOrderExecute(Sender: TObject);
begin
  SortItemClick(SortOriginalItem);
end;

procedure TDisplayMatrixDlg.DataGridColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if not IsColumn then
    DataGridRowMoved(Sender, sIndex, tIndex);
end;

procedure TDisplayMatrixDlg.DataGridContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  coords: TPoint;
begin
  coords := ClientToScreen(MousePos);
  FMouseGridCoords := DataGrid.MouseToCell(MousePos);
  if FMouseGridCoords.X = 0 then
    NamesColumnPopupMenu.PopUp(coords.X, coords.Y);
  Handled := True
end;

procedure TDisplayMatrixDlg.DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  delta: Integer = -1;
  cellCoord: Integer = -1;
begin
  try
    FSelfScrolling := True;
    if (ssShift in Shift) or (ssCtrl in Shift) then
    begin
      if Key = VK_LEFT then
      begin
        delta := DataGrid.Col - DataGrid.LeftCol;
        cellCoord := DataGrid.LeftCol - GridHorizScrollbar.LargeChange;
        DataGrid.LeftCol := ValidSelectableCol(cellCoord);
        DataGrid.Col := ValidSelectableCol(DataGrid.Col - GridHorizScrollbar.LargeChange + 1);
      end
      else if Key = VK_RIGHT then
      begin
        delta := DataGrid.Col - DataGrid.LeftCol;
        cellCoord := DataGrid.LeftCol + GridHorizScrollbar.LargeChange;
        DataGrid.LeftCol := ValidSelectableCol(cellCoord);
        if delta > 0 then
        begin
          cellCoord := DataGrid.Col + GridHorizScrollbar.LargeChange - 1;
          DataGrid.Col := ValidSelectableCol(cellCoord);
        end
        else
          DataGrid.Col := ValidSelectableCol(DataGrid.LeftCol);
      end
      else if Key = VK_UP then
      begin
        delta := DataGrid.Row - DataGrid.TopRow;
        cellCoord := DataGrid.TopRow - GridVertScrollbar.LargeChange;
        DataGrid.TopRow := ValidSelectableRow(cellCoord);
        cellCoord := DataGrid.Row - GridVertScrollbar.LargeChange + 1;
        if delta > 0 then
          DataGrid.Row := ValidSelectableRow(cellCoord)
        else
          DataGrid.Row := DataGrid.TopRow;
      end
      else if Key = VK_DOWN then
      begin
        delta := DataGrid.Row - DataGrid.TopRow;
        cellCoord := DataGrid.TopRow + GridVertScrollbar.LargeChange;
        DataGrid.TopRow := ValidSelectableRow(cellCoord);
        if delta > 0 then
        begin
          cellCoord := DataGrid.Row + GridVertScrollbar.LargeChange - 1;
          DataGrid.Row := ValidSelectableRow(cellCoord);
        end
        else
        begin
          cellCoord := DataGrid.TopRow;
          DataGrid.Row := ValidSelectableRow(cellCoord);
        end;
      end;
    end
    else
      inherited;
  finally
    FSelfScrolling := False;
  end;
end;

procedure TDisplayMatrixDlg.DataGridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  if (not ColVisible(GridHorizScrollbar.Position)) or (not ColVisible(aCol)) then
  begin
    if aCol < GridHorizScrollbar.Position then
      GridHorizScrollbar.Position := ValidSelectableCol(aCol)
    else if aCol > GridHorizScrollbar.Position then
      GridHorizScrollbar.Position := ValidSelectableCol(max(1, aCol - DataGrid.VisibleColCount));
  end;

  if (not RowVisible(GridVertScrollbar.Position)) or (not RowVisible(aRow)) then
  begin
    if (aRow < GridVertScrollbar.Position) then
      GridVertScrollbar.Position := ValidSelectableRow(aRow)
    else if (aRow > GridVertScrollbar.Position) then
      GridVertScrollbar.Position := ValidSelectableRow(max(1, aRow - DataGrid.VisibleRowCount));
  end;
  DataGrid.Invalidate;
end;

procedure TDisplayMatrixDlg.FindFirstActionExecute(Sender: TObject);
var
  searchQuery: String = '';
  alreadyOpen: Boolean = False;
begin
  UnhighlightSearchResult;
  if not ActionToggleSearchTools.Checked then
    ActionToggleSearchToolsExecute(Sender)
  else
    alreadyOpen := True;

  if SearchEdit.Text <> EmptyStr then
  begin
    searchQuery := Trim(SearchEdit.Text);
    if searchQuery = EmptyStr then
    begin
      ShowMessage('Please enter a query to search for');
      Exit;
    end;

    FSearchResult := FindTaxon(SearchEdit.Text, 0, True);
    if FSearchResult = -1 then
      ShowMessage(Format('Query "%s" not found', [SearchEdit.Text]));
  end
  else
  begin
    if alreadyOpen then
      ShowMessage('Please enter a query to search for');
    if SearchEdit.Visible then
      SearchEdit.SetFocus;
  end;
  Assert(FSearchResult < D_DisplayMatrix.NoOfRows, Format('invalid FSearchResult = %d', [FSearchResult]));
end;

procedure TDisplayMatrixDlg.FindNextActionExecute(Sender: TObject);
var
  startRow: Integer = 0;
  searchQuery: String = '';
  alreadyOpen: Boolean = False;
begin
  UnhighlightSearchResult;
  if not ActionToggleSearchTools.Checked then
    ActionToggleSearchToolsExecute(Sender)
  else
    alreadyOpen := True;

  if SearchEdit.Text <> EmptyStr then
  begin
    searchQuery := Trim(SearchEdit.Text);
    if searchQuery = EmptyStr then
    begin
      ShowMessage('Please enter a query to search for');
      Exit;
    end;

    if DataGrid.Row = (DataGrid.RowCount - 1) then
    begin
      if (MessageDlg('Already at the last row. Do you want to search from the beginning?', mtConfirmation, [mbYes, mbNo],0) = mrYes) then
        FSearchResult := FindTaxon(SearchEdit.Text, 0, True);
      Exit;
    end;
    if DataGrid.Row >= 1 then
      startRow := min(DataGrid.RowCount - 2, DataGrid.Row);
    FSearchResult := FindTaxon(SearchEdit.Text, startRow, True);
  end
  else
  begin
    if alreadyOpen then
      ShowMessage('Please enter a query to search for');
    if SearchEdit.Visible then
      SearchEdit.SetFocus;
  end;
  Assert(FSearchResult < D_DisplayMatrix.NoOfRows, Format('invalid FSearchResult = %d', [FSearchResult]));
end;

procedure TDisplayMatrixDlg.FIndPreviousActionExecute(Sender: TObject);
var
  startRow: Integer = 0;
  searchQuery: String = '';
  alreadyOpen: Boolean = False;
begin
  UnhighlightSearchResult;
  if not ActionToggleSearchTools.Checked then
    ActionToggleSearchToolsExecute(Sender)
  else
    alreadyOpen := True;

  if SearchEdit.Text <> EmptyStr then
  begin
    searchQuery := Trim(SearchEdit.Text);
    if searchQuery = EmptyStr then
    begin
      ShowMessage('Please enter a query to search for');
      Exit;
    end;

    if DataGrid.Row = 1 then
    begin
      if (MessageDlg('Already at the first row. Do you want to search from the end?', mtConfirmation, [mbYes, mbNo],0) = mrYes) then
        FSearchResult := FindTaxon(SearchEdit.Text, DataGrid.RowCount - 2, False);
      Exit;
    end;

    if DataGrid.Row >= 2 then
      startRow := DataGrid.Row - 2
    else
      startRow := DataGrid.RowCount - 2;
    FSearchResult := FindTaxon(SearchEdit.Text, startRow, False);
  end
  else
  begin
    if alreadyOpen then
      ShowMessage('Please enter a query to search for');
    if SearchEdit.Visible then
      SearchEdit.SetFocus;
  end;
  Assert(FSearchResult < D_DisplayMatrix.NoOfRows, Format('invalid FSearchResult = %d', [FSearchResult]));
end;

procedure TDisplayMatrixDlg.DataGridDrawCell(Sender: TObject; ACol, ARow: Longint; aRect: TRect; State: TGridDrawState);
const
  PADDING = ' ';
var
  d: Double;
  InvalidNum: Boolean = False;
  IsStdErrDisp: Boolean = False;
  IsTriangular: Boolean = False;
  aTextStyle: TTextStyle;
begin
  if not Visible then Exit;
  if (ACol > D_DisplayMatrix.NoOfCols+1) or (ARow > D_DisplayMatrix.NoOfRows+1) then // cell is outside the range
    Exit;
  with DataGrid.Canvas do
  begin
    DispStr := '';
    aTextStyle := TextStyle;
    aTextStyle.Layout := tlCenter;
    Font.Color := clBlack;
    if (ACol = 0) and (ARow = 0) then  // Top-Left cell
    begin
      Brush.Color := DataGrid.FixedColor;
      FillRect(aRect);
    end
    else if (ACol = 0) then  // fixed column (Leftmost); used to display Taxa Labels
    begin
      if (FSearchResult + 1) = ARow then
        Brush.Color := clAqua
      else
        Brush.Color := DataGrid.FixedColor;
      DispStr := ConstructCol0RowName(ARow);
      FillRect(aRect);
      aTextStyle.Alignment := taLeftJustify;
      TextStyle := aTextStyle;
      TextRect(aRect, aRect.Left+2, aRect.Top + 2, DispStr, TextStyle);
    end
    else if (ARow = 0) then  // fixed row (Top Row);
    begin
      DispStr := D_DisplayMatrix.ColName[ACol-1];
      if (FSearchResult + 1) = ACol then
        Brush.Color := clAqua
      else
        Brush.Color := DataGrid.FixedColor;
      FillRect(aRect);
      aTextStyle.Alignment := taCenter;
      TextStyle := aTextStyle;
      TextRect(aRect, (aRect.Left+aRect.Right) div 2, aRect.Top + 2, DispStr, TextStyle);
    end
    else if (ARow > 0) and (ACol > 0) then  // if a cell with data
    begin
      IsTriangular := (D_DisplayMatrix.FComputationType = gdPairwise) or
                      (D_DisplayMatrix.FComputationType = gdBetweenGroupMean) or
                      (D_DisplayMatrix.FComputationType = gdNetGroupMean);

      if gdSelected in State then
      begin
        Brush.Color := clHighlight;
        Font.Color  := clHighlightText;
      end
      else
      begin
        if ((FSearchResult + 1) = ARow) or ((FSearchResult + 1) = ACol) then
        begin
          if IsTriangular then
          begin
            if D_DisplayMatrix.IsLowerLeft and (ARow > ACol) then
              Brush.Color := clAqua
            else if (not D_DisplayMatrix.IsLowerLeft) and (ACol > ARow) then
              Brush.Color := clAqua
            else
              Brush.Color := clWhite;
          end
          else
            Brush.Color := clAqua;
        end
        else
          Brush.Color := clWhite;
      end;

      if (ARow = ACol) and IsTriangular then
      begin
        FillRect(aRect);
        DispStr := EmptyStr;
      end
      else if D_DisplayMatrix.FOnlyDistance and IsTriangular and
              ( ((    D_DisplayMatrix.IsLowerLeft) and (ARow <= ACol)) or
                ((not D_DisplayMatrix.IsLowerLeft) and (ARow >= ACol)) ) then
      begin
        FillRect(aRect);
        DispStr := EmptyStr;
      end
      else
      begin
        d := D_DisplayMatrix.CellContent(ARow, ACol, InvalidNum, IsStdErrDisp);
        if D_DisplayMatrix.FHasTestResult and not InvalidNum then
        begin
          if (abs(d) <= 0.05) and not (gdSelected in State) and (not IsStdErrDisp) then
            if d < 0 then  Brush.Color := clSilver
            else           Brush.Color := clYellow;
          if not D_DisplayMatrix.FAllowNeg then
             d:= abs(d);
        end;

        if (gdSelected in State) or (gdFocused in State) then
          Font.Color := clWhite
        else if IsStdErrDisp then
          Font.Color := clNavy
        else
          Font.Color := clBlack;

        if InValidNum then
        begin
          if gdSelected in State then
            Brush.Color := clBlue
          else
            Brush.Color := clRed;
          Font.Color := clWhite;
          FillRect(aRect);
          DispStr := 'n/c';
          aTextStyle.Alignment := taCenter;
          TextStyle := aTextStyle;
          TextRect(aRect, aRect.Right-2, aRect.Top+2, DispStr, TextStyle);
        end
        else if VS_DisplayMatrix.FPrecision = 0 then
        begin
          FillRect(aRect);
          DispStr := IntToStr(Round(d)) + PADDING;  // Round's off, not floor it
          aTextStyle.Alignment := taRightJustify;
          TextStyle := aTextStyle;
          TextRect(aRect, aRect.Right-2, aRect.Top+2, DispStr, TextStyle);  // used to be right-2
        end
        else
        begin
          FillRect(aRect);
          DispStr := FloatToStrF(d, ffFixed, VS_DisplayMatrix.FPrecision+4, VS_DisplayMatrix.FPrecision) + PADDING;
          aTextStyle.Alignment := taRightJustify;
          TextStyle := aTextStyle;
          TextRect(aRect, aRect.Right-2, aRect.Top+2, DispStr, TextStyle);
        end;
      end;
    end;

    if (gdFocused in State) then
    begin
      DrawFocusRect(aRect);
      if Length(DispStr) = 0 then
        VS_DisplayMatrix.CurStatusBar.Strings[0] := EmptyStr
      else if IsStdErrDisp then
        VS_DisplayMatrix.CurStatusBar.Strings[0] := D_DisplayMatrix.FVAcronym
      else
        VS_DisplayMatrix.CurStatusBar.Strings[0] := D_DisplayMatrix.FDAcronym;

      if (ARow > 0) and (ACol > 0) then
        UpdateStatusBar(ARow, ACol)
    end;
  end;
end;

function TDisplayMatrixDlg.FormHelp(Command: Word; Data: PtrInt; var CallHelp: Boolean): Boolean;
var
  helpTopic: String;
begin
  try
    Result := True;
    helpTopic := MapHelpContextToKeyword(Data);
    CallHelp := False;
    if Trim(helpTopic) <> EmptyStr then
      ShowContextSensitiveHelp(helpTopic)
    else
      ShowContextSensitiveHelp(HelpKeyword);
  except
    on E:Exception do
      ShowMessage('Oh no! Failed to initialize the help browser: ' + E.Message);
  end;
end;

procedure TDisplayMatrixDlg.FormResize(Sender: TObject);
begin
  InitScrollbars(False);
  DataGrid.Invalidate;
end;

procedure TDisplayMatrixDlg.HideSearchToolsActionExecute(Sender: TObject);
begin
  if ActionToggleSearchTools.Checked then
    ActionToggleSearchToolsExecute(Sender);
end;

procedure TDisplayMatrixDlg.MainMenu1DrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  MegaForm.DrawMenuItem(Sender, ACanvas, ARect, AState);
end;

procedure TDisplayMatrixDlg.MainMenu1MeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MegaForm.MeasureMenuItem(Sender, ACanvas, AWidth, AHeight);
end;

procedure TDisplayMatrixDlg.GridHorizScrollbarChange(Sender: TObject);
var
  mouseLeftButtonDown: Boolean = False;
begin
  if FSelfScrolling then
    Exit;
  try
    { begin workaround to stop the scrollbar from chasing the cursor when the mouseup event is lost}
    mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
    if (not mouseLeftButtonDown) and FScrollStarted then
    begin
      SendMessage(GridHorizScrollbar.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      FScrollStarted := False;
      Exit;
    end;
    { end workaround}

    FHScrollPos := GridHorizScrollbar.Position;
    if (FHScrollPos >= DataGrid.FixedCols) and (FHScrollPos <= DataGrid.ColCount) then
    begin
      DataGrid.LeftCol := min(FHScrollPos, DataGrid.ColCount - DataGrid.VisibleColCount);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TDisplayMatrixDlg.GridHorizScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  mouseLeftButtonDown: Boolean = False;
begin
  mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
  if (not mouseLeftButtonDown) and FScrollStarted then
    ScrollPos := FHScrollPos;
  if ScrollCode = scEndScroll then
    FScrollStarted := False;
  inherited;
end;

procedure TDisplayMatrixDlg.GridVertScrollbarChange(Sender: TObject);
var
  mouseLeftButtonDown: Boolean = False;
begin
  if FSelfScrolling then Exit;

  try
    { begin workaround to stop the scrollbar from chasing the cursor when the mouseup event is lost}
    mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
    if (not mouseLeftButtonDown) and FScrollStarted then
    begin
      SendMessage(GridVertScrollbar.Handle, WM_CANCELMODE, WM_CANCELMODE, 0);
      FScrollStarted := False;
      Exit;
    end;
    { end workaround}
    FVScrollPos := GridVertScrollbar.Position;
    if (FVScrollPos >= DataGrid.FixedRows) and (FVScrollPos <= DataGrid.RowCount) then
    begin
      DataGrid.TopRow := min(FVScrollPos, DataGrid.RowCount - DataGrid.VisibleRowCount);
      FScrollStarted := True;
    end;
  except
    FScrollStarted := False;
  end;
end;

procedure TDisplayMatrixDlg.GridVertScrollbarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  mouseLeftButtonDown: Boolean = False;
begin
  mouseLeftButtonDown := ((GetKeyState(VK_LBUTTON) and $8000) > 0);
  if (not mouseLeftButtonDown) and FScrollStarted then
    ScrollPos := FVScrollPos;

  if ScrollCode = scEndScroll then
    FScrollStarted := False;
  inherited;
end;

procedure TDisplayMatrixDlg.SearchEditChange(Sender: TObject);
begin
  if FSearchResult <> -1 then
  begin
    FSearchResult := -1;
    DataGrid.Invalidate;
  end;
end;

procedure TDisplayMatrixDlg.SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (SearchEdit.Text <> EmptyStr) then
    FindFirstActionExecute(Sender);
end;

procedure TDisplayMatrixDlg.ShowAnalysisSummaryActionExecute(Sender: TObject);
begin
  if Assigned(D_DisplayMatrix.AnalysisSummary) then
    OpenStringList(D_DisplayMatrix.AnalysisSummary, 'Analysis Summary', True)
  else
    ShowMessage('Application Error: analysis summary is missing');
end;

procedure TDisplayMatrixDlg.UpdateStatusBar(ARow, ACol: Integer);
var
  AStr: String;
begin
  AStr := '['+IntToStr(ARow)+','+IntToStr(ACol)+']';
  if (ShowPairNameItem.Enabled and ShowPairNameItem.Checked) then
    AStr := AStr + ' ('+D_DisplayMatrix.RowName[ARow-1]+'-'+D_DisplayMatrix.RowName[ACol-1]+')';
  VS_DisplayMatrix.CurStatusBar.Strings[1] := AStr + ' / ' +D_DisplayMatrix.FFullName;  // IMPORTANT FOR VISUAL BUILD
end;

procedure TDisplayMatrixDlg.ShowPairNameItemClick(Sender: TObject);
begin
  ShowPairNameItem.Checked := not ShowPairNameItem.Checked;
  ShowPairNameBtn.Down       := ShowPairNameItem.Checked;
  UpdateStatusBar(DataGrid.Row, DataGrid.Col);
end;

// Showing description of the analysis
procedure TDisplayMatrixDlg.ShowDescriptionItemClick(Sender: TObject);
var
  i: Integer;
  Position: Integer;
begin
  for i := 0 to D_DisplayMatrix.FDescription.Count - 1 do
  begin
    Position := Pos('[fsBold', D_DisplayMatrix.FDescription[i]);
    if Position > 0 then
      D_DisplayMatrix.FDescription[i] := Copy(Trim(D_DisplayMatrix.FDescription[i]),1, Length(D_DisplayMatrix.FDescription[i]) - 8);
  end;
  OpenStringList(D_DisplayMatrix.FDescription, 'Analysis options');
end;

// Showing title of the input data file
procedure TDisplayMatrixDlg.ShowTitleItemClick(Sender: TObject);
begin
  MessageDlg('Title: '+D_DisplayMatrix.FTitle, mtInformation, [mbOK],0);
end;

// format the data by lower or upper right
procedure TDisplayMatrixDlg.FormatSBtnClick(Sender: TObject);
var
  PrevCol: Integer;
begin
  LowerLeftAction.Checked := False;
  UpperRightAction.Checked := False;
  with Sender as TAction do
    Checked := True;
  D_DisplayMatrix.IsLowerLeft := LowerLeftAction.Checked;
  with DataGrid do
  begin
    PrevCol := Col;
    Col     := Row;
    Row     := PrevCol;
    Invalidate;
  end;
end;

procedure TDisplayMatrixDlg.ComputeLeftOfDecimalLen; //need to leave it here so it can access visual font info
var
  IntMaxD: Integer;
begin
  with DataGrid, Canvas do
  begin
    VS_DisplayMatrix.LeftOfDecimalLen := TextWidth('0');
    IntMaxD := Floor(D_DisplayMatrix.MaxD/10);
    while IntMaxD <> 0 do
    begin
      IntMaxD := IntMaxD div 10;
      VS_DisplayMatrix.LeftOfDecimalLen := VS_DisplayMatrix.LeftOfDecimalLen + TextWidth('0');
    end;
  end;
end;

function TDisplayMatrixDlg.GetAnalysisSummary: TStringList;
begin
  Result := D_DisplayMatrix.AnalysisSummary;
end;

procedure TDisplayMatrixDlg.SetAnalysisSummary(AValue: TStringList);
begin
  D_DisplayMatrix.AnalysisSummary := AValue;
end;

procedure TDisplayMatrixDlg.SetBestPrecisionForDisplay;
var
  aMin: Double = 0;
begin
  aMin := D_DisplayMatrix.MinD;
  if aMin >= 1 then
    FPrecision := 2
  else if aMin > 0.1 then
    FPrecision := 3
  else if aMin > 0.01 then
    FPrecision := 4
  else if aMin > 0.001 then
    FPrecision := 5
  else if aMin > 0.0001 then
    FPrecision := 6
  else if aMin > 0.00001 then
    FPrecision := 7
  else if aMin > 0.000001 then
    FPrecision := 8
  else if aMin > 0.0000001 then
    FPrecision := 9
  else if aMin > 0.00000001 then
    FPrecision := 10
  else if aMin > 0.000000001 then
    FPrecision := 11
  else
    FPrecision := 10;
end;

function TDisplayMatrixDlg.NumTaxa: Integer;
begin
  Result := D_DisplayMatrix.NoOfTaxa;
end;

function TDisplayMatrixDlg.NumGroups: Integer;
begin
  Result := D_DisplayMatrix.NoOfGps;
end;

procedure TDisplayMatrixDlg.SaveToFileItemClick(Sender: TObject);
{$IFDEF VISUAL_BUILD}
var
  OutputMatrixDlg: TOutputMatrixDlg = nil;
  ExportOptions : TDistExportOptions = nil;
  Results : TStringList = nil;
  {$ENDIF}
begin
{$IFDEF VISUAL_BUILD}
 try try
    OutputMatrixDlg  := TOutputMatrixDlg.Create(nil);
    OutputMatrixDlg.NoOfRows := D_DisplayMatrix.NoOfRows;
    OutputMatrixDlg.NoOfCols := D_DisplayMatrix.NoOfCols;
    OutputMatrixDlg.HelpContext := HC_Dist_Exp_Options_dialog_box;
    OutputMatrixDlg.AllowStandardError := (not D_DisplayMatrix.OnlyDistance);
    OutputMatrixDlg.AllowMEGAFormat := True;

    case D_DisplayMatrix.ComputationType of
      gdPairwise:
        begin
          if D_DisplayMatrix.NoOfTaxa < 3 then
            OutputMatrixDlg.AllowMEGAFormat := False;
        end;
      gdBetweenGroupMean, gdNetGroupMean:
        begin
          if D_DisplayMatrix.NoOfGps < 3 then
            OutputMatrixDlg.AllowMEGAFormat := False;
        end;
      gdWithinGroupMean:
        begin
          OutputMatrixDlg.AllowMEGAFormat := False;
          OutputMatrixDlg.IsColumnar := True;
        end;
    end;

    with OutputMatrixDlg do
    begin
      AllowNegative := D_DisplayMatrix.AllowNeg;
      Precision := VS_DisplayMatrix.FPrecision;

      if ShowModal = mrOk then
      begin
        ExportOptions := OutputMatrixDlg.GetExportOptions;
        Results := D_DisplayMatrix.WriteExportToFile(ExportOptions);
        if (ExportOptions.OutputFormat in [ExportExcel, ExportExelXML, ExportODS]) and ExcelWrite.hasExcel then
          RunAProgram(ExportOptions.SaveExcelFileTo)
        else if ExportOptions.OutputFormat <> ExportExcel then
          OpenStringList(Results, 'Distance Data');
      end;
    end;
  except
    On E: Exception do
    begin
      ShowMessage('Application error when exporting results: ' + E.Message)
    end;
  end;
  finally
    if Assigned(OutputMatrixDlg) then
      OutputMatrixDlg.Free;
   if Assigned(ExportOptions) then
     ExportOptions.Free;
   if Assigned(Results) then
     Results.Free;
  end;
 {$ENDIF}
end;

procedure TDisplayMatrixDlg.setDisallowMatrix(value: Boolean);
begin
  disallowMatrixExport := value;
end;

procedure TDisplayMatrixDlg.AddFormObserver(observer: IFormObserver);
begin
  if FFormObservers.IndexOf(observer) < 0 then
    FFormObservers.Add(observer);
end;

procedure TDisplayMatrixDlg.FontItemClick(Sender: TObject);
begin
  FontDialog.Font.Assign(DataGrid.Canvas.Font);
  if FontDialog.Execute then
    with DataGrid do
    begin
      FFontSize := FontDialog.Font.Size;
      DataGrid.Font.Assign(FontDialog.Font);
      DataGrid.Canvas.Font.Assign(FontDialog.Font);
      UpdateColumnWidths(True,True);
      DataGrid.Invalidate;
    end;
end;

procedure TDisplayMatrixDlg.QuitItemClick(Sender: TObject);
begin
  VS_DisplayMatrix.ShouldClose := True;
  if CloseQuery then
    Close;
end;

procedure TDisplayMatrixDlg.DataGridRowMoved(Sender: TObject; FromIndex, ToIndex: Longint);
var
  aId: Integer = -1;
begin
  if FSearchResult >= 0 then
    aId := D_DisplayMatrix.CurSet[FSearchResult];
  D_DisplayMatrix.MoveRow(FromIndex, ToIndex);
  FSearchResult := D_DisplayMatrix.FindInCurSet(aId);
  Assert(FSearchResult < D_DisplayMatrix.NoOfRows, Format('invalid FSearchResult = %d', [FSearchResult]));
  DataGrid.Invalidate;
end;

procedure TDisplayMatrixDlg.AvgAllItemClick(Sender: TObject);
begin
  MessageDlg('The overall average is '+
     FloatToStrF(D_DisplayMatrix.getAvgAll,ffFixed,VS_DisplayMatrix.FPrecision+4,VS_DisplayMatrix.FPrecision),mtInformation, [mbOK], 0);
end;

procedure TDisplayMatrixDlg.GpsAvgItemClick(Sender: TObject);
var
  DispDlg: TD_DisplayMatrix = nil;
  DispDlgVS: TVS_DisplayMatrix = nil;
  DispDlgV: TDisplayMatrixDlg = nil;
  MyGpNameList: TStringList = nil;
  MyD: PDistanceMatrix = nil;
  MyDComps: PDistanceMatrix = nil;
  MyMeanGpD : ArrayOfDouble; // within gp means
  MyComputeType: TDistType;
  aCaption: String;
  i: Integer;
begin
 SetLength(MyMeanGpD, 0);
  // does not do double matrix; only for the viewed stuff
 try try
   if Sender is TAction then
     aCaption := TAction(Sender).Caption
   else if Sender is TMenuItem then
     aCaption := TMenuItem(Sender).Caption
   else
     raise Exception.Create('Application Error: failed to detect computation type');

  if Pos('Within Groups', aCaption) > 0 then
    MyComputeType := gdWithinGroupMean
  else if Pos('Net Between Groups', aCaption) > 0 then
    MyComputeType := gdNetGroupMean
  else if Pos('Between Groups', aCaption) > 0 then
    MyComputeType := gdBetweenGroupMean
  else if Pos('For Vertebrates', aCaption) > 0 then
    MyComputeType := gdBetweenGroupMean
  else
    raise Exception.Create('Application Error: invalid call to GpsAvgItemClick');


  MyGpNameList := TStringList.Create;
  D_DisplayMatrix.getGpsAvg(MyComputeType, MyGpNameList, MyD, MyDComps, MyMeanGpD);

    // now prepare to display
    DispDlg := TD_DisplayMatrix.Create;
    DispDlgVS := TVS_DisplayMatrix.Create;
    DispDlgV := TDisplayMatrixDlg.Create(Application);

    DispDlgV.LinkDataClass(DispDlg);
    DispDlgV.LinkVisualStatusClass(DispDlgVS);
    DispDlg.LinkVisualStatusClass(DispDlgVS);

    DispDlg.Title := D_DisplayMatrix.FTitle;
    DispDlg.FigureGenerator.LoadTemplateFromFile('Distance_Matrix.htm'); // Can not create a caption for this since we don't have the MAI for this specific analysis (Only pairwise computation if computed, or none at all if using a MEGA dist file)
    DispDlg.Description := D_DisplayMatrix.FDescription;
    DispDlg.DAcronym := D_DisplayMatrix.FDAcronym;
    DispDlg.VAcronym := D_DisplayMatrix.FVAcronym;
    DispDlg.FullName:= D_DisplayMatrix.FFullName;
    DispDlg.ComputationType := MyComputeType;
    DispDlg.NoOfGps:= MyGpNameList.Count; //this is the best estimate FNoOfGps;
    for i:=0 to MyGpNameList.Count-1 do
      DispDlg.GpName[i] := MyGpNameList[i];

    FreeAndNil(MyGpNameList);

    case MyComputeType of
      gdBetweenGroupMean, gdNetGroupMean:
        begin
          DispDlg.DistMat := MyD;
          MyD := nil; // relinquish ownership
        end;
      gdWithinGroupMean:
        begin
          DispDlg.DistArray := MyMeanGpD;
          MyMeanGpD := nil;
        end;
      end;


     case MyComputeType of
       gdBetweenGroupMean: DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Avg. Between groups';
       gdNetGroupMean    : DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Net distance between groups (from Pairwise display)';
       gdWithinGroupMean : DispDlgV.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Avg. within groups';
     end;
     DispDlgV.CaptionBtn.Visible := False; //

    DispDlg.Initialize;
    DispDlg.FigureGenerator := nil; // we do not have access to MAI and DistPack here
    DispDlgV.CaptionBtn.Enabled := False;
    DispDlgV.miGenerateCaption.Visible := False;
    DispDlgV.AverageMenu.Visible := False;
    DispDlgV.Show;
  except
    On E: Exception do
    begin
      if DispDlgV <> nil then
      begin
        DispDlgV.Hide;
        FreeAndNil(DispDlgV);
      end;
      FreeAndNil(DispDlg);
      FreeAndNil(DispDlgVS);
      FreeAndNil(MyGpNameList);
      ShowMessage(E.Message);
    end;
  end
  Finally
	(*if DispDlg <> nil then
    	FreeAndNil(DispDlg);
	if DispDlgVS <> nil then
    	FreeAndNil(DispDlgVS); *)
  end;
end;

{
// sizing display and columns
procedure TDisplayMatrixDlg.DistCellSzTrackBarChange(Sender: TObject);
var
  i: Integer;
begin
  with DataGrid do
    for i:=1 to ColCount-1 do
      ColWidths[i] := DistCellSzTrackBar.Position*5;
end;
}

{
procedure TDisplayMatrixDlg.DistCellSzTrackBarChange(Sender: TObject);
var
  i: Integer;
begin
  with DataGrid do
    for i:=1 to ColCount-1 do
      ColWidths[i] := DistCellSzTrackBar.Position*5;
end;
}

// Precision is handled here
procedure TDisplayMatrixDlg.DecPrecisionBtnClick(Sender: TObject);
begin
  if (VS_DisplayMatrix.FPrecision <= 0) then
    Exit;
  VS_DisplayMatrix.DecPrecision;
  DecPrecisionAction.Enabled := (VS_DisplayMatrix.FPrecision > 0);
  IncPrecisionAction.Enabled := (VS_DisplayMatrix.FPrecision < 10);
  UpdateColumnWidths(False, True);
  DataGrid.Invalidate;
  FPrecision := VS_DisplayMatrix.FPrecision;
end;


procedure TDisplayMatrixDlg.IncPrecisionBtnClick(Sender: TObject);
begin
  if VS_DisplayMatrix.FPrecision >= 10 then
    Exit;
  VS_DisplayMatrix.IncPrecision;
  DecPrecisionAction.Enabled := (VS_DisplayMatrix.FPrecision > 0);
  IncPrecisionAction.Enabled := (VS_DisplayMatrix.Precision < 10);
  UpdateColumnWidths(False, True);
  DataGrid.Invalidate;
  FPrecision := VS_DisplayMatrix.FPrecision;
end;

// Group and taxa names
procedure TDisplayMatrixDlg.ShowGpNamesItemClick(Sender: TObject);
begin
  ShowGpNamesItem.Checked := not ShowGpnamesItem.Checked;
  UpdateColumnWidths(True, False);
  DataGrid.Invalidate;
end;

procedure TDisplayMatrixDlg.ShowTaxaNamesItemClick(Sender: TObject);
begin
  ShowTaxaNamesItem.Checked := not ShowTaxaNamesItem.Checked;
  UpdateColumnWidths(True, False);
  DataGrid.Invalidate;
end;

procedure TDisplayMatrixDlg.AssignHelpContext;
begin
  FileMenu.HelpContext            := HC_Dist_Matrix_Exp_File_Menu;
  ShowTitleItem.HelpContext     := HC_Dist_Matrix_Exp_File_Menu;
  ShowDescriptionItem.HelpContext := HC_Dist_Matrix_Exp_File_Menu;
  SaveToFileItem.HelpContext    := HC_Dist_Matrix_Exp_File_Menu;
  QuitItem.HelpContext          := HC_Dist_Matrix_Exp_File_Menu;

  DisplayMenu.HelpContext         := HC_Dist_Matrix_Exp_Display_Menu;
  ShowPairNameItem.HelpContext  := HC_Dist_Matrix_Exp_Display_Menu;
  SortSeqItem.HelpContext   := HC_Dist_Matrix_Exp_Display_Menu;
  SortOriginalItem.HelpContext  := HC_Dist_Matrix_Exp_Display_Menu;
  SortByNameItem.HelpContext := HC_Dist_Matrix_Exp_Display_Menu;
  SortGpItem.HelpContext     := HC_Dist_Matrix_Exp_Display_Menu;
  ShowTaxaNamesItem.HelpContext       := HC_Dist_Matrix_Exp_Display_Menu;
  ShowGpNamesItem.HelpContext         := HC_Dist_Matrix_Exp_Display_Menu;
  FontItem.HelpContext         := HC_Dist_Matrix_Exp_Display_Menu;

  AverageMenu.HelpContext       := HC_Dist_Matrix_Exp_Average_Menu;
  AvgAllItem.HelpContext       := HC_Dist_Matrix_Exp_Average_Menu;
  WithinGpsAvgItem.HelpContext       := HC_Dist_Matrix_Exp_Average_Menu;
  BetweenGpsAvgItem.HelpContext       := HC_Dist_Matrix_Exp_Average_Menu;
  NetBetweenGpsAvgItem.HelpContext       := HC_Dist_Matrix_Exp_Average_Menu;
end;

procedure TDisplayMatrixDlg.HelpMenuClick(Sender: TObject);
begin
  try
    ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
  Except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when initializing the help browser: ' + E.Message);
  end;
end;

procedure TDisplayMatrixDlg.miGenerateCaptionClick(Sender: TObject);
{$IFNDEF NO_BROWSER}
var
  {$IFDEF VISUAL_BUILD}
  aChBrowser : TMegaBrowserFrm;
  Caption : String;
  {$ENDIF}
{$ENDIF}
begin
  try
    // Bind the dialog to see where the standard error values are
    D_DisplayMatrix.FigureGenerator.FlushTemplate(D_DisplayMatrix);
    D_DisplayMatrix.FigureGenerator.BindData(D_DisplayMatrix, True);
    D_DisplayMatrix.FigureGenerator.FlushTemplate(Self);
    D_DisplayMatrix.FigureGenerator.BindData(Self, True);
    Caption := D_DisplayMatrix.FigureGenerator.GenerateLegend;
  except
    on E:Exception do
    begin
      ShowMessage('Sorry. A caption has not been generated for this analysis :(');
      Exit;
    end;
  end;
  {$IFDEF VISUAL_BUILD}
  {$IFNDEF NO_BROWSER}
  aChBrowser := CreateNewChromiumBrowserWindow(bmCaption);
  aChBrowser.LoadHtmlFromString(Caption);
  aChBrowser.SetTargetDimensions(800, 400)
  {$ENDIF}
  {$ENDIF}
end;

procedure TDisplayMatrixDlg.DataGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  DRAG_INCREMENT = 5;
var
  aCol: Integer = -1;
  aRow: Integer = -1;
begin

  if VS_DisplayMatrix.FIsResizing then
    begin
      if X > 15 then // Can't have col 0 smaller than 15 pixels
      DataGrid.ColWidths[0] := X;
    end;

  if (abs(X-DataGrid.ColWidths[0]) < 5) or (VS_DisplayMatrix.FIsResizing) then
  begin
    DataGrid.Cursor := crHSplit;
    DataGrid.Options := DataGrid.Options - [goRowMoving]; //If we show them the split arrow they can split but not move rows otherwise rows are accidentally moved
  end
  else
  begin
    DataGrid.Options := DataGrid.Options + [goRowMoving]; //They are not at a location to resize the first column so allow them to move rows
    DataGrid.Cursor := crArrow;
  end;

  DataGrid.MouseToCell(X, Y, aCol, aRow);
  if (aRow <> FLastRow) and (goRowMoving in DataGrid.Options) then
  begin
    if ssLeft in Shift then
    begin
      if (aRow < FLastRow) and (GridVertScrollbar.Position > 0) then { dragging upward}
      begin
        if abs(aRow - GridVertScrollbar.Position) <= DRAG_INCREMENT then
          GridVertScrollbar.Position := max(0, GridVertScrollbar.Position - 2);
      end
      else if (aRow > FLastRow) and (GridVertScrollbar.Position < GridVertScrollbar.Max) then { dragging downward}
      begin
        if abs(aRow - GridVertScrollbar.Position - GridVertScrollbar.PageSize) <= DRAG_INCREMENT then
          GridVertScrollbar.Position := min(GridVertScrollbar.Max, GridVertScrollbar.Position + 2);
      end;
    end;
  end;
  FLastRow := aRow;
end;

procedure TDisplayMatrixDlg.ShowListofInvalidDistancesItemClick(Sender: TObject);
var
  MoreResults : Boolean = False;
  StartAt : Integer;
  Result : TStringList;
  Offenses : ArrayOfInteger;
begin
  SetLength(Offenses, 0);
try
  StartAt := 1;
  Result := TStringList.Create;
  D_DisplayMatrix.CreateListOfInvalidDistances(MoreResults, StartAt, Offenses, Result);
  if Result.Count = 0 then
    ShowMessage('No invalid distances detected')
  else
  begin
    While MoreResults do
    begin
      if MessageDlg('Already found '+IntToStr(Result.Count)+' pairs. Would you like to see more?', mtConfirmation, [mbYes, mbNo], 0 ) = mrYes then
        D_DisplayMatrix.CreateListOfInvalidDistances(MoreResults, StartAt, Offenses, Result)
      else
        MoreResults := False;
    end;
    D_DisplayMatrix.CreateInvalidDistanceSummary(Offenses, Result);
    OpenStringList(Result, 'List of Invalid Distances');
  end;
  Offenses := nil;
  FreeAndNil(Result);
  except
    FreeAndNil(Result);
    RaiseErrorMessage(HC_Unexpected_Error, 'Unable to show list of invalid distances');
  end;
end;

procedure TDisplayMatrixDlg.FormShow(Sender: TObject);
begin
  VS_DisplayMatrix.FPrecision := FPrecision;
  DecPrecisionAction.Enabled := (VS_DisplayMatrix.FPrecision > 0);
  IncPrecisionAction.Enabled := (VS_DisplayMatrix.FPrecision < 10);
  AddWindowToTray(Self);
end;

procedure TDisplayMatrixDlg.DataGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DataGrid.Cursor = crHSplit then  // Only start resizing, once the cursor is over the correct place and you get a click, this way you can't "slide" into it accidently.
    VS_DisplayMatrix.FIsResizing := true;
end;

procedure TDisplayMatrixDlg.DataGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if VS_DisplayMatrix.FIsResizing then
    VS_Displaymatrix.FIsResizing := false;
 DataGrid.Invalidate;
end;

procedure TDisplayMatrixDlg.ActionToggleSearchToolsExecute(Sender: TObject);
begin
  DataGrid.Invalidate;
  ActionToggleSearchTools.Checked := not ActionToggleSearchTools.Checked;
  SearchPanel.Visible := ActionToggleSearchTools.Checked;
end;

procedure TDisplayMatrixDlg.SetExportOptions(Options: TMatrixExportOptionsSet);
begin
  ExportFormatOptions := Options;

  if SizeOf(Options) < 2 then   // An EMPTY set still has a sizeOf(emptySet) = 1.
  begin
    SaveToFileItem.Visible := False;
    N4.Visible := False;
    ExportDataBtn.Visible := False;
    ToolButton1.Visible := False;
  end;

  if meMega in Options then
    ExportMegaBtn.Visible := True
  else
    ExportMegaBtn.Visible := False;

  if meCsv in Options then
    ExportCsvBtn.Visible := True
  else
    ExportCsvBtn.Visible := False;

  if meExcel in Options then
    ExportXLBtn.Visible := True
  else
    ExportXLBtn.Visible := False;

  if meText in Options then
    ExportTxtBtn.Visible := True
  else
    ExportTxtBtn.Visible := False;
end;

{$ENDIF}

end.

