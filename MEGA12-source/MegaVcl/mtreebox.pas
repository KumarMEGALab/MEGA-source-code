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

unit MTreeBox;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF VISUAL_BUILD}Printers, {$IFDEF MSWINDOWS}Windows,{$ENDIF}{$ENDIF}
  LCLIntF, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,   Clipbrd, Math, Menus, MegaConsts, MOtuInfo, contnrs,
  MTreeData, MTreeList, MPartitionList, MCalibrationData, mancestralstatesexportheader,
  MSimpleTreeNode, FPCanvas, MVirtualScrollbox, MNewickExportOptions, mabstracttreerenderer,
  mpdftreerenderer, msvgtreerenderer, mbitmaptreerenderer, memftreerenderer, MLongintList,
  mtaxa_name_search, mtree_display_settings_cache, fpjson, jsonparser, syncobjs;

const
  SMALL_TREE_NUM_TAXA = 200;
  GROUPS_COMPRESSED_INDEX = 0;
  CLUSTERS_COMPRESSED_INDEX = 1;
  SIMILAR_COMPRESSED_INDEX = 2;
  LENGTH_OF_ROOT_BRANCH = 25;
  DEFAULT_PPOTU = 24;
  GENE_DUP_MARKER_COLOR = clBlue;
  SPECIATION_MARKER_COLOR = clRed;
  {$IFDEF WIN64}
  MAX_BITMAP_DIMENSION = 2147483647; // 262144;
  MAX_TAXA_STRING = '(100,000)';
  {$ELSE}
  MAX_BITMAP_DIMENSION = 2147483647; { 2GB}
  MAX_TAXA_STRING = '(100,000)';
  {$ENDIF}
  BASE_NODE_ATTRUBUTE_INDEX = 0;
  MIN_FONT_SIZE = 6;
  MAX_FONT_SIZE = 96;
  {$IFDEF MSWINDOWS}
  DEFAULT_FONT_NAME = 'Arial';
  {$ELSE}
    {$IFDEF DARWIN}
    DEFAULT_FONT_NAME = 'Arial';
    {$ELSE}
    DEFAULT_FONT_NAME = 'Helvetica';
    {$ENDIF}
  {$ENDIF}

type
  TTreeDrawingTarget = (tdtCanvas, tdtPdf, tdtSvg, tdtEmf);
  TRotateType = (rtNone, rtCounterClockWise, rtClockWise, rtFlip, rtCustom);
  TNodeType = (ntNone, ntNoFocus, ntOTU, ntInterior, ntCompressed, ntHidden, ntRoot);
  TBranchType = (btNone, btNoFocus, btExterior, btRootedExterior, btInterior, btRootedInterior);
  TNodeMarker = record
    Shape: TNodeMarkerShape;
    Color: TColor;
  end;

  {$IFDEF VISUAL_BUILD}

  TTreeCustomControl = class;

  { TNodeInfo }

  TNodeInfo = class
  private
    Tree: TTreeCustomControl;

    function GetDepth: Integer;
    function GetIndex: integer;
    function GetAncIndex: integer;
    function GetDes1Index: integer;
    function GetDes2Index: integer;
    function GetName: AnsiString;
    function GetNodeType: TNodeType;
    function GetHeight: double;
    function GetDivTime: double;
    function GetSize: Integer;
    function GetSpeciesName: AnsiString;
    function GetIsGeneDup: Boolean;
    function GetIsSpeciation: Boolean;
    function GetIsOtu: Boolean;
    function GetIsOutgroup: Boolean;
    function GetDataCoverage: Double;
    procedure SetName(AValue: AnsiString);
  public
    function IsOrHasChildInOutgroup: Boolean;
    property Index: integer read GetIndex;
    property AncIndex: integer read GetAncIndex;
    property Des1Index: integer read GetDes1Index;
    property Des2Index: integer read GetDes2Index;
    property NodeType: TNodeType read GetNodeType;
    property Height: double read GetHeight;
    property DivTime: double read GetDivTime;
    property SpeciesName: AnsiString read GetSpeciesName;
    property IsGeneDup: Boolean read GetIsGeneDup;
    property IsSpeciation: Boolean read GetIsSpeciation;
    property IsOtu: Boolean read GetIsOtu;
    property IsOutgroup: Boolean read GetIsOutgroup;
    property DataCoverage: Double read GetDataCoverage;
    property Size: Integer read GetSize;
    property Depth: Integer read GetDepth;
    property Name: AnsiString read GetName write SetName;
    constructor Create(Source: TTreeCustomControl);
    destructor Destroy; override;
  end;

  { TBranchInfo }

  TBranchInfo = class
  private
    Tree: TTreeCustomControl;

    function GetMeanBcl: Double;
    function GetNodeIndex: integer;
    function GetAncNodeIndex: integer;
    function GetBranchType : TBranchType;
    function GetLength : double;
    function GetStats2: Integer;
    function GetStatsStdDev: Double;
    function GetTotalLength : double;
    function GetSE : double;
    function GetMaxLen1 : double;
    function GetMaxLen2 : double;
    function GetStats : double;
  public
    property NodeIndex: integer read GetNodeIndex;
    property AncNodeIndex: integer read GetAncNodeIndex;
    property BranchType : TBranchType read GetBranchType;
    property Length : double read GetLength;
    property TotalLength : double read GetTotalLength;
    property SE : double read GetSE;
    property StatsStdDev: Double read GetStatsStdDev;
    property MaxLen1 : double read GetMaxLen1;
    property MaxLen2 : double read GetMaxLen2;
    property Stats : double read GetStats;
    property Stats2: Integer read GetStats2;
    property MeanBcl: Double read GetMeanBcl;
    constructor Create(Source: TTreeCustomControl);
    destructor Destroy; override;
  end;
  {$ENDIF VISUAL_BUILD}

  TBranch = record
      length : double;
      SE : double;
      StatsStdDev: Double;
      maxlen1, maxlen2 : double;
      bootnum : integer;
      stats : double;
      stat2 : integer;
      meanBcl: Double;
  end;

  TpNode  = ^TNode;
  TNode   = record
      index : integer;
      anc : TpNode;
      des1, des2 : TpNode;
      size : integer;
      cursize : double;
      depth : integer;
      width : integer;

      height : double;
      vh     : double;
      maxh: double;
      minh: double;
      rate: double;
      h0     : double;
      branch : TBranch;
      minOTU : integer;
      position : TPoint;
      angle : double;
      sector: double;
      avglen: double;
      name : AnsiString;
      PrivateName: AnsiString;
      SpeciesName: AnsiString;
      oriName : AnsiString;
      marker : TNodeMarker;
      charstate : AnsiString;
      OTU : boolean;
      compressed : boolean;
      autoCompressed: Boolean;
      outgroup: boolean;
      tempOutgroup: Boolean; { when the rooting tool in Tree Explorer is used, mark smaller clade as tempOutgroup - if we have multiple trees, propogate this and root all the trees on this outgroup}
      hidden : boolean;
      flag: boolean;
      hilighted: boolean;
      emphasized: boolean; // then draw with emphasis
      CustomHighlightColor: TColor; // If highlighted color differs from normal highlight color
      attrindex: integer;
      namesize: TPoint;
      speciesNameSize: TPoint;
      dx: integer;
      groupindex: integer;
      compressedIndex: Integer;
      bracket: TRect;
      capdepth: integer;
      IsGeneDuplication: Boolean;
      IsSpeciationEvent: Boolean;
      dataCoverage: Double;
      otuInfo: POtuInfo;
      sortValue: LongWord;
      sortString: AnsiString;
      tempValue: Extended; { a temporary placeholder for calculating various things that are not needed to persist}
  end;
  {$IFNDEF DARWIN}
  {$IFDEF CPU32}
  TpNodeArray = array[1..65536] of TpNode;
  {$ELSE}
  TpNodeArray = array[1..MaxInt] of TpNode;
  {$ENDIF}
  {$ELSE}
  TpNodeArray = array[1..65536] of TpNode;
  {$ENDIF}

  {$IFDEF VISUAL_BUILD}

  { TNodeAttrib }

  TNodeAttrib = class
    FNodeIndex: integer;
    Caption: AnsiString;
    Marker: TNodeMarker;
    OverwriteMarker: boolean;
    OverwriteDownstream: boolean;
    GraphicAlign: TGraphicAlign;
    Name: AnsiString;
  private
    CaptionSize: integer;
    FAutoCompressed: Boolean;
    FIsTemporaryAttribute: Boolean;
    FLinePen: TPen;
    FBracketPen: TPen;
    FFont: TFont;
    FCaptionFont: TFont;
    FBrush: TBrush;
    FManualCompressed: Boolean;
    FShowTaxonName: boolean;
    FShowSpeciesName: boolean;
    FShowTaxonMarker: boolean;
    FShowNodeMarker:  boolean;
    FShowCaption: boolean;
    FShowBracket: boolean;
    FShowImage: boolean;
    FBranchOption: TBranchOption;
    FBracketStyle: TBracketStyle;

    Bitmap: TBitmap;
    JPEGImage: TJPEGImage;
    IsJPEG: boolean;
    FImageScaled: boolean;

    function GetFont: TFont;
    function GetManualCompressed: Boolean;
    procedure SetAutoCompressed(AValue: Boolean);
    procedure SetIsTemporaryAttribute(AValue: Boolean);
    procedure SetManualCompressed(AValue: Boolean);
    procedure SetFont(newfont: TFont);
    procedure SetCaptionFont(newfont: TFont);
    procedure SetLineColor(value: TColor);
    function GetLineColor: TColor;
    procedure SetLineWidth(value: integer);
    function GetLineWidth:integer;
    procedure SetLineStyle(value: TPenStyle);
    function GetLineStyle: TPenStyle;
    procedure SetBracketLineWidth(value: integer);
    function GetBracketLineWidth:integer;
    procedure SetBracketLineColor(value: TColor);
    function GetBracketLineColor: TColor;
    procedure SetFillStyle(value: TBrushStyle);
    function GetFillStyle: TBrushStyle;
    function GetBoldFont: TFont;
    procedure SetNodeIndex(AValue: Integer);
    procedure SetShowBracket(AValue: boolean);
    function ShowLabel: boolean;
  public

    function DebugString: String;
    procedure DebugStrings(var aList: TStringList);
    function IsCompressed: Boolean;
    property NodeIndex: Integer read FNodeIndex write SetNodeIndex;
    property ManualCompressed: Boolean read GetManualCompressed write SetManualCompressed;
    property AutoCompressed: Boolean read FAutoCompressed write SetAutoCompressed;
    property Font: TFont read GetFont write SetFont;
    property BoldFont: TFont read GetBoldFont;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;

    property LineColor: TColor read GetLineColor write SetLineColor;
    property LineWidth: integer read GetLineWidth write SetLineWidth;
    property LineStyle: TPenStyle read GetLineStyle write SetLineStyle;
    property FillStyle: TBrushStyle read GetFillStyle write SetFillStyle;

    property BracketStyle: TBracketStyle read FBracketStyle write FBracketStyle;
    property BracketLineWidth: integer read GetBracketLineWidth write SetBracketLineWidth;
    property BracketColor: TColor read GetBracketLineColor write SetBracketLineColor;
    property BranchOption: TBranchOption read FBranchOption write FBranchOption;

    property ShowTaxonName: boolean read FShowTaxonName write FShowTaxonName;
    property ShowSpeciesName: boolean read FShowSpeciesName write FShowSpeciesName;
    property ShowTaxonMarker: boolean read FShowTaxonMarker write FShowTaxonMarker;

    property ShowCaption: boolean read FShowCaption write FShowCaption;
    property ShowBracket: boolean read FShowBracket write SetShowBracket;
    property ShowNodeMarker: boolean read FShowNodeMarker write FShowNodeMarker;
    property ShowImage: boolean read FShowImage write FShowImage;
    property ImageScaled: boolean read FImageScaled write FImageScaled;
    property Image: TBitmap read Bitmap;
    property IsTemporaryAttribute: Boolean read FIsTemporaryAttribute write SetIsTemporaryAttribute;

    procedure AssignBitmap(image: TBitmap);
    procedure AssignJPEG(image: TJPEGImage);
    procedure LoadBitmapFromFile(filename: AnsiString);
    procedure LoadJPEGFromFile(filename: AnsiString);
    procedure ClearImage;
    function IsImage: boolean;
    function IsSameFont(aFont: TFont): Boolean;
    procedure Assign(Source: TNodeAttrib);
    procedure AssignGraphAttrib(Source: TNodeAttrib);

    function ReadFromFile(var data: File; SessionFileVersion: integer; TreeSessionVersion: Integer):boolean;
    procedure WriteToFile(var data: File);

    constructor Create;
    destructor Destroy; override;
  end;

  { TNodeAttribList }

  TNodeAttribList = class(TList)
  private
    function GetItems(Index:integer): TNodeAttrib;
    procedure SetItems(Index:integer; AItem: TNodeAttrib);
  public
    function DebugStrings: TStringList;
    function GetNames: TStringList;
    procedure DeleteAll;
    function IndexOfName(aName: String): Integer;
    function IsSameFont(index: Integer; aFont: TFont): Boolean;
    function Add(Item: Pointer): Integer;
    property Items[index: integer]:TNodeAttrib read GetItems write SetItems; default;
  end;
 {$M+}
   //{$ENDIF}
  TLinearizeFunc = function(tree: TTreeData):double of object;
  //THeightSEFunc = function(NodeIndex: integer):double of object;
  THeightErrBarProc = procedure(NodeIndex: integer; var err0, err1:double) of object;
  TDivTimeFunc = function(NodeIndex: Integer): Double of object;
  TCalibrationFunc = function(const nodeIndex: Integer; var aMinTime: Double; var aMaxTime: Double; var IsSelected: Boolean): Boolean of object;

  { TTreeCustomControl }

  TTreeCustomControl = class(TEmfScrollbox)
    AttribList: TNodeAttribList;
    GroupAttrib: TNodeAttribList;
    CompressedNodeAttributes: TNodeAttribList;
  protected
    TreeList : TTreeList;
  private
    FAlignOtuNames: Boolean;
    FDataCoverageDecimals: Integer;
    FDivTimeDecimals: Integer;
    FDrawBoxForHighlightedNames: Boolean;
    FEdgePen: TPen;
    FHeightErrBarProc: THeightErrBarProc;
    FHideOverlappingDivTimes: Boolean;
    FIsDoingCalibrations: Boolean;
    FIsSamplingTimes: Boolean;
    FIsUPGMA: Boolean;
    FLatestSampleTime: Extended;
    FMakeBranchesEqualLength: Boolean;
    FMarkerPen: TPen;
    FFillBrush: TBrush;
    FMaxTimeFunc: TDivTimeFunc;
    FMinTimeFunc: TDivTimeFunc;
    FDivTimeFunc: TDivTimeFunc;
    FCalibrationFunc: TCalibrationFunc;
    FNamesAreChanged: Boolean;
    FNodeLabelFont: TFont;
    FOpenBrush: TBrush;
    FOnPaint: TNotifyEvent;
    FParentFormIsClosing: Boolean;
    FShowCalibrationTimes: Boolean;
    FShowCompressedClusterSize: Boolean;
    FShowNodeLabels: Boolean;
    FShowStatsRange: Boolean;
    FTreeBoxType: TTreeBoxType;
    FTreeDrawingTarget: TTreeDrawingTarget;


    Root : TpNode;
    Node : ^TpNodeArray;
    FSBL : double;
    Scale : double;
    {$IFDEF DARWIN}
    FSavedLines: array of array of Boolean;
    {$ENDIF}
    FAutoSize: Boolean;
    FTreeExist: boolean;
    Topoflag : boolean;
    MarkedPos : TPoint;

    MaxDepth : integer;
    MinTreeWidth: integer;
    xmax : double;
    ymax: Integer;
    xunit : double;
    yunit,gunit,xbase,ybase, MaxCharState : integer;
    Origin : TPoint;

    FNoOfOTUs : integer;
    FMaxStats : double;
    FValue : double;
    FValue2 : double;
    FStatsCutoff : integer;
    FBLenCutoff : double;
    FCondenseValue : integer;

    FDecimalDisplayMode: TFloatFormat;

    FNameEditingEnabled: Boolean;
    FBLenEditingEnabled: Boolean;
    FShowMappedTreeNames: Boolean;
	{ track the index of the current focused object and whether it is a branch, node, time, or name }
    FBranchFocused : boolean;
    FNodeFocused : boolean;
    FTimeFocused : boolean;
    FFocusedIndex : integer;
    FFocusedTimeIndex : integer;
    FFocusedNameIndex: integer;
    FCalibratedNodes: ArrayOfInteger; // for highlighting those nodes which have div time calibrations
    FBranchInfo: TBranchInfo;
    FNodeInfo: TNodeInfo;

    FOnNameClick: TNotifyEvent;

    FOnKeyDown: TKeyEvent;
    FOnChange: TNotifyEvent;
    FOnSearch: TNotifyEvent;
    FOnTreeResize: TNotifyEvent;
    FisRooted : boolean;
    FisBranchFixed : boolean;
    FisSE : boolean;
    FisStats : boolean;
    FisTimes : boolean;
    FisValue : boolean;
    FisValue2 : boolean;
    FShowStats : boolean;
    FShowDivergenceTimes: Boolean;
    FShowNodeIds: Boolean;
    FShowSelection: boolean;
    FFillSubtreeDelta : boolean;
    FStatsPosition : TBranchInfoPosition;
    FTimesPosition : TBranchInfoPosition;
    FStatsMargin : TPoint;
    FTimesMargin : TPoint;
    FShowBLen : boolean;
    FBLenPosition : TBranchInfoPosition;
    FBLenDecimals : integer;
    FValueDecimals : integer;
    FValue2Decimals : integer;
    FShowCharState : boolean;
    FShowScale : boolean;
    FShowSamplingTimeScale: Boolean;
    FShowTimeScale : boolean;
    FTimeFactor : double;
    FLatestTime : double;
    FStartAngle : integer;
    FCenterMargin : integer;
    FIsGeneDups: Boolean;
    FHasGeneDups: Boolean; { so TTreeViewForm can enable/disable actions appropriately}
    FHasSpeciations: Boolean;
    FShowGeneDupMarkers: Boolean;
    FShowSpeciationMarkers: Boolean;
    FShowRoot: boolean;
    FShowDataCoverage: Boolean;

    FShowHeightErrBar: boolean;

    FForceLinearized: boolean;
    FIsLinearized: boolean;
    FIsCondensed: boolean;
    FTopologyOnly: boolean;

    FTreeStyle : TTreeStyle;
    FBranchStyle : TBranchStyle;
    FTreeWidth : integer;
    FMinWidth, FMinHeight: integer;
    FRadius : integer;
    FPixelsPerUnit : double;

    FHorzTaxonName: boolean;
    FAlignCaption: boolean;

    FOTU_Font  : TFont;
    FCaptionFont: TFont;
    FPrivateFont: TFont;
    FBranchPen : TPen;
    FBrush: TBrush;
    FHilightColor: TColor;
    FScalePen : TPen;
    FStatsFont, FBLensFont, FTimesFont, FScaleFont, FCharStateFont : TFont;

    FScaleText : AnsiString;
    FTimeText : AnsiString;
    FScaleUnit : AnsiString;
    FTimeUnit : AnsiString;
    FScaleTick : double;
    FTimeTick : double;
    FSource : AnsiString;
    FTitle : AnsiString;
    FDescription : AnsiString;
    FStatsName : AnsiString;
    FValueName : AnsiString;
    FValue2Name : AnsiString;

    FDistance : AnsiString;
    FGap: AnsiString;
    FMethod : AnsiString;
    FTestMethod : AnsiString;

    hMetaFile : THandle;

    FDistanceMatrix : PDistanceMatrix;

    FUseSubtreeAttrib: boolean;
    FUseGroupAttrib: boolean;

    FLinearizeFunc: TLinearizeFunc;
    FPaintingArea: TRect;
    FMaxOTUPos: integer;
    FLastOtuCoords : TRect;
    FNewOtuCoords : TRect;
    FTempRect : TRect;
    FCurAttrib: TNodeAttrib;
    FMyNodeAttrib: TNodeAttrib;
    FAttribIndex: integer;
    FRevflg: boolean;
    FNodesProcessed: Int64;
    FDrawFullTree: Boolean;
    FBitmapRenderer: TBitmapTreeRenderer;
    FPdfRenderer: TPdfTreeRenderer;
    FSvgRenderer: TSvgTreeRenderer;
    FEmfRenderer: TEmfTreeRenderer;
    FRenderer: TAbstractTreeRenderer;
    FmaxTreeHeight: Integer;
    FmaxTreeWidth: Integer;
    FIsDrawing: Boolean;
    FTreeIndex: integer;
    FAutoCompressedSettings: array of array of Boolean;
    CompressedAttributesArray: array of TNodeAttribList;
    FMaxXPosition: Integer;
    EditBox: TObject; { keep this a TObject instead of TEdit otherwise streaming session properties creates duplicates}
    FOriginalEditText: AnsiString;
    FTaxaNameSearch: TTaxaNamesSearch;
    FScaleBarHeight: Integer;
    function GetHasMarkers: Boolean;
    function GetHorzFactor: Double;
    function GetHorzRatio: Double;
    function GetIsStatsStdDev: Boolean;
    function GetNumDupsBreakdown: String;
    function GetShowImages: Boolean;
    procedure InitEditBox(IsTime: Boolean = false);
    function GetCoordsLong(index: Integer): AnsiString;
    function GetCompressedClusterSize(otu: TpNode): Integer;
    function GetIsInternalOutgroup: Boolean;
    procedure InitRenderers;
    procedure FreeRenderers;
    function DumpNodeInfo: TStringList;
    function HasCalibrations: Boolean;
    function GetNumCalibrations: Integer;
    function GetLeftMargin: Integer;
    function GetTopMargin: Integer;
    function NeedsPainting(aNode: TpNode): Boolean;
    function NodeIsInViewableArea(aNodeIndex: Integer): Boolean;
    function HasBranchInClipRect(aNode: TpNode): Boolean;
    procedure SetDataCoverageDecimals(AValue: Integer);
    procedure SetDivTimeDecimals(AValue: Integer);
    procedure SetDrawBoxForHighlightedNames(AValue: Boolean);
    procedure SetHideOverlappingDivTimes(AValue: Boolean);
    procedure SetIsDoingCalibrations(AValue: Boolean);
    procedure SetIsSamplingTimes(AValue: Boolean);
    procedure SetIsUPGMA(AValue: Boolean);
    procedure SetLatestSampleTime(AValue: Extended);
    procedure SetMakeBranchesEqualLength(AValue: Boolean);
    procedure SetNodeLabelFont(AValue: TFont);
    procedure SetShowDataCoverage(AValue: Boolean);
    procedure SetShowDivergenceTimes(AValue: boolean);
    procedure SetShowImages(AValue: Boolean);
    procedure SetShowNodeIds(AValue: Boolean);
    procedure SetShowRoot(AValue: boolean);
    procedure SetShowStats(AValue: boolean);
    procedure SetShowStatsRange(AValue: Boolean);
    procedure SetTreeDrawingTarget(AValue: TTreeDrawingTarget);
    function UpdatePaintingAreaCoords: Boolean;
    function PointNearRect(aRect: TRect; aPoint: TPoint): Boolean;
    procedure DoSetDivergenceTime(value : double);
    procedure InitMem; dynamic;
    procedure ResetMem; dynamic;
    procedure InitTree; dynamic;
    procedure InitStatsMargins;
    procedure SetEdgePen(AValue: TPen);
    procedure SetFillBrush(AValue: TBrush);
    procedure SetMarkerPen(AValue: TPen);
    procedure SetOpenBrush(AValue: TBrush);
    procedure SetStats; dynamic;
    procedure SetRootAtLast;
    procedure SetPosition; virtual;
    procedure AlignCaptions;
    procedure SetCaptionY(p: TpNode);
    procedure SetCaptionX(p: TpNode);
    procedure SetYmax;
    procedure SetClusterHeight;
    function  GetDpiRatio: Double;
    function VertLineWidth(p: TpNode): integer;
    function HasOverlappingDivTime(aNode: TpNode; x: Integer; y: Integer; aText: String): Boolean;
    procedure DrawNodeIds;
    procedure DrawNodeLabels;
    procedure DrawDivergenceTimes;
    procedure DrawCalibrations;
    procedure DrawDataCoverage;
    procedure DrawStat;
    procedure DrawStatsRadiation(p: TpNode);
    procedure DrawFocus; virtual;
    procedure DrawScale; virtual;
    procedure DrawScaleForNonLinearized;
    procedure DrawScaleForLinearized;
    procedure DrawScaleCircle;
    procedure DrawCharState;
    procedure DrawOTUName(p: TpNode);
    procedure DrawOTUNameRadiation(p: TpNode);
    procedure DrawCaption(p: TpNode; a: TNodeAttrib);
    procedure DrawCompressedArea(p: TpNode; a: TNodeAttrib);
    procedure DrawCompressedAreaRadiation(p: TpNode);
    procedure DrawBitmap(p: TpNode; a: TNodeAttrib);

    procedure DrawExtendedLinesToOtuNames;
    procedure DrawBranches;
    procedure DrawBranch(p : TpNode); virtual;
    procedure DrawBranchRectangle(p: TpNode);
    procedure DrawBranchCurve(p: TpNode);
    procedure DrawBranchCircle(p: TpNode);
    procedure DrawBranchStraight(p: TpNode);
    procedure DrawBranchRadiation(p: TpNode);
    procedure DrawMarkerAngle;
    procedure DrawMarker;
    procedure DrawSquare(x, y, r : integer; angle : double; aPen: TPen; aBrush : TBrush);
    procedure DrawTriangle(x, y, r : integer; angle : double; aPen: TPen; aBrush : TBrush);
    procedure DrawCircle(x, y, r : integer; aPen: TPen; aBrush : TBrush);
    procedure DrawGeneDupMarker;  {putting this in its own procedure because otherwise everything gets messed up when groups are defined (specifically, GetNodeAttrib gets group attributes)}
    procedure ChangeAttrib(i: Integer);
    procedure SetCalibratedNodeIndices(NodeIndices: ArrayOfInteger);
    procedure DrawTree; overload;
    procedure DrawTree(aCanvas: TCanvas); overload;
    procedure GetCoordsForNodeInfo(aNode: TpNode; aText: String; var x: Integer; var y: Integer);
    procedure GetCoordsForDivTime(aNode: TpNode; aText: String; var x: Integer; var y: Integer);
    function GetBestBLenPosition(aNode: TpNode; aText: String): TPoint;
    function BLenDisplayedInsideBranch(aNode: TpNode): Boolean;
    function DistanceBetweenNodes(node1: TpNode; node2: TpNode): Double;
    procedure RotateText(R: TRect; aText: String; RotateType: TRotateType; Orientation: Integer=0);
    procedure DrawBackground;
    procedure MoveRoot(p: TpNode; midpoint: boolean); dynamic;

    procedure UpdateBrush(aBrush: TBrush; aStyle: TFPBrushStyle; aColor: TColor);
    procedure UpdatePen(aPen: TPen; PenStyle: TFpPenStyle; PenWidth: Integer; aColor: TColor; aEndCap: TPenEndCap; aJoinStyle: TPenJoinStyle);
    procedure UpdateOtuFont(a: TNodeAttrib);
    procedure UpdatePrivateFont(a: TNodeAttrib);
    procedure UpdateCaptionFont(a: TNodeAttrib);

    function GetNoOfNodes:integer;
    function SearchMidPoint:integer;
    function GetIsOutgroup:boolean;
    function GetOutgroup(index : integer):boolean;
    procedure SetOutgroup(index : integer; value : boolean);
    function GetCurrentOutgroup(index : integer):boolean;
    procedure SetDistanceMatrix(value: PDistanceMatrix);

    function GetTreeWidth:integer;
    procedure SetTreeWidth(w : integer);

    procedure SetClusterWidth(p: TpNode);
    function GetTreeHeight:integer;
    function GetCurNoOfOTUs:integer;

    function GetLongestPath:double;
    function GetMinBranchLength:double;
    function GetMaxBranchLength:double;

    procedure SetIsCondensed(value: boolean);
    procedure SetCondenseValue(value : integer);
    procedure SetOTU_Font(f : TFont);
    procedure SetStatsFont(f : TFont);
    procedure SetBLensFont(f : TFont);
    procedure SetTimesFont(f: TFont);
    procedure SetCharStateFont(f : TFont);
    procedure SetScaleFont(f : TFont);

    function GetBranchPen: TPen; virtual;
    procedure SetBranchPen(p : TPen);
    procedure SetScalePen(p : TPen);
    procedure SetTopologyOnly(value : boolean); virtual;
    function GetShowOTUName: boolean;
    procedure SetShowOTUName(b : boolean);
    function GetShowSelection: Boolean;
    procedure SetShowSelection(b : boolean);
    function GetShowOTUMarker: boolean;
    procedure SetShowOTUMarker(b : boolean); virtual;
    procedure SetShowBLen(b : boolean);
    procedure SetStatsCutoff(value : integer);
    procedure SetBLenCutoff(value : double);
    procedure InitScale; virtual;
    procedure SetScaleText(text : AnsiString);
    procedure SetTimeText(text : AnsiString);
    procedure SetTimeFactor(value : double);
    procedure SetLatestTime(value : double);
    function GetScaleDecimals: integer;
    function GetNameFocused:boolean;
    function GetOTUName(i : integer):AnsiString;
    procedure SetOTUName(index: integer; name: AnsiString); virtual;
    procedure SetBranchLength(index: integer; branchLength: Double); virtual;
    function GetAncestorName(i : integer):AnsiString;  // Used in Ancestral Sequences, supplies a name if applicable or the two decendants of the node. "(des1, des2)"
    function GetOTUOrigionalName(i: integer): AnsiString;
    function GetIndexOfOrigionalName(OrigName: AnsiString): Integer;
    function GetIndexOfName(Name: AnsiString): Integer;
    function GetClusterName(i : integer):AnsiString;
    function GetGroupName(i : integer):AnsiString;
    procedure SetClusterName(index: integer; name:AnsiString);
    procedure OnUserEditsOTUName(Sender : TObject); virtual;
    procedure EndUserEditsOTUName(Sender : TObject); virtual;
    procedure OnUserEditsTime(Sender: TObject); virtual;
    procedure EndUserEditsTime(Sender: TObject); virtual;
    procedure EditBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditBoxOnExit(Sender : TObject);
    function GetCharState(Index : integer):AnsiString;
    procedure SetCharState(Index : integer; const newstate : AnsiString);
    procedure SetClusterHeightFromDistances;
    procedure SetClusterHeightByBranchLengths;
    procedure SetClusterHeightUPGMA;
    procedure SetClusterHeightByLinearizeFunc;
    procedure SetStatsPosition(position : TBranchInfoPosition);
    procedure SetTimesPosition(Position : TBranchInfoPosition);
    procedure SetStatsMargin(margin : TPoint);
    procedure SetTimesMargin(Margin: TPoint);
    procedure SetBLenPosition(position : TBranchInfoPosition);
    procedure SetBLenDecimals(value : integer);
    procedure SetPixelsPerUnit(value : double);
    procedure SetPixelsPerOTU(value : integer);
    function GetPixelsPerOTU:integer;
    procedure SetPixelsPerGroupMember(value : integer);
    function GetPixelsPerGroupMember:integer;
    procedure SetBranchStyle(value : TBranchStyle);
    procedure SetTreeStyle(value : TTreeStyle);
    procedure SetMarker(index : integer; newstyle : TNodeMarker{TMarkerItem});
    function GetMarker(index : integer):TNodeMarker; //TMarkerItem;
    function GetTopologyOnly: boolean;
    function GetIsCondensed: boolean;

    function GetIsDistanceMatrix:boolean;
    function GetIsScale:boolean;
    function GetIsTimeScale:boolean;
    function GetIsBranchLength:boolean; virtual;
    function GetIsSE:boolean; virtual;
    function GetIsStats:boolean; virtual;
    function GetIsTimes: Boolean; virtual;
    function GetShowBLen:boolean;
    procedure SetFillSubtreeDelta(value: boolean);

    procedure Draw; virtual;

    function  FocusNode(x, y : Int64):boolean;
    function  FocusBranch(x, y : integer):boolean;
    function  FocusName(x, y : integer):boolean;
    procedure ClickName(Sender: TObject); dynamic;

    procedure ClearAttrib;

    procedure SetGroupIndex;

    procedure SetUseSubtreeAttrib(value: boolean);
    procedure SetUseGroupAttrib(value: boolean);

    procedure SetForceLinearized(value: boolean);
    function GetHilightOTUName(index: integer):boolean;
    procedure SetHilightOTUName(index: integer; value: boolean);
    function GetLineWidth(p: TpNode): integer;
    function GetNodeAttrib(p: TpNode): TNodeAttrib;
    function GetTaxonMarker(p: TpNode): TNodeMarker;
    function GetMarkerWidth(p: TpNode): Integer;
    function EquivalentMarkers(m1, m2: TNodeMarker): boolean;

    procedure SetCenterMargin(value: integer);

    procedure SetIsLinearized(value: boolean);
    function GetCoords(i: integer): AnsiString;  // Gives the "name" in a coordinate way "(des1, des2)"
    function GetAncestorNodeNumber(i: integer): integer;
    function GetShowSpeciesName: Boolean;
    procedure SetShowSpeciesName(const Value: Boolean);
    function GetNumGeneDups: Integer;
    function GetNumSpeciations: Integer;
    function GetPixelFormat(aRect: TRect): TPixelFormat;
    procedure MakeMetaFile;
    procedure SaveToEnhMetaFile(filename : String);
    procedure ComputeCaptionSize(var a: TNodeAttrib);
  protected
    FTreeDisplaySettingsCache: TTreeDisplaySettingsCache;
    FIsPainting: Boolean;
    function InCompressedClade(n: TpNode): Boolean;
    function GetCompressedAncestor(descendant: TpNode): TpNode;
    function GetShowCharState:boolean; virtual;
    procedure DrawBranchLength; virtual;
    procedure UpdateScrollPosition; override;
    procedure MPolyline(p: array of TPoint);
    procedure MTextOut(const x: Integer; const y: Integer; const str: String; const orientation: Integer = 0);
    procedure MPolygon(const p: array of TPoint);
    procedure MRectangle(const p: array of TPoint);
    procedure MRoundRectangle(aRect: TRect; const rx, ry: Integer);
    procedure MPolyBezier(const p: array of TPoint; const n: Integer; Filled: Boolean=False; Continuous: Boolean=False);
    procedure MDrawImage(const x: Integer; const y: Integer; const aBitmap: TBitmap);
    procedure MDrawArc(const aLeft: Integer; const aTop: Integer; const aRight: Integer; const aBottom: Integer; const startAngle16deg: Integer; const angleLength16deg: Integer);
    procedure MDrawCircle(const aRect: TRect);
    procedure MDrawSquare(x, y, r: Integer; a: Double);
    procedure Paint; override;

    procedure AddToSavedLines(p: array of TPoint; size: Integer);

    procedure CompressCluster(index: integer);
    procedure ExpandCluster(index: integer);
    procedure ExpandAllCluster(index: integer);

    procedure GetSessionProperties(var json: TJSONObject); virtual;
    procedure SetSessionProperties(const json: TJsonObject); virtual;
    procedure DebugToConsole;
  public
    TimeScale : double;
    IsLivingSequences: Boolean;
    Governer: Integer; { throttles progress updates up or down depending on the size of the tree - larger trees get progress updates less frequently}
    BeginRefreshCallback: TBeginRefreshCallback;
    EndRefreshCallback: TEndRefreshCallback;
    ProgressCallback: TProgressCallback;
    StatusCallback: TRebuildStatusCallback;
    IsResizing: Boolean;
    SkipDrawing: Boolean;

    SessionIsReltime: Boolean; { for backwards compatibility of session files}
    ShowCalibratedNodeMarker: Boolean;
    HideOverlappingTaxa: Boolean;
    procedure SetCaptionPosition;
    procedure GetScalingFactors(var aXUnit, aXMax: Double; var aYUnit, aYMax, aGUnit: Integer);
    function TreeBoxName: String;
    procedure BeginResize;
    procedure EndResize;
    function HasImages: Boolean;
    function PixelsPerUnitDisplayValue(var maxVal: Integer; var aCaptionStr: String): Integer;
    function PixelsPerUnitInternalValue(displayVal: Integer): Integer;
    procedure SetPixelsPerUnitFromDisplayValue(displayVal: Integer);
    function GetEditBox: TEdit;
    function GetDivTimeBoundingBox(aNode: TpNode): TRect;
    function GetNodeAtDivTimeCoords(X, Y: Integer):Integer;
    function GetBLenBoundingBox(aNode: TpNode): TRect;
    function GetBranchAtBLenCoords(X, Y: Integer):Integer;
    function FocusTime(x,y : integer):boolean;
    procedure StartUserEditingName;
    procedure StartUserEditingTime;
    procedure AutoNameInternalNodes;
    procedure ClearInternalNodeNames;
    procedure SetTreeSize;
    function SmallestPositiveBranchLength: Double;
    function IsAutoCompressedClusters: Boolean;
    function GetOtuNamesInCluster(const clusterRoot: Integer; var aList: TStringList): Integer;
    procedure GetFocusedTaxonNameAndGroup(var aTaxonName: String; var aGroupName: String);
    function NearestTipNodes(ancestralNodeIndex: Integer): TList;
    function FurthestTipNodes(ancestralNodeIndex: Integer): TList;
    function GetNodeIndexAtCoords(x,y: integer):integer;
    function GetBranchIndexAtCoords(x,y: integer):integer;
    function FocusedOnOtuNode: Boolean;
    function FocusedOnOtuBranch: Boolean;
    function FocusedOnOutgroup: Boolean;
    function DebugStringsToFile(filename: String): Boolean;
    function DebugStrings: TStringList;
    function DebugString(nodeIndex: Integer): String;
    function DebugPositionString(nodeIndex: Integer): String;
    function DebugPositionStrings: TStringList;
    function SaveToPng(Filename: String): Boolean;
    function SaveToJpeg(Filename: String): Boolean;
    function SaveToTiff(Filename: String): Boolean;
    function SaveToBitmap(Filename: String): Boolean;
    function SaveToPdf(Filename: String): Boolean;
    function SaveToSvg(Filename: String): Boolean;
    function SaveToEmf(Filename: String): Boolean;
    function TreeToJpeg: TJPEGImage;
    function ScaleBarHeight: Integer;
    procedure GetInternalOutgroupNames(var aList: TStringList);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DblClick; override;
    procedure GetSimpleNodeData(var NodeData: TSimpleTreeNodeArray);
    procedure ClearCalibratedNodeMarkers;
    procedure DrawCalibratedNodeMarkers;
    procedure ResetScrollBounds;
    function GetDrawingArea: TRect;
    function NumNodesDrawn: Integer;
    procedure DoDblClick;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;  
    procedure DoDragEnd(Target: TObject; X, Y: Integer);
    procedure DoKeyDown(var Key: Word; Shift: TShiftState);
    function OutgroupIsDes1: Boolean;
    function OutgroupIsDes2: Boolean;
    procedure FocusOnRoot;
    function IsFocusedOnRoot: Boolean;
    procedure SetIsRooted(const AValue: Boolean);
    function FindMrca(TaxonA: Integer; TaxonB: Integer): Integer; overload;
    function FindMrca(nameA: String; nameB: String): Integer; overload;
    function FindMrca(taxa: TStringList): Integer; overload;
    function FindMrca(taxaIDs: TLongintList): Integer; overload;
    function FindTwoExtantTaxa(MrcaId: Integer; var TaxonA: Integer; var TaxonB: Integer): Boolean;
    function FindTwoExtantTaxaNames(MrcaId: Integer; var TaxonA: String; var TaxonB: String): Boolean;
    function GetNodeLabelForFocusedNode(generateLabelIfNotExist: Boolean): String;
    function FindNodeId(TaxonName: String): Integer;
    function GetTaxonName(Taxon: Integer): String;
    { When the user applies a calibration, we make a copy of the TCalibrationTime
      instance for saving to a session file. SetDivergenceTime makes the copy}
    procedure SetDivergenceTime(DivergenceTime: TCalibrationTime);
    function GetDivergenceTime: double; overload;
    function GetDivergenceTime(NodeId: Integer): double;
    procedure GetEarliestAndLatestRtdtTimes(var earliestDate: Double; var latestDate: Double);
    function GetFormattedDivergenceTimeString(convertToDate: Boolean): String; overload;
    function GetFormattedDivergenceTimeString(aTime: Extended; convertToDate: Boolean): String; overload;
    function RtdtTimeToDateString(rtdtTime: Extended): String;
    function GetDaysElapsed: Int64;
    function GetMinDivergenceTime: Extended;
    function GetMaxDivergenceTime: Extended;
    procedure RemoveDivergenceTimeCalibration;
    procedure GetIntCoords(i: integer; var des1, des2: integer);
    procedure SetAttrIndex; // Moved to public since we need to call this after any font change and when simply changing font size the SetOTUFont does NOT get called, thus we must call it manually.
    function DebugFontStrings: TStringList;
    function DebugFontStr: String;
    function NodeCoords(NodeIndex: Integer): TRect;
    procedure SetShowGroupNames(aValue: Boolean);

    function SaveSessionProperties(filename: String): Boolean;
    function LoadSessionProperties(filename: String): Boolean;

    property HorzFactor: Double read GetHorzFactor;
    property HorzRatio: Double read GetHorzRatio;
    property ShowImages: Boolean read GetShowImages write SetShowImages;
    property IsDoingCalibrations: Boolean read FIsDoingCalibrations write SetIsDoingCalibrations;
    property TreeDrawingTarget: TTreeDrawingTarget read FTreeDrawingTarget write SetTreeDrawingTarget;
    property IsUPGMA: Boolean read FIsUPGMA write SetIsUPGMA;
    property CalibratedNodes: ArrayOfInteger read FCalibratedNodes write SetCalibratedNodeIndices;
    property Source : AnsiString read FSource;
    property Title : AnsiString read FTitle;
    property Description : AnsiString read FDescription;
    property Distance : AnsiString read FDistance;
    property Gap : AnsiString read FGap;
    property Method : AnsiString read FMethod;
    property TestMethod : AnsiString read FTestMethod;
    property IsLinearized : boolean read FIsLinearized write SetIsLinearized;
    property HeightErrBarProc: THeightErrBarProc read FHeightErrBarProc write FHeightErrBarProc;
    property MinTimeFunc: TDivTimeFunc read FMinTimeFunc write FMinTimeFunc;
    property MaxTimeFunc: TDivTimeFunc read FMaxTimeFunc write FMaxTimeFunc;
    property DivTimeFunc: TDivTimeFunc read FDivTimeFunc write FDivTimeFunc;
    property CalibrationFunc: TCalibrationFunc read FCalibrationFunc write FCalibrationFunc;
    property IsCondensed : boolean read GetIsCondensed write SetIsCondensed;
    property AncestorNodeNumber[index : integer] : integer read GetAncestorNodeNumber;
    property TreeExist: boolean read FTreeExist;

    property CurrentNoOfOTUs : integer read GetCurNoOfOTUs;
    property NoOfNodes: integer read GetNoOfNodes;

    property DecimalDisplayMode: TFloatFormat read FDecimalDisplayMode write FDecimalDisplayMode;
    property StatsName : AnsiString read FStatsName;
    property MaxStats : double read FMaxStats;
    property ValueName : AnsiString read FValueName;
    property Value: double read FValue write FValue;
    property ValueDecimals: integer read FValueDecimals write FValueDecimals;
    property IsValue: boolean read FisValue;
    property Value2Name: AnsiString read FValue2Name;
    property Value2: double read FValue2;
    property Value2Decimals: integer read FValue2Decimals write FValue2Decimals;
    property IsValue2: boolean read FisValue2 write FisValue2;
    property IsRooted: boolean read FisRooted write FIsRooted;
    property IsBranchFixed: boolean read FisBranchFixed;
    property IsBranchLength: boolean read GetIsBranchLength;
    property IsSE: boolean read GetIsSE;
    property IsStats: boolean read GetIsStats;
    property IsStatsStdDev: Boolean read GetIsStatsStdDev;
    property IsTimes: Boolean read GetIsTimes write FisTimes;
    property IsScale: boolean read GetIsScale;
    property IsTimeScale: boolean read GetIsTimeScale;
    property IsOutgroup: boolean read GetIsOutgroup;
    property IsInternalOutgroup: Boolean read GetIsInternalOutgroup;
    property IsDistanceMatrix : boolean read GetIsDistanceMatrix;
    property TimeFactor : double read FTimeFactor write SetTimeFactor;
    property LatestTime : double read FLatestTime write SetLatestTime;
    property Outgroup[Index: integer] : boolean read GetOutgroup write SetOutgroup;
    property CurrentOutgroup[Index: integer] : boolean read GetCurrentOutgroup;
    property LongestPath : double read GetLongestPath;
    property MinBranchLength : double read GetMinBranchLength;
    property MaxBranchLength : double read GetMaxBranchLength;
    property CondenseValue : integer read FCondenseValue write SetCondenseValue;
    property DistanceMatrix : PDistanceMatrix read FDistanceMatrix write SetDistanceMatrix;

    property OTUName[index : integer] : AnsiString read GetOTUName;
    property AncestorName[index : integer] : AnsiString read GetAncestorName;
    property CoordsName[index : integer] : AnsiString read GetCoords;
    property CoordsNameLong[index: Integer]: AnsiString read GetCoordsLong;
    property OTUOrigionalName[index : integer] : AnsiString read GetOTUOrigionalName;
    property IndexOfOrigionalName[OrigionalName: AnsiString] : integer read GetIndexOfOrigionalName;
    property IndexOfName[Name: AnsiString] : integer read GetIndexOfName;

    property GroupName[index : integer] : AnsiString read GetGroupName;
    property HilightOTUName[index : integer] : boolean read GetHilightOTUName write SetHilightOTUName;
    property HilightColor: TColor read FHilightColor write FHilightColor;
    property ClusterName[index : integer] : AnsiString read GetClusterName write SetClusterName;
    property Marker[index : integer] : TNodeMarker read GetMarker write SetMarker;
    property HasMarkers: Boolean read GetHasMarkers;
    property CharState[Index: Integer]: AnsiString read GetCharState write SetCharState;

    property PixelsPerUnit : double read FPixelsPerUnit write SetPixelsPerUnit;
    property TreeHeight : integer read GetTreeHeight;
    property MinHeight : integer read FMinHeight;
    property MinWidth : integer read FMinWidth;
    property TopMargin: Integer read GetTopMargin;
    property LeftMargin: Integer read GetLeftMargin;

    property StatsCutoff : integer read FStatsCutoff write SetStatsCutoff;
    property BLenCutoff : double read FBLenCutoff write SetBLenCutoff;

    property ScaleText : AnsiString read FScaleText write SetScaleText;
    property ScaleDecimals : integer read GetScaleDecimals;
    property ScaleTick : double read FScaleTick write FScaleTick;
    property ScaleUnit : AnsiString read FScaleUnit write FScaleUnit;
    property TimeText : AnsiString read FTimeText write SetTimeText;
    property TimeTick : double read FTimeTick write FTimeTick;
    property TimeUnit : AnsiString read FTimeUnit write FTimeUnit;

    property ParentFormIsClosing: Boolean read FParentFormIsClosing write FParentFormIsClosing;
    property NodeFocused : boolean read FNodeFocused;
    property BranchFocused : boolean read FBranchFocused;
    property TimeFocused : boolean read FTimeFocused;
    property FocusedIndex : integer read FFocusedIndex;
    property FocusedTimeIndex : integer read FFocusedTimeIndex;
    property BranchInfo: TBranchInfo read FBranchInfo;
    property NodeInfo: TNodeInfo read FNodeInfo;

    property NameFocused : boolean read GetNameFocused;
    property FocusedNameIndex : integer read FFocusedNameIndex;
    property OnNameClick: TNotifyEvent read FOnNameClick;

    property UseSubtreeAttrib: boolean read FUseSubtreeAttrib write SetUseSubtreeAttrib;
    property UseGroupAttrib: boolean read FUseGroupAttrib write SetUseGroupAttrib;
    property IsSamplingTimes: Boolean read FIsSamplingTimes write SetIsSamplingTimes;
    property LatestSampleTime: Extended read FLatestSampleTime write SetLatestSampleTime;
    property HideOverlappingDivTimes: Boolean read FHideOverlappingDivTimes write SetHideOverlappingDivTimes;
    property LinearizeFunc: TLinearizeFunc read FLinearizeFunc write FLinearizeFunc;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure MakeBranchFromRoot; dynamic;
    procedure MakeRootOnBranch; dynamic;
    procedure MakeRootOnMidPoint; dynamic;
    procedure MakeRootByOutgroup(OutgroupOnBottom: Boolean=False); dynamic;
    procedure MakeRootByInternalOutgroup(outgroupNodes: TStringList; OutgroupOnBottom: Boolean = True); dynamic;
    procedure PutOutgroupOnBottom;
    procedure FocusOnOutgroup;
    function GetOutgroupAncestorIndex: Integer;
    procedure MarkOutgroupNodes; { sets outgroup := true for all nodes in the outgroup cluster. This is done so we can avoid drawing divergence times for these nodes}
    function MarkInternalOutgroupNodes(outgroupNodes: TStringList): TpNode;
    function GetFirstOutgroupMemberIndex: Integer;
    procedure AddOutgroup(index : integer);
    procedure RemoveOutgroup(index : integer);
    procedure SortClusterInOrder;
    procedure SortTreeByYear;
    procedure SortTreeByContinent;
    procedure SortTreeByGroup;
    procedure SortClusterForShape;
    procedure FlipCluster; dynamic;
    procedure FlipAllCluster; dynamic;
    procedure BuildTree;
    procedure BuildNoDraw;
    procedure Refresh; virtual;
    function FindEarliestAndLatestDates(var earliestDate: String; var latestDate: String): Boolean; overload;
    function FindEarliestAndLatestDates(var earliestDate: Double; var latestDate: Double): Boolean; overload;
    function FocusedNodeHeight: Extended;
    function NodeHeight(index: Integer): Extended;
    procedure FocusOnNode(index: integer);
    procedure FocusOnBranch(nodeindex: integer);
    procedure FocusOnTime(nodeindex: integer);
    procedure FocusOnName(index: integer);
    function FocusOnPoint(Point: TPoint): Boolean;
    procedure ClearFocus;
    procedure Print;
    procedure PrintSmall;
    procedure CopyImageToClipBoard;
    function WriteTreeInTableFormat: TStringList;
    procedure AssignTreeAttrib(Source: TTreeCustomControl); dynamic;
    procedure Clear;
    procedure SetCustomHilightColorOTUName(index: integer; value: boolean; Color: TColor = clWhite);
    procedure GetTreeData(tree: TTreeData; UseTimeFactor: Boolean=False); virtual;
    procedure GetTreeDataNoOutgroup(var tree: TTreeData; var namesList: TStringList; UseTimeFactor: Boolean=False);
    procedure GetDescName(nodeindex: integer; names: TStringList);
    procedure BringOtuNameIntoView(aOtuName: String);
    function GetMetafileHandle: THandle;
    function StrLength(TheStr: AnsiString; TheFont: TFont): Integer;
    function StrHeight(TheStr: AnsiString; TheFont: TFont): Integer;
    function GetTaxaDisplayInfoList(aNode: TpNode): TNodeDisplayInfoList;
    function GetInternalNodeDisplayInfoList(aNode: TpNode): TNodeDisplayInfoList;
    function DoSearch(taxaInfo: TNodeDisplayInfoList; const Query: AnsiString; const Contains: Boolean; const HighlightAll: Boolean; const QueryChanged: Boolean; var focusedResultIndex: Integer): Integer;
    function SearchTipNames(const Query: AnsiString; const Contains: Boolean; const HighlightAll: Boolean; const QueryChanged: Boolean; var focusedResultIndex: Integer): Integer;
    function SearchLabelledNodes(Query: String; Contains: Boolean; HighlightAll: Boolean; QueryChanged: Boolean; var focusedResultIndex: Integer): Integer;
    function SearchGeneDup(var Index: Integer; var NodeId: Integer; const SearchDir: TSearchDirection; const IsDup: Boolean = True): Boolean;
    function GeneDupFocusedIndex(IsDup: Boolean =  True): Integer;
    function FocusedOnGeneDup: Boolean;
    function FocusedOnSpeciation: Boolean;
    function AvgEvolutionaryRate: Double;
    constructor Create(AOwner: TComponent; aType: TTreeBoxType); overload;
    destructor Destroy; override;

    property AlignOtuNames: Boolean read FAlignOtuNames write FAlignOtuNames;
    property TreeBoxType: TTreeBoxType read FTreeBoxType;
    property EditNameEnabled: Boolean read FNameEditingEnabled write FNameEditingEnabled;
    property EditTimesEnabled: Boolean read FBLenEditingEnabled write FBLenEditingEnabled;
    property StatsMargin : TPoint read FStatsMargin write SetStatsMargin;
    property TimesMargin : TPoint read FTimesMargin write SetTimesMargin;
    property ForceLinearized : boolean read FForceLinearized write SetForceLinearized; // For consistency of UPGMA tree.
    property OTU_Font : TFont read FOTU_Font write SetOTU_Font;
    property NodeLabelFont: TFont read FNodeLabelFont write SetNodeLabelFont;
    property StatsFont : TFont read FStatsFont write SetStatsFont;
    property BLensFont : TFont read FBLensFont write SetBLensFont;
    property TimesFont : TFont read FTimesFont write SetTimesFont;
    property ScaleFont : TFont read FScaleFont write SetScaleFont;
    property CharStateFont : TFont read FCharStateFont write SetCharStateFont;
    property CaptionFont: TFont read FCaptionFont write FCaptionFont;
    property PrivateFont: TFont read FPrivateFont write FPrivateFont;
    property HasGeneDups: Boolean read FHasGeneDups write FHasGeneDups;
    property ShowNodeLabels: Boolean read FShowNodeLabels write FShowNodeLabels;
    property DrawBoxForHighlightedNames: Boolean read FDrawBoxForHighlightedNames write SetDrawBoxForHighlightedNames;
    property MakeBranchesEqualLength: Boolean read FMakeBranchesEqualLength write SetMakeBranchesEqualLength;
    property ShowSelection: boolean read GetShowSelection write SetShowSelection;
  published

    property NoOfOTUs : integer read FNoOfOTUs;
    property DivTimeDecimals: Integer read FDivTimeDecimals write SetDivTimeDecimals;
    property DataCoverageDecimals: Integer read FDataCoverageDecimals write SetDataCoverageDecimals;
    property NamesAreChanged: Boolean read FNamesAreChanged;
    property NumCalibrations: Integer read GetNumCalibrations;
    property NumGeneDups: Integer read GetNumGeneDups;
    property NumSpeciations: Integer read GetNumSpeciations;
    property IsGeneDups: Boolean read FIsGeneDups write FIsGeneDups;
    property HasSpeciations: Boolean read FHasSpeciations write FHasSpeciations;
    property ShowGeneDupMarkers: Boolean read FShowGeneDupMarkers write FShowGeneDupMarkers;
    property ShowSpeciationMarkers: Boolean read FShowSpeciationMarkers write FShowSpeciationMarkers;
    property NumDupsBreakdown: String read GetNumDupsBreakdown;
    property BranchPen : TPen read FBranchPen write SetBranchPen;
    property EdgePen: TPen read FEdgePen write SetEdgePen;

    property ScalePen : TPen read FScalePen write SetScalePen;
    property MarkerPen: TPen read FMarkerPen write SetMarkerPen;
    property FillBrush: TBrush read FFillBrush write SetFillBrush;
    property OpenBrush: TBrush read FOpenBrush write SetOpenBrush;

    property Radius : integer read FRadius write FRadius;
    property StartAngle : integer read FStartAngle write FStartAngle;
    property CenterMargin : integer read FCenterMargin write SetCenterMargin;

    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property TreeWidth : integer read GetTreeWidth write SetTreeWidth;
    property PixelsPerGroupMember : integer read GetPixelsPerGroupMember write SetPixelsPerGroupMember;
    property PixelsPerOTU : integer read GetPixelsPerOTU write SetPixelsPerOTU;

    property TreeStyle : TTreeStyle read FTreeStyle write SetTreeStyle;
    property BranchStyle : TBranchStyle read FBranchStyle write SetBranchStyle;
    property FillSubtreeDelta : boolean read FFillSubtreeDelta write SetFillSubtreeDelta;
    property StatsPosition : TBranchInfoPosition read FStatsPosition write SetStatsPosition;
    property TimesPosition : TBranchInfoPosition read FTimesPosition write SetTimesPosition;

    property BLenPosition : TBranchInfoPosition read FBLenPosition write SetBLenPosition;
    property BLenDecimals : integer read FBLenDecimals write SetBLenDecimals; // @SK mega2b4

    property ShowOTUName : boolean read GetShowOTUName write SetShowOTUName;
    property ShowSpeciesName: Boolean read GetShowSpeciesName write SetShowSpeciesName;
    property ShowOTUMarker : boolean read GetShowOTUMarker write SetShowOTUMarker;
    property ShowStats : boolean read FShowStats write SetShowStats;
    property ShowStatsRange: Boolean read FShowStatsRange write SetShowStatsRange;
    property ShowDataCoverage: Boolean read FShowDataCoverage write SetShowDataCoverage;
    property ShowDivergenceTimes : boolean read FShowDivergenceTimes write SetShowDivergenceTimes;
    property ShowCalibrationTimes: Boolean read FShowCalibrationTimes write FShowCalibrationTimes;
    property ShowNodeIds: Boolean read FShowNodeIds write SetShowNodeIds;
    property ShowBLen : boolean read GetShowBLen write SetShowBLen;
    property ShowScale : boolean read FShowScale write FShowScale;
    property ShowTimeScale : boolean read FShowTimeScale write FShowTimeScale;
    property ShowSamplingTimeScale: Boolean read FShowSamplingTimeScale write FShowSamplingTimeScale;
    property ShowCharState : boolean read GetShowCharState write FShowCharState;
    property ShowCompressedClusterSize: Boolean read FShowCompressedClusterSize write FShowCompressedClusterSize;
    property ShowHeightErrBar: boolean read FShowHeightErrBar write FShowHeightErrBar;

    property ShowRoot : boolean read FShowRoot write SetShowRoot;

    property ShowTopologyOnly : boolean read GetTopologyOnly write SetTopologyOnly;
    property HorzTaxonName: boolean read FHorzTaxonName write FHorzTaxonName;
    property AlignCaption: boolean read FAlignCaption write FAlignCaption;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ShowMappedTreeNames: Boolean read FShowMappedTreeNames write FShowMappedTreeNames;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    //property OnDragOver;
    property OnEndDock;
    //property OnEndDrag;
    //property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    //property OnStartDrag;
    property OnSearch: TNotifyEvent read FOnSearch write FOnSearch;
    property OnTreeResize: TNotifyEvent read FOnTreeResize write FOnTreeResize;
  end;


{ TTreeEditOperation }

TTreeEditOperation = class(TObject)
public
  TreeBox: TTreeCustomControl;
  OperationType: UndoAction;
  UndoTreeData: TTreeData;
  UndoNameList: TStringList;
  UndoPrivateNameList: TStringList;
  UndoIndex: Integer;
  TreeDataChanged: Boolean;
  NameListChanged: Boolean;
  PrivateNameListChanged: Boolean;

  constructor CreateInitialTreeUndoOp(aTreeBox: TTreeCustomControl; aTree: TTreeData; names: TStringList; privateNames: TStringList);
  constructor CopyLastUndoState(var EditOp: TTreeEditOperation; opType: UndoAction);
  constructor CreateOpTopologyChanged(EditOp: TTreeEditOperation; opType: UndoAction);
  constructor CreateOpAddOTU(EditOp: TTreeEditOperation; node: integer; name: AnsiString);
  constructor CreateOpRemoveOTU(EditOp: TTreeEditOperation; index: integer);
  constructor CreateOpSetOTUName(EditOp: TTreeEditOperation; index: integer; name: AnsiString);
  constructor CreateOpSetOTUPrivateName(EditOp: TTreeEditOperation; index: integer; name: AnsiString);
  destructor Destroy; override;
  function DebugString: String;
  procedure UpdateActiveNamesMap(var activeNames: TStringList);
  procedure MarkUndefinedPrivateNames;
end;

TTreeEditOperationArray = array of TTreeEditOperation;

{ TTreeEditBox}
TTreeEditBox = class(TTreeCustomControl)
private
  FNodeMarkerPoints: TPointArray;
  FLinePoints: TPointArray;
  FPolyLinePoints: TPointArray;
  FEditEnabled : boolean;
  Dragging: boolean;
  TargetBranch: integer;
  FFocusedBranchPen: TPen;
  FDrawingFocusedNode: Boolean;

  FDragStartPoint: TPoint;
  FDragLastPoint : TPoint;
  FStartPixelsPerOTU: Integer;
  FStartTreeWidth: Integer;
  FStartTreeHeight: Integer;
  FStartTreeRadius: Integer;

  procedure DrawFocus; override;
  function GetBranchPen: TPen; override;
  procedure DrawBranch(p: TpNode); override;
  procedure ResetMem; override;
  function ResetTree(tree: TTreeData; name: TStringList; privateName: TStringList):boolean;
  function GetNewickTree: AnsiString;
  function GetNewickPrivateTree: AnsiString;
  procedure ClearUndoStack;
public
  UndoStack: TStack;
  DropLocation: TPoint;
  {$IFDEF DEBUG} function UndoStackDebugStrings: TStringList; {$ENDIF}
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  function BranchImageCursor(SelectedNode: TpNode; FoundTarget: Boolean = False): TCursorImage;
  function GetLastTreeState: TTreeEditOperation;
  function MoveNode(p, target: TpNode):boolean; overload;
  function MoveNode(Source, Target: Integer): Boolean; overload;
  property NewickTree: AnsiString read GetNewickTree;
  property NewickPrivateTree: AnsiString read GetNewickPrivateTree;

  procedure MakeRootOnBranch; override;
  procedure MakeRootOnMidPoint; override;
  procedure MakeRootByOutgroup(OutgroupOnBottom: Boolean=False); override;
  procedure FlipCluster; override;
  procedure FlipAllCluster; override;
  procedure Undo;
  function  CanUndo: Boolean;
  function FocusOnClosest(cursorPos: TPoint; searchRadius: Integer): Boolean;
  procedure SetOTUName(index: integer; name: AnsiString); override;
  procedure SetBranchLength(index: integer; branchLength: Double); override;
  procedure SetOTUPrivateName(index: integer; name: AnsiString; Speedy: Boolean = False); // Speedy was added for when we are setting private names one after another for a speedup, since we dont' need to create and undo and change the width.
  function  LargestWidthOfOTUNames: Integer;
  function  MaxWidthOTUNames: Integer;
  function GetOTUPrivateName(index: integer): AnsiString;
  property OTUPrivateName[index : integer] : AnsiString read GetOTUPrivateName;
  function HeightAsDivergenceTime(NodeId: Integer): double;
  function SetTreeData(isEditTopologyMode: Boolean; tree: TTreeData; name: TStringList; privatename: TStringList; IsRooted: Boolean = False):boolean;
  function GetIsBranchLength: Boolean; override;
  procedure RemoveOTU(index: integer);
  procedure InsertOTU(nodeindex: integer; name: AnsiString);

  function GetIndexOfPrivateName(Name: AnsiString): Integer;
  procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
  constructor Create(AOwner: TComponent; aType: TTreeBoxType);
  destructor Destroy; override;

  property IndexOfPrivateName[Name: AnsiString] : integer read GetIndexOfPrivateName;
  property IsTimetree : Boolean read FIsRooted; // IsTimetree should be a separate property, for now use IsRooted
published
  property EditEnabled : boolean read FEditEnabled write FEditEnabled;

end;

  TBranchInfoFunc = function(tree: TTreeData):double of object;


  TArrayOfNodeAttribList = array[0..(MaxInt div SizeOf(Pointer))-1] of TNodeAttribList;
  PArrayOfNodeAttribList = ^TArrayOfNodeAttribList;
  PString = ^AnsiString;
  TArrayOfString = array[0.. (MaxInt div SizeOf(Pointer)) - 1] of PString; //array[word] of PString;
  PArrayOfString = ^TArrayOfString;
  TAncStateProc = procedure(AncState: PArrayOfString; SiteIndex : integer; tree: TTreeData) of object;
  TAncProbFunc = function(NodeIndex, SiteIndex: integer; tree: TTreeData; Probs: TStringList):double of object;

 {$M+}

 { TTreeBox}
  TTreeBox = class(TTreeCustomControl)
  private
    FFileVersion,MyCurrentVersion: integer;
    FisTimes: boolean;
    FNoOfTrees: integer;
    FSiteIndex: integer;
    FMaxSiteIndex: integer;
    FConsensusValue: integer;
    FFreqName: AnsiString;
    FFreqDecimals: integer;
    ConsTreeIsValue: boolean;
    ConsTreeIsValue2: boolean;
    ConsCondenseValue: integer;

    ConsTree : TTreeData;
    ConsCondensed, ConsTopoOnly, ConsRooted: boolean;
    FPartitionList: TPartitionList;

    Initialized: PArrayOfBool;
    FDataInitialized: PArrayOfBool;

    FBlenFunc: TBranchInfoFunc;
    FSEFunc: TBranchInfoFunc;
    FStatsFunc: TBranchInfoFunc;
    FValueFunc: TBranchInfoFunc;
    FValue2Func: TBranchInfoFunc;

    FAncStateProc: TAncStateProc;
    FAncProbFunc: TAncProbFunc;
    AncState: PArrayOfString;

    AttribListArray: PArrayOfNodeAttribList;
    function GetClustersAreAutoCompressed: Boolean;
    function GetGroupsAreAutoCompressed: Boolean;
    function GetHasInternalNodeLabels: Boolean;
    function GetHasNonZeroBranchLength: Boolean;
    function GetSimilarSeqsAreAutoCompressed: Boolean;
    procedure InitMem; override;
    procedure ResetMem; override;
    procedure InitTree; override;
    procedure SetStats; override;
    procedure EndUserEditsOTUName(Sender : TObject); override;
    procedure SetCondenseValue(value : integer);
    function GetCondenseValue: integer;
    procedure SetTreeIndex(index : integer);
    procedure SetOutgroups(outgroup : boolean);
    procedure LoadTreeData(index : integer);
    procedure InitTreeData(index : integer);
    procedure SaveTreeData(index : integer);
    procedure MakeConsensusTree;
    procedure SetConsensusValue(value : integer);
    function GetValue(index:integer):double;
    function GetValue2(index:integer):double;
    function GetFrequency(index:integer):double;
    function GetSBL(index:integer):double;
    function GetTotalFrequency:double;
    function GetStatsName:AnsiString;
    function GetIsConsensus:boolean;
    procedure SetAncStateProc(proc: TAncStateProc);
    procedure SetSiteIndex(value: integer);
    procedure SetMaxSiteIndex(value: integer);
    function GetIsValue:boolean;
    function GetIsValue2:boolean;
    function GetIsFreq:boolean;
    procedure SetValueFunc(f: TBranchInfoFunc);
    procedure SetValue2Func(f: TBranchInfoFunc);
    function GetIsBranchLength:boolean; override;
    function GetIsSE:boolean; override;
    function GetIsStats:boolean; override;
    function GetIsTimes: Boolean; override;
    function GetMaxStats:double;
    function GetIsAncState:boolean;
    function GetDataInitialized(index: integer):boolean;
    // For Caption Expert
    function GetLabelledSites : AnsiString;
    function GetCodonPositions : AnsiString;
    function GetTreeCI : AnsiString;
    function GetTreeRI : AnsiString;
    function GetTreeRCI : AnsiString;
    function GetTreeCI_infosites : AnsiString;
    function GetTreeRI_infosites : AnsiString;
    function GetTreeRCI_infosites : AnsiString;
    function GetSiteIndexForCaption : AnsiString;

    function GetInformation: TStringList;
    procedure ExportTree(var f: TextFile; treeindex: integer);
    procedure SetTopologyOnly(value : boolean); override;

    procedure MoveRoot(p: TpNode; midpoint: boolean); override;
    procedure SetAttribListToAttributesForActiveTree;
    procedure SetCompressedAttribList;
    procedure DeleteAttrib(index: integer);
    procedure DeleteCompressedAttrib(index: Integer);

    procedure SetShowOTUMarker(b : boolean); override;

    procedure SetValue(index: integer; value: double);
    procedure SetValue2(index: integer; value: double);
    function GetFirstRepresentativeInCluster(p: TpNode): String;
  protected
    function GetShowCharState: boolean; override;
    procedure DrawBranchLength; override;
    procedure GetSessionProperties(var json: TJsonObject); override;
    procedure SetSessionProperties(const json: TJsonObject); override;
  published
    property FreqDecimals: integer read FFreqDecimals write FFreqDecimals;

  public
    procedure SetAncState;
    procedure SetTaxonGroupName(aTaxon: String; aGroupName: String);
    procedure SetTaxaGroupName(aGroupName: String; aNodeIndex: Integer);
    procedure ExpandAutoCompressedClusters(DoRefresh: Boolean);
    procedure AutoCompressCluster(p: TpNode);
    procedure CompressClustersBySize(clusterSize: Int64);
    procedure CompressGroups;
    procedure CompressSimilarSequences(maxDiff: Extended);
    procedure UpdateCalibratedNodes(aCalibrations: TCalibrations; showUnselected: Boolean = False);
    function GetAncStatesExportHeader: TAncStatesExportHeader;
    procedure GetInternalNodeLabels(var NodeLabels: TStringList);
    procedure GetTreeData(tree: TTreeData; UseTimeFactor: Boolean=False); override;
    procedure UpdateDataCoverage(AData: TTreeData);
    procedure AssignTreeList(ATreeList: TTreeList);
    procedure AssignTreeListInternalNodeLabelFunc(var AFunc: TGetInternalNodeLabelFunc);
    function GetOTUNamesList: TStringlist;
    procedure GetOTUNames(var aList: TStringList);
    function GetSpeciesNamesList: TStringList;
    function SetSpeciesNames(AList: TStringList): Boolean;
    procedure GetOutgroupTaxa(var AList: TStringList);
    function GetOutgroupInfo: TStringList;
    procedure SetOutgroupCluster(AColor: TColor; AIndex: Integer);
    procedure HighlightOutgroupCluster(ANode: TpNode; IsDes1: Boolean; AColor: TColor);
    procedure ClearOutgroups;
    procedure ClearTemporaryAttributes;
    procedure HighlightIngroupRootBranch(aColor: TColor);
    procedure SetGeneDuplications(AGeneTree: TSimpleTreeNodeArray); overload;
    function LargestWidthOfOTUNames: Integer;
    property Value[Index:integer] : double read GetValue write SetValue;
    property Value2[Index:integer] : double read GetValue2 write SetValue2;
    property SBL[Index:integer] : double read GetSBL;
    property Frequency[Index:integer] : double read GetFrequency;
    property DataInitialized[index: integer]: boolean read GetDataInitialized;
    procedure AssignTreeAttrib(Source: TTreeCustomControl); override;
    property Information: TStringList read GetInformation;
    property FileVersion: integer read FFileVersion;
    property HasInternalNodeLabels: Boolean read GetHasInternalNodeLabels;
    procedure SetOtuInfo(otuInfo: TList);
    function ReplaceTaxaNames(namesList: TStringList): Integer;
    function NumGroups: Integer;
    function ReadFromFile(var data: File; SessionFileVersion: integer):boolean; virtual;
    procedure WriteToFile(var data: File); virtual;
    destructor Destroy; override;
  published
    property GroupsAreAutoCompressed: Boolean read GetGroupsAreAutoCompressed;
    property ClustersAreAutoCompressed: Boolean read GetClustersAreAutoCompressed;
    property SimilarSeqsAreAutoCompressed: Boolean read GetSimilarSeqsAreAutoCompressed;
    property HasNonZeroBranchLength: Boolean read GetHasNonZeroBranchLength;
    property FreqName: AnsiString read FFreqName write FFreqName;
    property NoOfTrees: integer read FNoOfTrees;
    property TreeIndex: integer read FTreeIndex write SetTreeIndex;
    property isValue : boolean read GetIsValue;
    property isValue2 : boolean read GetIsValue2;
    property isFreq: boolean read GetIsFreq;
    property isBranchLength : boolean read GetIsBranchLength;
    property isSE : boolean read GetIsSE;
    property isStats : boolean read GetIsStats;
    property isTimes : boolean read GetIsTimes write FisTimes;

    property TotalFrequency: double read GetTotalFrequency;

    property StatsName : AnsiString read GetStatsName;
    property MaxStats: double read GetMaxStats;
    property isConsensus : boolean read GetIsConsensus;
    property ConsensusValue: integer read FConsensusValue write SetConsensusValue;
    property CondenseValue: integer read GetCondenseValue write SetCondenseValue;

    property BLenFunc: TBranchInfoFunc read FBLenFunc write FBLenFunc;
    property SEFunc: TBranchInfoFunc read FSEFunc write FSEFunc;
    property StatsFunc: TBranchInfoFunc read FStatsFunc write FStatsFunc;

    property ValueFunc: TBranchInfoFunc read FValueFunc write SetValueFunc;
    property Value2Func: TBranchInfoFunc read FValue2Func write SetValue2Func;

    property isAncState: boolean read GetIsAncState;
    property SiteIndex: integer read FSiteIndex write SetSiteIndex;
    property SiteIndexForCaption: AnsiString read GetSiteIndexForCaption;
    property MaxSiteIndex: integer read FMaxSiteIndex write SetMaxSiteIndex;
    property AncStateProc: TAncStateProc read FAncStateProc write SetAncStateProc;
    property AncProbFunc: TAncProbFunc read FAncProbFunc write FAncProbFunc;

    // for CaptionExpert
    property CodonPositionsInfo : AnsiString read GetCodonPositions;
    property LabelledSitesInfo : AnsiString read GetLabelledSites;
    property TreeCI : AnsiString read GetTreeCI;
    property TreeRI : AnsiString read GetTreeRI;
    property TreeRCI : AnsiString read GetTreeRCI;
    property TreeCI_infosites : AnsiString read GetTreeCI_infosites;
    property TreeRI_infosites : AnsiString read GetTreeRI_infosites;
    property TreeRCI_infosites : AnsiString read GetTreeRCI_infosites;
    function GetSubtreeAttribIndex(const nodeIndex: Integer; var IsGroupAttribute: Boolean): Integer;
    function GetSubtreeAttrib(var NodeAttrib: TNodeAttrib; const nodeindex: integer): boolean;
      // return if Node[nodeindex] is the attribute holder
    function NodeIsAttributeHolder(nodeIndex: Integer): Boolean;
    procedure SetSubtreeAttrib(NodeAttrib: TNodeAttrib; nodeindex: integer);
    procedure ClearSubtreeAttrib(nodeindex: integer; recursive: boolean);
    procedure ClearAllSubtreeAttrib;
    procedure OverwriteAttribDownstream(nodeindex: integer);
    procedure SetTaxonMarkerOfSubtree(nodeindex: integer; marker: TNodeMarker);

    procedure SetGroupInfo(groupinfo: TStringList);
    function GetGroupInfo: TStringList;
    function GetGroupNames: TStringList;
    procedure GetGroupAttrib(Attrib: TNodeAttrib; GroupName: AnsiString);
    procedure SetGroupAttrib(Attrib: TNodeAttrib; GroupName: AnsiString);
    procedure SetTaxonMarkerOfGroup(GroupName: AnsiString; marker: TNodeMarker);

    function AncStateProb(AncState: AnsiChar; NodeIndex: integer):double;
    function GetAncState(proc: TAncStateProc; NodeIndex: integer; Site: Integer=-1):AnsiString;

    function  ImportFromNewickStandard(filename : AnsiString; Unroot: Boolean=False):boolean;
    function  ImportFromNewickString(NewickTree : AnsiString):boolean; overload;
    function  ImportFromNewickString(NewickTree : AnsiString; NamesList: TStringList):boolean; overload;

    function  ImportFromMEGA:boolean;

    procedure GetSubtree(subtree: TTreeBox);
    function GetSubtreeTreeList: TTreeList;

    function SetTreeList(aTreeList: TTreeList; optimize: boolean):boolean;
    procedure SetTreeListRooted(const AValue: Boolean);
    procedure SetPartitionList(Partitions: TPartitionList);
    function GetPartitionList: TPartitionList;
    procedure AssignPartitionList(AList: TPartitionList);
    function HasPartitions: Boolean;
    function HasSpeciesNames: Boolean;
    procedure ExportAllTreesToFile(filename : AnsiString);
    procedure ExportCurrentTreeToFile(filename : AnsiString);

    procedure ExportAllTreesToNewickFile(filename : AnsiString);
    procedure ExportCurrentTreeToNewickFile(filename : AnsiString);
    function GetCurrentTree: TTreeData;
    function GetRootedNewickTreeNoBCL: String;
    function GetNewickTree(useQuotesForLabels: Boolean): String; virtual; overload;
    function GetNewickTree(Options: TNewickExportOptions): String; virtual; overload;
    function GetSubtreeNewick(useQuotesForLabels: Boolean): String;
    function GetBootstrapNewickString(Options: TNewickExportOptions): String;
    function GetAllNewickTrees(Options: TNewickExportOptions): TStringList;
    procedure SetOTUName(index: integer; name: AnsiString);
    procedure ResetBaseNodeAttribFonts;
    function FinalizeCalibration(var ACalibration: TCalibrationTime): Boolean;
    constructor Create(AOwner: TComponent; aType: TTreeBoxType); overload;

    procedure InitDefaultValues;
    property OnNameClick;
  end;

  { TCustomTimetreeBox }

  TCustomTimetreeBox = class(TTreeBox)
    private
      FOutgroup: TpNode;
      procedure DrawScale; override;
      procedure InitScale; override;
      procedure SetPosition; override;
      function GetBestTimeScale: Double;
    protected
      FTimetree: TObject;
    public
      constructor Create(aTimetree: TObject; aOwner: TComponent; aType: TTreeboxType);
      destructor Destroy; override;
      function ReadFromFile(var data: File; sessionFileVersion: integer):boolean; override;
      procedure WriteToFile(var data: File); override;
      procedure FindOutgroup;
      procedure SetCalibrations(aCalibrations: TCalibrations);
      function MinTime(NodeIndex: Integer): Double;
      function MaxTime(NodeIndex: Integer): Double;
      function DivTime(NodeIndex: Integer): Double;
      function GetNewickNoOutgroup: String;
      procedure ErrBarBounds(NodeIndex: Integer; var errHigh: Double; var errLow: Double);
  end;

  function TreeStyleString(aStyle: TTreeStyle): String;
  function StringToTreeStyle(str: String): TTreeStyle;
  function BranchStyleString(aStyle: TBranchStyle): String;
  function StringToBranchStyle(str: String): TBranchStyle;
  function BranchInfoPositionString(aInfo: TBranchInfoPosition): String;
  function StringToBranchInfoPosition(str: String): TBranchInfoPosition;

{$ENDIF}

implementation
{$IFDEF VISUAL_BUILD}
uses
  {$IFDEF DEBUG}
  mdeveloper_console, MEditorForm,
  {$ENDIF}
  mlittle_bootstrap_tree,
  MTreeProc, StrUtils, MGlobalSettings, types, MegaUtils, dateutils, IntfGraphics,
  fpimage, fppdf, stringutils, registry;

function TreeStyleString(aStyle: TTreeStyle): String;
begin
  Result := EmptyStr;
  case aStyle of
    tsTraditional: Result := 'traditional';
    tsRadiation: Result := 'radiation';
    tsCircle: Result := 'circle';
  end;
end;

function StringToTreeStyle(str: String): TTreeStyle;
begin
  if str = 'traditional' then
    Result := tsTraditional
  else if str = 'radiation' then
    Result := tsRadiation
  else if str = 'circle' then
    Result := tsCircle
  else
    raise Exception.Create('invalid tree style: ' + str);
end;

function BranchStyleString(aStyle: TBranchStyle): String;
begin
  Result := EmptyStr;
  case aStyle of
    bsRectangular: Result := 'rectangular';
    bsCurved: Result := 'curved';
    bsStraight: Result := 'straight';
  end;
end;

function StringToBranchStyle(str: String): TBranchStyle;
begin
  if str = 'rectangular' then
    Result := bsRectangular
  else if str = 'curved' then
    Result := bsCurved
  else if str = 'straight' then
    Result := bsStraight
  else
    raise Exception.Create('invalid branch style: ' + str);
end;

function BranchInfoPositionString(aInfo: TBranchInfoPosition): String;
begin
  Result := EmptyStr;
  case aInfo of
    bipAutomatic: Result := 'automatic';
    bipAboveBranch: Result := 'aboveBranch';
    bipBelowBranch: Result := 'belowBranch';
  end;
end;

function StringToBranchInfoPosition(str: String): TBranchInfoPosition;
begin
  if str = 'automatic' then
    Result := bipAutomatic
  else if str = 'aboveBranch' then
    Result := bipAboveBranch
  else if str = 'belowBranch' then
    Result := bipBelowBranch
  else
    raise Exception.Create('invalid branch info position: ' + str);
end;

{ TCustomTimetreeBox }

procedure TCustomTimetreeBox.DrawScale;
begin
  if Topoflag then Exit;
  if (not FShowTimeScale) and (not FShowScale) then Exit;
  UpdatePen(FScalePen, psSolid, FScalePen.Width, ScalePen.Color, pecSquare, pjsMiter);
  Canvas.Pen := FScalePen;
  Canvas.Brush.Style := bsClear;
  DrawScaleForLinearized
end;

procedure TCustomTimetreeBox.InitScale;
var
  aScale: Double = 0;
begin
  aScale := TimeScale;
  inherited InitScale;
  FTimeFactor := 1;
  TimeScale := aScale;
  ScaleUnit := Format('%0.2f', [TimeScale]);
  if order(abs(TimeScale)) >= 1 then
    FTimeText := FloatToStrF(TimeScale + LatestTime, DecimalDisplayMode, 15, 0)
  else
    FTimeText := FloatToStrF(TimeScale + LatestTime, DecimalDisplayMode, 15, Abs(order(TimeScale)));
end;

procedure TCustomTimetreeBox.SetPosition;
begin
  if not Assigned(FOutgroup) then
    FindOutgroup;
  inherited SetPosition;
  if (not FTopologyOnly) and (TreeStyle = tsTraditional) then
    FOutgroup.position.X := Round(xmax*xunit);
end;

function TCustomTimetreeBox.GetBestTimeScale: Double;
var
  aTimetree: TLittleBootstrapsTimetree = nil;
  aTime: Double = -1;
  r: Double = -1;
begin
  aTimetree := TLittleBootstrapsTimetree(FTimetree);
  aTime := 2*aTimetree.GetIngroupRootDivTime/10;
  if abs(aTime) < 0.00000000000001 then
    Result := 0
  else
  begin
    Result := aTime; // Scale*abs(aTime);
    r := Result/Power(10,Floor(log10(Result)));
    if Trunc(r + 0.5) >= 8 then
      Result := Power(10,Floor(log10(Result)) + 1)
    else if Trunc(r) >= 3 then
      Result := 5*Power(10,Floor(log10(Result)))
    else if (Trunc(r) = 2) or (Trunc(r + 0.5) = 2) then
      Result := 2*Power(10,Floor(log10(Result)))
    else
      Result := Power(10,Floor(log10(Result)));

    FTimeTick := Result;

    if order(abs(Result)) >= 1 then
      FTimeText := FloatToStrF(Result + LatestTime,DecimalDisplayMode,15, 0)
    else
      FTimeText := FloatToStrF(Result + LatestTime,DecimalDisplayMode, 15, Abs(order(Result)));
  end;
end;

procedure TCustomTimetreeBox.FindOutgroup;
var
  i: Integer = -1;
begin
  for i := 1 to NoOfOTUs do
    if Node^[i].outgroup then
    begin
      FOutgroup := Node[i];
      break;
    end;
  Assert(Assigned(FOutgroup), 'failed to locate outgroup taxon');
end;

procedure TCustomTimetreeBox.SetCalibrations(aCalibrations: TCalibrations);
begin
  TLittlebootstrapsTimetree(FTimetree).SetCalibrations(aCalibrations);
end;

constructor TCustomTimetreeBox.Create(aTimetree: TObject; aOwner: TComponent; aType: TTreeboxType);
begin
  inherited Create(aOwner, aType);
  FTimetree := aTimetree;
  FShowBLen := False;
  FShowRoot := False;
  FisRooted := True;
  IsTimes := True;
  FShowDivergenceTimes := True;
  FShowTimeScale := True;
  FShowScale := False;
  TimeScale := GetBestTimescale;
  FShowHeightErrBar := False;
  FMaxTimeFunc := MaxTime;
  FMinTimeFunc := Mintime;
  FDivTimeFunc := DivTime;
  FHeightErrBarProc := ErrBarBounds;
  FTimeFactor := 1;
  FShowStats := False;
  FShowNodeIds := False;
  FShowDataCoverage := False;
  FShowStatsRange := False;
end;

destructor TCustomTimetreeBox.Destroy;
begin
  if Assigned(FTimetree) then
  begin
    Assert(FTimetree is TLittleBootstrapsTimetree);
    TLittleBootstrapsTimetree(FTimetree).Free;
  end;
  inherited Destroy;
end;

function TCustomTimetreeBox.ReadFromFile(var data: File; sessionFileVersion: integer): boolean;
var
  aTimeTree: TLittleBootstrapsTimetree = nil;
  i: Integer = -1;
begin
  Result := inherited ReadFromFile(data, SessionFileVersion);
  for i := 1 to NoOfOTUs do
    Node[i].outgroup := TreeList[0].IsOutgroupMember[i - 1];

  try
    if Result then
    begin
      aTimeTree := TLittleBootstrapsTimetree.CreateFromSessionFile(data, sessionFileVersion, TreeList.NoOfOTUs, TreeList.OTUNameList);
      FTimeTree := aTimeTree;
    end
    else
      raise Exception.Create('TTreeBox.ReadFromFile failed');
  except
    on E:Exception do
    begin
      Result := False;
    end;
  end;
end;

procedure TCustomTimetreeBox.WriteToFile(var data: File);
var
  aTimeTree: TLittleBootstrapsTimetree = nil;
begin
  inherited WriteToFile(data);
  aTimeTree := TLittleBootstrapsTimetree(FTimetree);
  if not aTimeTree.SaveToSessionFile(data) then
    raise Exception.Create('failed to save TCustomTimetreeBox to session file');
end;

function TCustomTimetreeBox.MinTime(NodeIndex: Integer): Double;
begin
  if Assigned(FTimetree) then
    Result := TLittleBootstrapsTimetree(FTimetree).MinTime(NodeIndex - 1)
  else
    Result := -1;
end;

function TCustomTimetreeBox.MaxTime(NodeIndex: Integer): Double;
begin
  if Assigned(FTimetree) then
    Result := TLittleBootstrapsTimetree(FTimetree).MaxTime(NodeIndex - 1)
  else
    Result := -1;
end;

function TCustomTimetreeBox.DivTime(NodeIndex: Integer): Double;
begin
  if Assigned(FTimetree) then
    Result := TLittleBootstrapsTimetree(FTimetree).DivTime(NodeIndex - 1)
  else
    Result := -1;
end;

function TCustomTimetreeBox.GetNewickNoOutgroup: String;
begin
  Assert(FTimetree is TLittleBootstrapsTimetree);
  Result := TLittlebootstrapsTimetree(FTimetree).GetNewickNoOutgroup(TLittleBootstrapsTimetree(FTimetree).HasCalibrations);
end;

procedure TCustomTimetreeBox.ErrBarBounds(NodeIndex: Integer; var errHigh: Double; var errLow: Double);
begin
  if Assigned(FTimetree) then
    TLittleBootstrapsTimetree(FTimetree).ErrorBarHeight(NodeIndex - 1, errHigh, errLow)
  else
  begin
    errLow := -1;
    errHigh := -1;
  end;
end;

{ TTreeCustomControl}

constructor TTreeCustomControl.Create(AOwner: TComponent; aType: TTreeBoxType);
begin
    inherited Create(AOwner);
    FScaleBarHeight := 0;
    FCalibrationFunc := nil;
    FTreeDisplaySettingsCache := TTreeDisplaySettingsCache.Create;
    FTaxaNameSearch := TTaxaNamesSearch.Create;
    FAlignOtuNames := False;
    FDrawBoxForHighlightedNames := False;
    FShowNodeLabels := False;
    FShowCompressedClusterSize := False;
    FIsDrawing := False;
    IsLivingSequences := False;
    FIsDoingCalibrations := False;
    FHideOverlappingDivTimes := True;
    FTreeBoxType := aType;
    FParentFormIsClosing := False;
    FIsPainting := False;
    FTreeDrawingTarget := tdtCanvas;
    InitRenderers;
    FIsUPGMA := False;
    IsSamplingTimes := False;
    SkipDrawing := False;
    FDrawFullTree := False;
    Governer := 1;
    FNamesAreChanged := False;
    FShowDataCoverage := False;
    IsGeneDups := False;
    FShowGeneDupMarkers := False;
    FShowSpeciationMarkers := False;
    FDecimalDisplayMode := ffFixed;
    DivTimeDecimals := 2;
    DataCoverageDecimals := 0;
    HideOverlappingTaxa := True;
    ControlStyle := ControlStyle + [csReplicatable];
    Width := 105;
    Height := 105;
    FNodeInfo := TNodeInfo.Create(Self);
    FBranchInfo := TBranchInfo.Create(Self);
    EditBox := nil;
    AttribList := TNodeAttribList.Create;
    AttribList.Add(TNodeAttrib.Create);

    FBranchPen := TPen.Create;
    FBranchPen.Assign(AttribList[0].FLinePen);
    FBrush := TBrush.Create;
    FBrush.Assign(AttribList[0].FBrush);

    FScalePen := TPen.Create;
    FEdgePen := TPen.Create;
    FMarkerPen := TPen.Create;
    FFillBrush := TBrush.Create;
    FOpenBrush := TBrush.Create;

    FAutoSize := true;
    FTreeExist := false;
    hMetaFile := 0;
    FNoOfOTUs := 0;
    FStatsCutOff := 0;
    FCondenseValue := 50;
    FMaxStats := 0.0;
    FFocusedIndex := 0;
    ShowCalibratedNodeMarker := True;
    FFocusedNameIndex := 0;
    FNodeFocused := false;
    FBranchFocused := false;
    FTimeFocused := false;
    FDistanceMatrix := nil;
    FStatsPosition := bipAutomatic;
    FTimesPosition := bipAutomatic;
    FBLenPosition := bipAutomatic;
    FShowStats := true;
    FShowDivergenceTimes := False;
    FShowNodeIds := False;
    FIsTimes := False;
    FShowBLen := false;
    FShowCharState := false;
    FPixelsPerUnit := 0;
    FShowScale := true;
    FShowTimeScale := true;
    FShowSamplingTimeScale := False;
    FTimeFactor := 0.0;
    MaxCharState := 0;
    SessionIsReltime := False;
    FShowSelection := true;
    FFillSubtreeDelta := true;
    FHorzTaxonName := false;
    FAlignCaption := false;
    FShowRoot := true;
    FTreeWidth := TE_DEFAULT_TREE_WIDTH;
    MinTreeWidth := TE_DEFAULT_TREE_WIDTH;
    FRadius := 300;
    FCenterMargin := 20;
    FStartAngle := 0;
    yunit := Abs(Font.Height)*6;
    gunit := 8;
    FBLenCutoff := 0.0;
    FBLenDecimals := 0;
    FStatsMargin.X := 12;
    FStatsMargin.Y := 4;
    FTimesMargin.X := 80;
    FTimesMargin.Y := 16;
    FHilightColor := clRed;
    GroupAttrib := TNodeAttribList.Create;
    FUseSubtreeAttrib := true;
    FUseGroupAttrib   := False;
    FLinearizeFunc := nil;
    FScaleUnit := EmptyStr;
    FTopologyOnly := False;
end;

destructor TTreeCustomControl.Destroy;
var
  i: integer;
begin
    if Assigned(FTaxaNameSearch) then
      FTaxaNameSearch.Free;
    if Assigned(FTreeDisplaySettingsCache) then
      FTreeDisplaySettingsCache.Free;
    ResetMem;

    FScalePen.Free;
    FEdgePen.Free;
    FMarkerPen.Free;
    FBranchPen.Free;
    FFillBrush.Free;
    FOpenBrush.Free;
    FBrush.Free;

    if AttribList.Count > 0 then
      for i := AttribList.Count-1 downto 0 do
        AttribList[i].Free;
    AttribList.Free;

    if GroupAttrib.Count > 0 then
      for i := GroupAttrib.Count-1 downto 0 do
        GroupAttrib[i].Free;
    GroupAttrib.Free;

    if Assigned(EditBox) then
      EditBox.Free;

    FBranchInfo.Free;
    FNodeInfo.Free;
    FreeRenderers;
    inherited Destroy;
end;

procedure TTreeCustomControl.GetIntCoords(i: integer; var des1, des2: integer);
begin
  des1 := Node[i].des1.index;
  des2 := Node[i].des2.index;
end;

function TTreeCustomControl.GetNoOfNodes:integer;
begin
  Result := 2*NoOfOTUs -1;
end;

function TTreeCustomControl.GetNumGeneDups: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := NoOfOtus + 1 to NoOfNodes do
    if Node[i].IsGeneDuplication then
      inc(Result);
end;

function TTreeCustomControl.GetNumSpeciations: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := NoOfOtus + 1 to NoOfNodes do
    if Node[i].IsSpeciationEvent then
      inc(Result);
end;

function TTreeCustomControl.GetPixelFormat(aRect: TRect): TPixelFormat;
const
  _2GB = 2147483647;
var
  MemRequired: Int64;
  TempBitmap: Graphics.TBitmap;
begin
  try
    try
      { test if we can draw a device dependent bitmap}
      TempBitmap := Graphics.TBitmap.Create;
      TempBitmap.SetSize(RectWidth(aRect), RectHeight(aRect));
      Result := pfDevice;
    except
      { it failed so try and draw a device independent bitmap}
      MemRequired := RectWidth(aRect) * RectHeight(aRect);
      if MemRequired * 4 < _2GB then
        Result := pf32bit
      else if MemRequired * 3 < _2GB then
        Result := pf24bit
      else if MemRequired * 2 < _2GB then
        Result := pf16bit
      else if MemRequired * 1 < _2GB then
        Result := pf8bit
      else if MemRequired * 0.5 < _2GB then
        Result := pf4bit
      else if MemRequired < _2GB then
        Result := pf1bit
      else
        raise Exception.Create('The memory required to draw the image exceeds the 2GB limit set by Windows OS');
    end;
  finally
    TempBitmap.Free;
  end;
end;

procedure TTreeCustomControl.MPolyline(p: array of TPoint);
{$IFDEF DARWIN}
var
  size: Integer;
{$ENDIF}
begin
  FRenderer.MPolyline(p);
  {$IFDEF DARWIN}
  size := Length(p);
  AddToSavedLines(p, size);
  {$ENDIF}
end;

procedure TTreeCustomControl.MTextOut(const x: Integer; const y: Integer; const str: String; const orientation: Integer = 0);
begin
  FRenderer.MTextOut(x, y, str, orientation);
end;

procedure TTreeCustomControl.MPolygon(const p: array of TPoint);
begin
  FRenderer.MPolygon(p);
end;

procedure TTreeCustomControl.MRectangle(const p: array of TPoint);
begin
  FRenderer.MRectangle(p, Canvas.Brush.Style = bsSolid);
end;

procedure TTreeCustomControl.MRoundRectangle(aRect: TRect; const rx, ry: Integer);
begin
  FRenderer.MRoundRectangle(aRect, Canvas.Brush.Style = bsSolid, rx, ry);
end;

procedure TTreeCustomControl.MPolyBezier(const p: array of TPoint;
  const n: Integer; Filled: Boolean; Continuous: Boolean);
begin
  FRenderer.MPolyBezier(p, n, Filled, Continuous);
end;

procedure TTreeCustomControl.MDrawImage(const x: Integer; const y: Integer;
  const aBitmap: TBitmap);
begin
  FRenderer.MDrawImage(x, y, aBitmap);
end;

procedure TTreeCustomControl.MDrawArc(const aLeft: Integer;
  const aTop: Integer; const aRight: Integer; const aBottom: Integer;
  const startAngle16deg: Integer; const angleLength16deg: Integer);
begin
  FRenderer.MDrawArc(aLeft, aTop, aRight, aBottom, startAngle16deg, angleLength16deg);
end;

procedure TTreeCustomControl.MDrawCircle(const aRect: TRect);
begin
  FRenderer.MDrawEllipse(aRect);
end;

procedure TTreeCustomControl.MDrawSquare(x, y, r: Integer; a: Double);
begin
  FRenderer.MDrawSquare(x, y, r, a, Canvas.Brush.Style = bsSolid);
end;

procedure TTreeCustomControl.Paint;
begin
  if SkipDrawing then
    Exit;
  try
    FIsPainting := True;
    if TreeExist then
      Draw
    else
      DrawBackground;
    if Assigned(FOnPaint) then
      FOnPaint(Self);
  finally
    FIsPainting := False;
  end;
end;

procedure TTreeCustomControl.UpdateScrollPosition;
begin
  inherited UpdateScrollPosition;
  if Assigned(EditBox) and TEdit(EditBox).Visible then
    TEdit(EditBox).Hide;
  Repaint;
end;

procedure TTreeCustomControl.DblClick;
begin
  if EditNameEnabled and NameFocused then
    StartUserEditingName;
  if EditTimesEnabled and (FocusedTimeIndex > 0) then
    StartUserEditingTime;
  inherited;
end;

function TTreeCustomControl.FocusOnPoint(Point: TPoint): Boolean;
begin
  Result := true;
  { reordered so more efficient functions are checked first }
  if not FocusName(Point.X, Point.Y) then     
    if not FocusNode(Point.X, Point.Y) then
       if not FocusBranch(Point.X, Point.Y) then
	       if not FocusTime(Point.X, Point.Y) then
	         Result := false;
end;

procedure TTreeCustomControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FParentFormIsClosing then Exit;
  if @OnMouseDown <> nil then
    OnMouseDown(Self, Button, Shift, X, Y);

  if FShowSelection then
  begin
    if Button = mbLeft then
    begin
      { reuse code instead of reimplementing }
      if not FocusOnPoint(Point(X, Y)) then
         inherited;
    end
    else
      inherited;

    if NodeFocused or BranchFocused or TimeFocused then
      SetFocus;
  end
  else
    Inherited;
end;

procedure TTreeCustomControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FParentFormIsClosing then Exit;
  if Assigned(@OnMouseUp) then
    OnMouseUp(Self, Button, SHift, X, Y)
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TTreeCustomControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FParentFormIsClosing then Exit;
  //if Assigned(@DoMouseMove) then
  //  DoMouseMove(Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TTreeCustomControl.KeyDown(var Key: Word; Shift: TShiftState);
var p: TpNode;
begin
  if @FOnKeyDown <> nil then
    OnKeyDown(Self, Key, Shift);
  if (FocusedIndex > 0) and (Shift = []) then
    case Key of
      VK_LEFT : if NodeFocused and (FocusedIndex <> Root.index) then
                begin
                  p := Node[FocusedIndex].anc;
                  while (p <> Root) and p.hidden do
                    p := p.anc;
                  FocusOnBranch(FocusedIndex);
                end
                else if BranchFocused then
                begin
                  p := Node[FocusedIndex].anc;
                  while (p <> Root) and p.hidden do
                    p := p.anc;
                  FocusOnNode(p.index);
                end;
      VK_RIGHT: if BranchFocused then
                  FocusOnNode(FocusedIndex);
      VK_UP   : if NodeFocused and (not Node[FocusedIndex].OTU) and (not Node[FocusedIndex].compressed) then
                begin
                  p := Node[FocusedIndex].des1;
                  while (not p.OTU) and p.hidden do
                    p := p.des1;
                  FocusOnBranch(p.index);
                end
                else if BranchFocused then
                begin
                  p := Node[FocusedIndex];
                  while (not p.OTU) and p.hidden do
                    p := p.des1;
                  FocusOnNode(FocusedIndex);
                end;
      VK_DOWN : if NodeFocused and (not Node[FocusedIndex].OTU) and (not Node[FocusedIndex].compressed) then
                begin
                  p := Node[FocusedIndex].des2;
                  while (not p.OTU) and p.hidden do
                    p := p.des2;
                  FocusOnBranch(p.index);
                end
                else if BranchFocused then
                begin
                  p := Node[FocusedIndex];
                  while (not p.OTU) and p.hidden do
                    p := p.des2;
                  FocusOnNode(FocusedIndex);
                end;
      VK_HOME : FocusOnNode(Root.index);
    end
  else
    inherited;
  Invalidate;
end;

procedure TTreeCustomControl.AssignTreeAttrib(Source: TTreeCustomControl);
begin
    if Source.isBranchLength then
      if isBranchLength then
        PixelsPerUnit := Source.PixelsPerUnit
      else
        TreeWidth := Source.TreeWidth
    else
      TreeWidth := Source.TreeWidth;
    PixelsPerOTU := Source.PixelsPerOTU;

    FRadius := Source.FRadius;
    FStartAngle := Source.FStartAngle;
    FCenterMargin := Source.FCenterMargin;

    FBLenCutoff := Source.FBLenCutoff;
    FBLenPosition := Source.FBLenPosition;

    FStatsCutoff := Source.FStatsCutoff;
    FCondenseValue := Source.FCondenseValue;
    FStatsPosition := Source.FStatsPosition;
    FTimesPosition := Source.FTimesPosition;
    FStatsMargin := Source.FStatsMargin;
    FTimesMargin := Source.FTimesMargin;

    ShowOTUName := Source.ShowOTUName;
    ShowSelection := Source.ShowSelection;
    ShowOTUMarker := Source.ShowOTUMarker;
    FShowBLen := Source.FShowBLen;
    FShowStats := Source.FShowStats;
    FShowDivergenceTimes := Source.FShowDivergenceTimes;
    FShowNodeIds := Source.FShowNodeIds;
    FShowScale := Source.FShowScale;
    FShowTimeScale := Source.FShowTimeScale;
    FShowSamplingTimeScale := Source.FShowSamplingTimeScale;
    FShowCharState := Source.FShowCharState;

    FOTU_Font.Assign(Source.FOTU_Font);
    FStatsFont.Assign(Source.FStatsFont);
    FBLensFont.Assign(Source.FBLensFont);
    FTimesFont.Assign(Source.FTimesFont);
    FCharStateFont.Assign(Source.FCharStateFont);
    FScaleFont.Assign(Source.FScaleFont);
    FBranchPen.Assign(Source.FBranchPen);
    FScalePen.Assign(Source.FScalePen);

    FForceLinearized := Source.FForceLinearized;
    if IsDistanceMatrix and Source.IsDistanceMatrix then
      FIsLinearized := Source.FIsLinearized;

    FLinearizeFunc := Source.FLinearizeFunc;
    SetTopologyOnly(Source.FTopologyOnly);
    IsCondensed  := Source.IsCondensed;


    FTreeStyle := Source.FTreeStyle;
    FBranchStyle := Source.FBranchStyle;

    FHorzTaxonName := Source.FHorzTaxonName;
    FAlignCaption := Source.AlignCaption;

    FFillSubtreeDelta := Source.FillSubtreeDelta;

    FUseSubtreeAttrib := Source.FUseSubtreeAttrib;
    FUseGroupAttrib   := Source.FUseGroupAttrib;

    SetAttrIndex;
    Refresh;
end;

procedure TTreeCustomControl.SetOTU_Font(f : TFont);
begin
    FOTU_Font := f;
    SetAttrindex;
end;

procedure TTreeCustomControl.SetStatsFont(f : TFont);
begin
    FStatsFont := f;
end;

procedure TTreeCustomControl.SetBLensFont(f: TFont);
begin
  FBLensFont := f;
end;

procedure TTreeCustomControl.SetTimesFont(f : TFont);
begin
  FTimesFont := f;
end;

procedure TTreeCustomControl.SetCharStateFont(f : TFont);
begin
    FCharStateFont := f;
end;

procedure TTreeCustomControl.SetScaleFont(f : TFont);
begin
    FScaleFont := f;
end;

function TTreeCustomControl.GetBranchPen: TPen;
begin
  Result := FBranchPen;
end;

procedure TTreeCustomControl.SetBranchPen(p : TPen);
begin
    FBranchPen.Assign(p);
end;

procedure TTreeCustomControl.SetMarker(index : integer; newstyle : TNodeMarker{TMarkerItem});
begin
    if (index < 1) or (index > NoOfNodes) then Exit;
    Node[index].marker := newstyle;
end;

function TTreeCustomControl.GetMarker(index : integer):TNodeMarker;//TMarkerItem;
begin
    if (index < 1) or (index > NoOfNodes) then begin
        Result.Shape := msNone;
        Exit;
    end;
    Result := Node[index].marker;
end;

procedure TTreeCustomControl.SetScalePen(p : TPen);
begin
    FScalePen.Assign(p);
end;

procedure TTreeCustomControl.SetIsCondensed(value: boolean);
begin
  if not TreeExist then Exit;
  if not IsStats then Exit;
  if value = FIsCondensed then Exit;
  FIsCondensed := value;
  if FIsCondensed then
    Topoflag := true
  else
    Topoflag := FTopologyOnly;
end;

function TTreeCustomControl.GetIsCondensed: boolean;
begin
    Result := FIsCondensed;
end;

procedure TTreeCustomControl.SetTopologyOnly(value : boolean);
begin
  if value = ShowTopologyOnly then Exit;
  FTopologyOnly := value;
  if FTopologyOnly then
  begin
    Topoflag := true;
  end
  else
  begin
    Topoflag := false;
    FIsCondensed := false;
  end;
end;

function TTreeCustomControl.GetTopologyOnly: boolean;
begin
  if IsCondensed then
    Result := true
  else
    Result := FTopologyOnly;
end;

procedure TTreeCustomControl.SetForceLinearized(value: boolean);
begin
  FForceLinearized := value;
end;

function TTreeCustomControl.GetShowOTUName: boolean;
begin
  result := AttribList[0].ShowTaxonName;
end;

function TTreeCustomControl.GetShowSelection: Boolean;
begin
  result := FShowSelection;
end;

function TTreeCustomControl.GetShowSpeciesName: Boolean;
begin
  result := AttribList[0].ShowSpeciesName;
end;

procedure TTreeCustomControl.GetSimpleNodeData(var NodeData: TSimpleTreeNodeArray);
var
  i: Integer;
begin
  if Length(NodeData) <> NoOfNodes then
    SetLength(NodeData, NoOfNodes);

  for i := 0 to NoOfNodes - 1 do
  begin
    if not Assigned(NodeData[i]) then
    begin
      NodeData[i] := TSimpleTreeNode.Create;
      NodeData[i].NodeIndex := i;
    end;

  end;

  for i := 0 to NoOfNodes - 1 do
  begin
    if Assigned(Node[i + 1].anc) then
      NodeData[i].Ancestor := NodeData[Node[i + 1].anc.index - 1];
    NodeData[i].IsOtu := Node[i + 1].OTU;
    if NodeData[i].IsOtu then
      NodeData[i].SpeciesName := Node[i + 1].SpeciesName
    else
    begin
      NodeData[i].Des1 := NodeData[Node[i + 1].des1.index  - 1];
      NodeData[i].Des2 := NodeData[Node[i + 1].des2.index - 1];
    end;
    NodeData[i].SequenceName := Node[i + 1].name;
  end;
end;

procedure TTreeCustomControl.SetShowSelection(b : boolean);
begin
  FShowSelection := b;
end;

procedure TTreeCustomControl.SetShowSpeciesName(const Value: Boolean);
var
  i: Integer;
begin
  if Value = ShowSpeciesName then Exit;
  for i := 0 to AttribList.Count - 1 do
    AttribList[i].ShowSpeciesName := Value;
  if GroupAttrib.Count > 0 then
    for i := 0 to GroupAttrib.Count - 1 do
      GroupAttrib[i].ShowSpeciesName := Value;
end;

procedure TTreeCustomControl.SetShowOTUName(b : boolean);
var
  i: integer;
begin
  if b = ShowOTUName then Exit;
  for i := 0 to AttribList.Count-1 do
    AttribList[i].ShowTaxonName := b;
  if GroupAttrib.Count > 0 then
    for i := 0 to GroupAttrib.Count-1 do
      GroupAttrib[i].ShowTaxonName := b;
end;

function TTreeCustomControl.GetShowOTUMarker: boolean;
begin
  result := AttribList[0].ShowTaxonMarker;
end;

procedure TTreeCustomControl.SetShowOTUMarker(b : boolean);
var
  i: integer;
begin
  if b = ShowOTUMarker then Exit;
  for i := 0 to AttribList.Count-1 do
    AttribList[i].ShowTaxonMarker := b;
  if GroupAttrib.Count > 0 then
    for i := 0 to GroupAttrib.Count-1 do
      GroupAttrib[i].ShowTaxonMarker := b;
end;

procedure TTreeCustomControl.SetBranchStyle(value : TBranchStyle);
begin
    if value = FBranchStyle then Exit;
    FBranchStyle := value;
end;

procedure TTreeCustomControl.SetTreeStyle(value : TTreeStyle);
begin
  if value = FTreeStyle then Exit;
  if (value = tsRadiation) and (not IsBranchLength) then Exit;
  FTreeStyle := value;
end;

procedure TTreeCustomControl.SetFillSubtreeDelta(value: boolean);
begin
  if value = FillSubtreeDelta then Exit;
  FFillSubtreeDelta := value;
end;

function TTreeCustomControl.GetIsStats:boolean;
begin
    Result := FisStats;
end;

function TTreeCustomControl.GetIsTimes: Boolean;
begin
  Result := FisTimes;
end;

procedure TTreeCustomControl.SetStatsCutoff(value : integer);
begin
    if (value < 0) or (value > 100) then Exit;
    FStatsCutoff := value;
end;

function TTreeCustomControl.GetIsBranchLength:boolean;
begin
    Result := (FSBL > 0.00000000000001);
end;

procedure TTreeCustomControl.SetBLenCutoff(value : double);
begin
    if (value < 0) then Exit;
    if (value > xmax) then
       FBLenCutoff := xmax
    else
       FBLenCutoff := value;
end;

procedure TTreeCustomControl.SetShowBLen(b : boolean);
begin
    if b = ShowBLen then Exit;
    FShowBLen := b;
end;

function TTreeCustomControl.GetShowBLen:boolean;
begin
    if not (TreeStyle = tsTraditional) then
        Result := false
    else
        Result := FShowBLen;
end;

function TTreeCustomControl.GetIsSE:boolean;
begin
    Result := FisSE;
end;

function TTreeCustomControl.GetShowCharState:boolean;
begin
  if TreeStyle <> tsTraditional then
    Result := false
  else if BranchStyle <> bsRectangular then
    Result := false
  else
    Result := FShowCharState;
end;

procedure TTreeCustomControl.SetStatsPosition(position : TBranchInfoPosition);
begin
    if position = FStatsPosition then Exit;
    FStatsPosition := position;
end;

procedure TTreeCustomControl.SetTimesPosition(Position: TBranchInfoPosition);
begin
  if Position = FTimesPosition then Exit;
  FTimesPosition := Position
end;

procedure TTreeCustomControl.SetStatsMargin(margin : TPoint);
begin
    if (margin.X = FStatsMargin.X) and (margin.Y = FStatsMargin.Y) then Exit;
    FStatsMargin.X := margin.X;
    FStatsMargin.Y := margin.Y;
end;

procedure TTreeCustomControl.SetTimesMargin(Margin: TPoint);
begin
  if (Margin.X = FTimesMargin.X) and (Margin.Y = FTimesMargin.Y) then Exit;
    FTimesMargin.X := Margin.X;
    FTimesMargin.Y := Margin.Y;
end;

procedure TTreeCustomControl.SetBLenPosition(position : TBranchInfoPosition);
begin
  if position = FBLenPosition then Exit;
  FBLenPosition := position;
end;

procedure TTreeCustomControl.SetBLenDecimals(value : integer);
begin
    if value = BLenDecimals then Exit;
    FBLenDecimals := value;
end;

function TTreeCustomControl.GetTreeWidth:integer;
begin
    if Topoflag then
        Result := MinTreeWidth
    else
        Result := FTreeWidth;
end;

procedure TTreeCustomControl.SetTreeWidth(w : integer);
begin
  if not TreeExist then Exit;
  if w = TreeWidth then Exit;
  if w < 80 then
      w := 80
  else if w > 32767 then
      w := 32767;
  if isBranchLength then
      FPixelsPerUnit := FPixelsPerUnit*w/FTreeWidth;

  if TreeWidth = MinTreeWidth then begin
      FTreeWidth := w;
      MinTreeWidth := w;
  end
  else begin
      MinTreeWidth := Round(w/FTreeWidth*MinTreeWidth);
      FTreeWidth := w;
  end;
  if @FOnTreeResize <> nil then FOnTreeResize(Self);
end;

procedure TTreeCustomControl.SetPixelsPerUnit(value : double);
var
  aTreeWidth: Double = -1;
begin
    if not TreeExist then Exit;
    if not isBranchLength then Exit;
    if FPixelsPerUnit = value then Exit;
    if value < 0 then exit; // Exit if the input is a negative number
    aTreeWidth := value/FPixelsPerUnit*FTreeWidth;
    if aTreeWidth > 8000 then exit;
    FTreeWidth := Round(aTreeWidth);
    MinTreeWidth := Round(value/FPixelsPerUnit*MinTreeWidth);
    FPixelsPerUnit := value;
end;

procedure TTreeCustomControl.SetClusterWidth(p: TpNode);
begin
  if not TreeExist then Exit;
  if p.OTU then
    p.width := yunit
  else if p.compressed then
    if gunit*p.size < yunit then
      p.width := yunit
    else
      p.width := gunit*p.size
  else
  begin
    SetClusterWidth(p.des1);
    SetClusterWidth(p.des2);
    p.width := p.des1.width + p.des2.width;
  end;
end;

function TTreeCustomControl.GetTreeHeight:integer;
begin
  Result := Root.width div 4;
end;


function TTreeCustomControl.GetCurNoOfOTUs:integer;

  procedure SearchCurOTUs(p: TpNode);
  begin
    if p.OTU or p.compressed then
      Inc(Result)
    else begin
      SearchCurOTUs(p.des1);
      SearchCurOTUs(p.des2);
    end;
  end;

begin
  Result := 0;
  SearchCurOTUs(Root);
end;

procedure TTreeCustomControl.SetPixelsPerOTU(value : integer);
begin
  if value < 0 then Exit;
  if value >= 2048 then exit;
  if value = yunit then Exit;
  yunit := value;
  SetClusterWidth(Root);
  if @FOnTreeResize <> nil then
    FOnTreeResize(Self);
end;

function TTreeCustomControl.GetPixelsPerOTU:integer;
begin
    Result := yunit;
end;

function TTreeCustomControl.GetDpiRatio: Double;
Const
  C_KEY='Control Panel\Desktop\WindowMetrics';
  DEFAULT_DPI=96.0;
var
  RegistryEntry: TRegistry;
  dpi : double;
begin
  try
    try
    RegistryEntry := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
    RegistryEntry.RootKey := HKEY_CURRENT_USER;
    RegistryEntry.Access := KEY_READ or KEY_WOW64_64KEY;
    if RegistryEntry.KeyExists(C_KEY) then
    begin
      if RegistryEntry.OpenKeyReadOnly(C_KEY) then
        dpi := RegistryEntry.ReadInteger('AppliedDPI')
      else
        dpi := DEFAULT_DPI;
    end
    else
    begin
     dpi := DEFAULT_DPI;
    end;
    except
      dpi := DEFAULT_DPI;
    end;
  finally
    if Assigned(RegistryEntry) then
      RegistryEntry.Free;
  end;
  if dpi <= 125.0 then
    Result := 1.0
  else
    Result := dpi / DEFAULT_DPI;
end;

function TTreeCustomControl.VertLineWidth(p: TpNode): integer;
var
  q: TpNode;
begin
  if p = Root then
    result := 0
  else if GetNodeAttrib(p).BranchOption = boFullBranch then
    result := GetNodeAttrib(p).LineWidth
  else
  begin
    q := p.anc;
    while (q <> Root) and ((GetNodeAttrib(q).BranchOption = boBranchOnly) or q.hidden) do
      q := q.anc;
    result := GetNodeAttrib(q).LineWidth
  end;
end;

function TTreeCustomControl.HasOverlappingDivTime(aNode: TpNode; x: Integer; y: Integer; aText: String): Boolean;
var
  ts: TSize;
  i, j, j2: Integer;
begin
  Result := False;
  {$IFDEF DARWIN}
     if aNode.OTU then
    Exit;
  Result := True;
  ts := Canvas.TextExtent(aText);
  i := x;
  j := y;
  while (i < (x + ts.cx)) and (j < (y + ts.cy)) do
  begin
    if FSavedLines[i, j] <> False then
      Exit;
    inc(i);
    inc(j);
  end;
  if i < (x + ts.cx) then
  begin
    j := y;
    j2 := y + ts.cy;
    while (i < (x + ts.cx)) do
    begin
      if (FSavedLines[i, j] <> False) or (FSavedLines[i, j2] <> False) then
        Exit;
      inc(i);
    end;
  end;
  Result := False;
  {$ELSE}
  if aNode.OTU then
    Exit;
  Result := True;
  ts := Canvas.TextExtent(aText);
  i := x;
  j := y;
  while (i < (x + ts.cx)) and (j < (y + ts.cy)) do
  begin
    if Canvas.Pixels[i, j] <> clWhite then
      Exit;
    inc(i);
    inc(j);
  end;
  if i < (x + ts.cx) then
  begin
    j := y;
    j2 := y + ts.cy;
    while i < (x + ts.cx) do
    begin
      if (Canvas.Pixels[i, j] <> clWhite) or (Canvas.Pixels[i, j2] <> clWhite) then
        Exit;
      inc(i);
    end;
  end;

  Result := False;
  {$ENDIF}
end;

procedure TTreeCustomControl.DrawNodeIds;
var
  aText : String;
  clientX: Integer = -1;
  clientY: Integer = -1;
  x: Integer = -1;
  y: Integer = -1;
  i : integer;
begin
  if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then
    Exit;
  Canvas.Font.Assign(StatsFont);
  Canvas.Brush.Style := bsClear;
  for i := NoOfOTUs+1 to NoOfNodes do
  begin
    if not NeedsPainting(Node[i]) then
      Continue;
    if Node[i].hidden then
      Continue;

    if Node[i] = Root then
      Continue;

    with Node[i]^ do
    begin
      atext :=  '(' + IntToStr(Node[i].index) + ')';
      GetCoordsForNodeInfo(Node[i], aText, x, y);
      BoxToClient(x+xbase, y+ybase, clientX, clientY);
      MTextOut(clientX, clientY, atext);
    end;
  end;
end;

procedure TTreeCustomControl.DrawNodeLabels;
var
  aText : String = '';
  clientX: Integer = -1;
  clientY: Integer = -1;
  x: Integer = -1;
  y: Integer = -1;
  i : integer = -1;
begin
  if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then
    Exit;
  Canvas.Font.Assign(NodeLabelFont);
  Canvas.Brush.Style := bsClear;
  for i := NoOfOTUs + 1 to NoOfNodes do
  begin
    if not NeedsPainting(Node[i]) then
      Continue;
    if Node[i].hidden then
      Continue;

    if (Node[i] = Root) and (not FShowRoot) then
      Continue;

    with Node[i]^ do
    begin
      if FTreeIndex <> 0 then
        aText := TreeList[FTreeIndex - 1].InternalNodeLabel[i - NoOfOTUs - 1]
      else
        aText := EmptyStr;
      if aText = EmptyStr then
        aText := name;
      if aText <> EmptyStr then
      begin
        if hilighted then
        begin
          if CustomHighlightColor <> clWhite then
            Canvas.Font.Color := CustomHighlightColor
          else
            Canvas.Font.Color := HilightColor;
        end
        else
          Canvas.Font.Color := FNodeLabelFont.Color;

        GetCoordsForDivTime(Node[i], aText, x, y);
        { parameters adjusted for changes to GetCoordsForDivTime }
        BoxToClient(x, y, clientX, clientY);
        MTextOut(clientX, clientY, atext);
      end;
    end;
  end;
end;

procedure TTreeCustomControl.DrawDivergenceTimes;
var
  clientX: Integer = -1;
  clientY: Integer = -1;
  x: Integer = -1;
  y: Integer = -1;
  i: integer;
  DivTime: Double;
  DivTimeStr: String;
begin
  if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then
    Exit;

  Canvas.Font.Assign(TimesFont);
  Canvas.Brush.Style := bsClear;
  for i := NoOfOTUs+1 to NoOfNodes do
  begin
    if not NeedsPainting(Node[i]) then
      Continue;
    if Node[i].hidden or (Node[i] = Root) then
      Continue;

    //DivTime := Node[i]^.Height * TimeFactor + LatestTime;
    DivTime := GetDivergenceTime(i);
    DivTimeStr := FloatToStrF(DivTime, DecimalDisplayMode, 8, DivTimeDecimals);
    with Node[i]^ do
    begin
      if outgroup then
        Canvas.Font.Color := clSilver
      else
        Canvas.Font.Color := TimesFont.Color;
      GetCoordsForDivTime(Node[i], DivTimeStr, x, y);
      if (BoxToClient(x, y, clientX, clientY)=true) then
      begin
        if (not FHideOverlappingDivTimes) or (not HasOverlappingDivTime(Node[i], clientX, ClientY, DivTimeStr)) then
          MTextOut(clientX, clientY, DivTimeStr);
      end;
    end;
  end;
end;

procedure TTreeCustomControl.DrawCalibrations;
var
  clientX: Integer = -1;
  clientY: Integer = -1;
  x: Integer = -1;
  y: Integer = -1;
  i: integer = 0;
  minTime: Double = 0;
  maxTime: Double = 0;
  timeStr: String = '';
  isSelected: Boolean = True;
begin
  if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then
    Exit;
  if not Assigned(FCalibrationFunc) then
    Exit;

  Canvas.Font.Assign(TimesFont);
  Canvas.Brush.Style := bsClear;
  for i := NoOfOTUs + 1 to NoOfNodes do
  begin
    if not NeedsPainting(Node[i]) then
      Continue;
    if Node[i].hidden or (Node[i] = Root) then
      Continue;

    if FCalibrationFunc(i, minTime, maxTime, isSelected) then
    begin
      if CompareValue(minTime, maxTime, FP_CUTOFF) = 0 then
        timeStr := RemoveTrailingZeros(FloatToStrF(minTime, DecimalDisplayMode, 8, DivTimeDecimals))
      else if (CompareValue(minTime, 0, FP_CUTOFF) > 0) and (CompareValue(maxTime, 0, FP_CUTOFF) < 0) then
      begin
        timeStr := RemoveTrailingZeros(FloatToStrF(minTime, DecimalDisplayMode, 8, DivTimeDecimals));
        timeStr := Format('>= %s', [timeStr]);
      end
      else if (CompareValue(maxTime, 0, FP_CUTOFF) > 0) and (CompareValue(minTime, 0, FP_CUTOFF) < 0) then
      begin
        timeStr := RemoveTrailingZeros(FloatToStrF(maxTime, DecimalDisplayMode, 8, DivTimeDecimals));
        timeStr := Format('<= %s', [timeStr]);
      end
      else
        timeStr := RemoveTrailingZeros(FloatToStrF(minTime, DecimalDisplayMode, 8, DivTimeDecimals)) + ' - ' + RemoveTrailingZeros(FloatToStrF(maxTime, DecimalDisplayMode, 8, DivTimeDecimals));

      with Node[i]^ do
      begin
        if IsDoingCalibrations then
        begin
          if isSelected then
            Canvas.Font.Color := clRed
          else
            Canvas.Font.Color := clGrayText
        end
        else
          Canvas.Font.Color := TimesFont.Color;
        GetCoordsForDivTime(Node[i], timeStr, x, y);
        if BoxToClient(x, y, clientX, clientY) then
        begin
          //if (not FHideOverlappingDivTimes) or (not HasOverlappingDivTime(Node[i], clientX, ClientY, DivTimeStr)) then
          MTextOut(clientX, clientY, timeStr);
        end;
      end;
    end;
  end;
end;

procedure TTreeCustomControl.DrawDataCoverage;
var
  clientX: Integer = -1;
  clientY: Integer = -1;
  x: Integer = -1;
  y: Integer = -1;
  i: integer;
  DataCoverage: Double;
  DataCoverageStr: String;
begin
  if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) or IsDoingCalibrations then
    Exit;

  Canvas.Font.Assign(StatsFont);
  Canvas.Brush.Style := bsClear;
  for i := NoOfOTUs+1 to NoOfNodes do
  begin
    if not NeedsPainting(Node[i]) then
      Continue;
    if Node[i].hidden then
      Continue;

    if Node[i] = Root then
    begin
      Continue;
    end;

    DataCoverage := Node[i]^.dataCoverage;
    DataCoverageStr := FloatToStrF(DataCoverage * 100, DecimalDisplayMode, 8, DataCoverageDecimals) + '%';
    with Node[i]^ do
    begin
      GetCoordsForNodeInfo(Node[i], DataCoverageStr, x, y);
      BoxToClient(x+xbase, y+ybase, clientX, clientY);
      MTextOut(clientX, clientY, DataCoverageStr);
    end;
  end;
end;

procedure TTreeCustomControl.DrawStat;
var
  clientX: Integer = -1;
  clientY: Integer = -1;
  x: Integer = -1;
  y: Integer = -1;
  i: integer;
  aNode: TNode;
  aText: String;
  lBound: Int64 = 0;
  uBound: Int64 = 0;
begin
    if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) or (FIsDoingCalibrations) then Exit;

    Canvas.Font.Assign(StatsFont);
    Canvas.Brush.Style := bsClear;
    for i := NoOfOTUs+1 to NoOfNodes do
    begin
      if not NeedsPainting(Node[i]) then
        Continue;
      if (Node[i].hidden) or (Node[i] = Root) then Continue;
      if (Node[i].anc = Root) and not (isRooted and ShowRoot) then
      begin
          if Root.des1.OTU or Root.des2.OTU then Continue;
          if (Node[i] = Root.des1) then begin
             if (Node[i].position.x < Root.des2.position.x) then Continue;
          end
          else if (Node[i] = Root.des2) then begin
             if (Node[i].position.x <= Root.des1.position.x) then Continue;
          end;
      end;
      aNode := Node[i]^;
      with aNode do
      begin
        if FShowStats and (branch.stat2 < StatsCutOff) then Continue;
        if FShowStats or (CompareValue(branch.StatsStdDev, 0.0, FP_CUTOFF) <= 0) then
          aText := IntToStr(branch.stat2)
        else if FShowStatsRange then
        begin
          if CompareValue(branch.StatsStdDev, 0, FP_CUTOFF) > 0 then
          begin
            lBound := max(0, round(branch.stat2 - branch.StatsStdDev));
            uBound := min(100, round(branch.stat2 + branch.StatsStdDev));
          end
          else
          begin
            lBound := 0;
            uBound := 0;
          end;

          if lBound = uBound then
            aText := IntToStr(lBound)
          else
            aText := Format('%.d-%.d', [lBound, uBound]);
        end;
        GetCoordsForNodeInfo(Node[i], aText, x, y);
        BoxToClient(x+xbase, y+ybase, clientX, clientY);
        MTextOut(clientX, clientY, aText);
      end;
    end;
end;

procedure TTreeCustomControl.DrawStatsRadiation(p: TpNode);
var
  sibling: TpNode;
  text : String;
  radians: double;
  x,y, deltaHoriz, deltaVert : integer;
  tWidth, tHeight : integer;
  reverseText,CrampedWithAnc,CrampedWithDes1,CrampedWithDes2: boolean;
  degrees: Integer;
  multiplier: Double;
  clientX: Integer = -1;
  clientY: Integer = -1;
begin
    multiplier := -1.0;
    if p = root then Exit;
    if p = p.anc.des1 then
      sibling := p.anc.des2
    else
      sibling := p.anc.des1;

    if p.anc = root then
    begin
      if root.des1.OTU or root.des2.OTU then
        exit;
      if p.sector > sibling.sector then
        exit;
    end;
    if p.branch.stat2 < StatsCutOff then Exit;

    if ((p.angle > PI/2) and (p.angle < 3*PI/2)) or  { >90 and < 270, i.e. in quadrant II or quadrant III}
       ((p.angle < -PI/2) and (p.angle > -3*PI/2)) then { < -90 and > -270, i.e. in quadrant II or quadrant III}
      reverseText := true
    else
      reverseText := false;

    if reverseText then
      radians := p.angle +PI { increase by 180}
    else
      radians := p.angle;
    degrees := Round(radians/2/PI*3600); { convert radians to degress *100 (*100 needed by TFont)}

    Canvas.Font.Assign(StatsFont);
    text := IntToStr(p.branch.stat2);
    tWidth := Canvas.TextWidth(text);
    tHeight := Canvas.TextHeight(text);

    deltaHoriz := -(tWidth + StatsMargin.X + GetLineWidth(p)*2);
    if TreeStyle = tsCircle then
      deltaVert := -(tHeight + StatsMargin.Y + GetLineWidth(p)*2)
    else
      deltaVert := -(StatsMargin.Y + GetLineWidth(p)*2);

    if TreeStyle = tsRadiation then
    begin
      CrampedWithAnc := trunc(DistanceBetweenNodes(p, p.anc)) < Canvas.TextWidth(IntToStr(p.branch.stat2))*2 +StatsMargin.X +GetLineWidth(p);
      if p.des1.OTU then
        CrampedWithDes1 := false
      else
        CrampedWithDes1 := trunc(DistanceBetweenNodes(p, p.des1)) < (Canvas.TextWidth(IntToStr(p.des1.branch.stat2))+abs(StatsFont.Height))*1 +StatsMargin.X +GetLineWidth(p);
      if p.des2.OTU then
        CrampedWithDes2 := false
      else
        CrampedWithDes2 := trunc(DistanceBetweenNodes(p, p.des2)) < (Canvas.TextWidth(IntToStr(p.des2.branch.stat2))+abs(StatsFont.Height))*1 +StatsMargin.X +GetLineWidth(p);
      if reverseText then
      begin
        if CrampedWithAnc then
        begin
          if ((p.anc = root) or (p.anc.sector > 2*PI/3)) and (p.sector < PI/3) then
          begin
            if p = p.anc.des2 then
              deltaVert := Round(Abs(StatsFont.Height)*multiplier) - deltaVert;
          end
          else
            if p = p.anc.des1 then
              deltaVert := Round(Abs(StatsFont.Height)*multiplier) -deltaVert;
        end
        else if (not CrampedWithDes1) and CrampedWithDes2 then
        begin
          if not (((p.anc = root) or (p.anc.sector > 2*PI/3)) and (p.sector < PI/3)) then
            deltaVert := Round(Abs(StatsFont.Height)*multiplier) -deltaVert;
        end
        else if not(CrampedWithDes1 and (not CrampedWithDes2)) then
        begin
          if ((p.anc = root) or (p.anc.sector > 2*PI/3)) and (p.sector < PI/3) then
          begin
            if p = p.anc.des2 then
              deltaVert := Round(Abs(StatsFont.Height)*multiplier) - deltaVert;
          end
          else if p = p.anc.des1 then
            deltaVert := Round(Abs(StatsFont.Height)*multiplier) -deltaVert;
        end
      end
      else
      begin
        if CrampedWithAnc then
        begin
          if ((p.anc = root) or (p.anc.sector > 2*PI/3)) and (p.sector < PI/3) then
          begin
            if p = p.anc.des1 then
              deltaVert := Round(Abs(StatsFont.Height)*multiplier) -deltaVert;
          end
          else
            if p = p.anc.des2 then
              deltaVert := Round(Abs(StatsFont.Height)*multiplier) - deltaVert;
        end
        else if CrampedWithDes1 and (not CrampedWithDes2) then
        begin
          if not (((p.anc = root) or (p.anc.sector > 2*PI/3)) and (p.sector < PI/3)) then
            deltaVert := Round(Abs(StatsFont.Height)*multiplier) -deltaVert;
        end
        else if not ((not CrampedWithDes1) and CrampedWithDes2) then
        begin
          if ((p.anc = root) or (p.anc.sector > 2*PI/3)) and (p.sector < PI/3) then
          begin
           if p = p.anc.des1 then
            deltaVert := Round(Abs(StatsFont.Height)*multiplier) -deltaVert;
          end
          else if p = p.anc.des2 then
            deltaVert := Round(Abs(StatsFont.Height)*multiplier) - deltaVert;
        end
      end;
      if not (CrampedWithAnc or CrampedWithDes1 or CrampedWithDes2) then
        deltaVert := 1;
    end
    else
      if reverseText then
      begin
        if p = p.anc.des1 then
          deltaVert := Abs(StatsFont.Height) - deltaVert
      end
      else if p = p.anc.des2 then
      begin
        deltaVert := Abs(StatsFont.Height) - GetLineWidth(p); //-deltaVert;
      end;

    if reverseText then
    begin
      x := p.position.x -Round(Cos(radians)*deltaHoriz -Sin(radians)*deltaVert) -Round(cos(radians)*tWidth);
      y := p.position.y +Round(Sin(radians)*deltaHoriz +Cos(radians)*deltaVert) +Round(sin(radians)*tHeight);

      //x := x - Round(Cos(radians)*dx -Sin(radians)*dy) - Round(cos(radians)*p.namesize.x) - Round(10*Cos(radians));
      //y := y + Round(Sin(radians)*dx +Cos(radians)*dy) + Round(sin(radians)*p.namesize.x) + Round(10*Sin(radians));
    end
    else
    begin
      x := p.position.x +Round(Cos(radians)*deltaHoriz +Sin(radians)*deltaVert);
      y := p.position.y -Round(Sin(radians)*deltaHoriz -Cos(radians)*deltaVert);
    end;
    BoxToClient(x+xbase, y+ybase, clientX, clientY) ;
    RotateText(Rect(clientX, clientY, clientX + StrLength(text, Canvas.Font), clientY + StrLength(text, Canvas.Font)), text, rtCustom, degrees);
end;

procedure TTreeCustomControl.SetPixelsPerGroupMember(value : integer);
begin
  if value < 0 then Exit;
  if value >= 8192 then exit;
  if value = gunit then Exit;
  gunit := max(1, value);
  SetClusterWidth(Root);
end;

function TTreeCustomControl.GetPixelsPerGroupMember:integer;
begin
    Result := gunit;
end;

procedure TTreeCustomControl.SetTimeFactor(value : double);
var r : double;
begin
    if not TreeExist then Exit;
    if not IsBranchLength then exit;
    FTimeFactor := value;
    if abs(FTimeFactor) < 0.00000000000001 then
      TimeScale := 0
    else
    begin
        TimeScale := Scale*abs(TimeFactor);
        r := TimeScale/Power(10,Floor(log10(TimeScale)));
        if Trunc(r+0.5) >= 8 then
            TimeScale := Power(10,Floor(log10(TimeScale))+1)
        else if Trunc(r) >= 3 then
            TimeScale := 5*Power(10,Floor(log10(TimeScale)))
        else if (Trunc(r) = 2) or (Trunc(r+0.5) = 2) then
            TimeScale := 2*Power(10,Floor(log10(TimeScale)))
        else
            TimeScale := Power(10,Floor(log10(TimeScale)));

        if TimeFactor < 0 then
          TimeScale := -TimeScale;

        FTimeTick := TimeScale;

        if order(abs(TimeScale)) >= 1 then
            FTimeText := FloatToStrF(TimeScale+LatestTime,DecimalDisplayMode,15, 0)
        else
            FTimeText := FloatToStrF(TimeScale+LatestTime,DecimalDisplayMode, 15, Abs(order(TimeScale)));
    end;
end;

procedure TTreeCustomControl.SetLatestTime(value : double);
begin
  FLatestTime := value;
  if order(abs(TimeScale)) >= 1 then
      FTimeText := FloatToStrF(TimeScale+LatestTime,DecimalDisplayMode,15, 0)
  else
      FTimeText := FloatToStrF(TimeScale+LatestTime,DecimalDisplayMode, 15, Abs(order(TimeScale)));
end;

function TTreeCustomControl.NeedsPainting(aNode: TpNode): Boolean;
var
  aRect: TRect;
begin
  if FDrawFullTree or (NoOfOTUs < 500) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  //if not UpdatePaintingAreaCoords then
  //  Exit;
  aRect := FPaintingArea;

  Result := PointNearRect(aRect, aNode.position);
  if (not Result) and (aNode.anc <> nil) then
    Result := PointNearRect(aRect, aNode.anc.position);
  if (not Result) and (aNode.des1 <> nil) then
  begin
    Result := PointNearRect(aRect, aNode.des1.position);
    if (not Result) and (aNode.des1.des1 <> nil) then
      Result := PointNearRect(aRect, aNode.des1.des1.position);
    if (not Result) and (aNode.des1.des2 <> nil) then
      Result := PointNearRect(aRect, aNode.des1.des2.position);
  end;
  if (not Result) and (aNode.des2 <> nil) then
  begin
    Result := PointNearRect(aRect, aNode.des2.position);
    if (not Result) and (aNode.des2.des1 <> nil) then
      Result := PointNearRect(aRect, aNode.des2.des1.position);
    if (not Result) and (aNode.des2.des2 <> nil) then
      Result := PointNearRect(aRect, aNode.des2.des2.position);
  end;
  if (not Result) and (not aNode.OTU) then
    Result := HasBranchInClipRect(aNode);
end;

function TTreeCustomControl.NodeIsInViewableArea(aNodeIndex: Integer): Boolean;
var
  n: TpNode = nil;
begin
  if aNodeIndex < 1 then
    Exit(False);
  n := Node[aNodeIndex];
  Result := PtInRect(FPaintingArea, n.position);
end;

function TTreeCustomControl.GetTopMargin: Integer;
begin
  Result := ybase;
end;

procedure TTreeCustomControl.InitRenderers;
begin
  FBitmapRenderer := TBitmapTreeRenderer.Create(Canvas);
  FPdfRenderer := TPdfTreeRenderer.Create(Canvas);
  FSvgRenderer := TSvgTreeRenderer.Create(Canvas);
  FEmfRenderer := TEmfTreeRenderer.Create(Canvas);
  FRenderer := FBitmapRenderer;
end;

procedure TTreeCustomControl.InitEditBox(IsTime: Boolean = false);
var
  e: TEdit = nil;
begin
  if not Assigned(EditBox) then
  begin
    e := TEdit.Create(Self);
    e.Parent := Self;
    e.BorderStyle := bsSingle;
    e.Visible := false;
    e.OnKeyDown := EditBoxOnKeyDown;
    FOriginalEditText := '';
    e.AutoSelect := True;

    { visually indicate edit box with outline instead of highlight colors
      so user can see their selected text and the location of the I-beam }
    e.Color := clWhite;
    e.Font.Color := clBlack;

    e.MaxLength := 200; // We run into issues when the name is too long (crash error).  We should solve it when we do our hardening.
    EditBox := e;
  end
  else
    e := TEdit(EditBox);

  if (IsTime) then
  begin
    e.OnChange := OnUserEditsTime;
    e.OnEditingDone := EndUserEditsTime;
  end
  else
  begin
    e.OnChange := OnUserEditsOTUName;
    e.OnEditingDone := EndUserEditsOTUName;
  end;
end;

function TTreeCustomControl.GetShowImages: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if Assigned(AttribList) and (AttribList.Count > 0) then
    for i := 0 to AttribList.Count - 1 do
      if AttribList[i].ShowImage then
        Exit(True);

  if Assigned(GroupAttrib) and (GroupAttrib.Count > 0) then
    for i := 0 to GroupAttrib.Count - 1 do
      if GroupAttrib[i].ShowImage then
        Exit(True);
end;

function TTreeCustomControl.GetHasMarkers: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if NoOfNodes > 0 then
    for i := 1 to NoOfNodes do
      if Marker[i].Shape <> msNone then
        Exit(True);
end;

function TTreeCustomControl.GetHorzFactor: Double;
begin
  Result := Power(10, 2-Floor(Log10(FPixelsPerUnit)));
end;

function TTreeCustomControl.GetHorzRatio: Double;
begin
  if CompareValue(PixelsPerUnit, 0, FP_CUTOFF) <> 0 then
    Result := FTreeWidth/FPixelsPerUnit
  else
    Result := FTreeWidth;
end;

function TTreeCustomControl.GetIsStatsStdDev: Boolean;
begin
  Result := False;
  if Assigned(TreeList) and (TreeList.Count > 0) and (FTreeIndex > 0) then
    Result := TreeList[FTreeIndex - 1].IsStatsStdDev;
end;

function TTreeCustomControl.GetNumDupsBreakdown: String;
begin
  Result := EmptyStr;
  if (NumGeneDups > 0) and (NumSpeciations > 0) then
    Result := Format(' %.0n gene duplications and %.0n speciations were found in the tree.', [NumGeneDups*1.0, NumSpeciations*1.0])
  else if NumGeneDups > 0 then
    Result := Format('%.0n gene duplications were found in the tree.', [NumGeneDups*1.0])
  else if NumSpeciations > 0 then
    Result := Format('%.0n speciations were found in the tree.', [NumSpeciations*1.0]);
end;

function TTreeCustomControl.GetCoordsLong(index: Integer): AnsiString;
begin
  Result := EmptyStr;
  if index = 0 then
    Exit;
  if Node[index].des1.Otu then
    Result := Format('(%s . ', [OTUName[Node[index].des1.index]])
  else
    Result := Format('(%d . ', [Node[index].des1.index]);

  if Node[index].des2.Otu then
    Result := Result + Format('%s)', [OTUName[Node[index].des2.index]])
  else
    Result := Result + Format('%d)', [Node[index].des2.index]);
end;

function TTreeCustomControl.GetCompressedClusterSize(otu: TpNode): Integer;
var
  n: TpNode;
begin
  Result := otu.size;
  n := otu;
  while Assigned(n.anc) do
  begin
    if n.anc.compressed then
      n := n.anc
    else
    begin
      Result := n.size;
      Exit;
    end;
  end;
end;

function TTreeCustomControl.GetOtuNamesInCluster(const clusterRoot: Integer; var aList: TStringList): Integer;
var
  n: TpNode;
begin

  if clusterRoot < GetNoOfNodes then
  begin
    n := Node[clusterRoot];
    if n.OTU then
      aList.Add(n.name);
    if Assigned(n.des1) then
      GetOtuNamesInCluster(n.des1.index, aList);
    if Assigned(n.des2) then
      GetOtuNamesInCluster(n.des2.index, aList);
    Result := aList.Count;
  end
  else
    Result := 0;
end;

procedure TTreeCustomControl.GetFocusedTaxonNameAndGroup(var aTaxonName: String; var aGroupName: String);
var
  n: TpNode = nil;
begin
  if (FocusedIndex > 0) and (FocusedIndex <= FNoOfOTUs) then
  begin
    n := Node[FocusedIndex];
    aTaxonName := n.name;
    if n.groupindex >= 0 then
      aGroupName := GroupAttrib[n.groupIndex].Name
    else
      aGroupName := EmptyStr;
  end
  else if (FocusedIndex > 0) then
  begin
    n := Node[FocusedIndex];
    if n.name <> EmptyStr then
      aTaxonName := n.name
    else
      aTaxonName := Format('node-%d', [n.index]);
    if n.groupindex >= 0 then
      aGroupName := GroupAttrib[n.groupindex].Name
    else
      aGroupName := EmptyStr
  end
  else
  begin
    aTaxonName := EmptyStr;
    aGroupName := EmptyStr;
  end;
end;

function TTreeCustomControl.GetIsInternalOutgroup: Boolean;
var
  i: Integer;
begin
  Result := False;
  if FNoOfOTUs > 0 then
    for i := 1 to FNoOfOTUs do
      if Node[i].tempOutgroup then
      begin
        Result := True;
        Exit;
      end;
end;

procedure TTreeCustomControl.FreeRenderers;
begin
  if Assigned(FBitmapRenderer) then
    FBitmapRenderer.Free;
  if Assigned(FPdfRenderer) then
    FPdfRenderer.Free;
  if Assigned(FSvgRenderer) then
    FSvgRenderer.Free;
  if Assigned(FEmfRenderer) then
    FEmfRenderer.Free;
end;

function TTreeCustomControl.DumpNodeInfo: TStringList;
var
  aNode: TpNode;
  i: Integer;

  function DebugNodeInfo(n: TpNode): String;
  begin
    Result := IntToStr(n.index);
    if Assigned(n.anc) then
      Result := Result + Format(', anc: %d', [n.anc.index])
    else
      Result := Result + ', anc: nil';
    if Assigned(n.des1) then
      Result := Result + Format(', des1: %d', [n.des1.index])
    else
      Result := Result + ', des1: nil';
    if Assigned(n.des2) then
      Result := Result + Format(', des2: %d', [n.des2.index])
    else
      Result := Result + ', des2: nil';
  end;

begin
  Result := TStringList.Create;
  for i := 1 to NoOfNodes do
  begin
    aNode := Node[i];
    Result.Add(DebugNodeInfo(aNode));
  end;
end;

function TTreeCustomControl.HasCalibrations: Boolean;
begin
  Result := (GetNumCalibrations > 0);
end;

function TTreeCustomControl.GetNumCalibrations: Integer;
begin
  Result := Length(FCalibratedNodes);
end;

function TTreeCustomControl.GetLeftMargin: Integer;
begin
  Result := xbase;
end;

function TTreeCustomControl.HasBranchInClipRect(aNode: TpNode): Boolean;
begin
  Result := False;
  if aNode.OTU then
  begin
    Assert(False, 'do not call this for leaf nodes, it is wasteful');
  end
  else
  begin
    if (aNode.position.y >= FPaintingArea.Bottom) and ((aNode.des1.position.y <= FPaintingArea.Top) or (aNode.des2.position.y <= FPaintingArea.Top)) then
      Result := True
    else if (aNode.position.y <= FPaintingArea.Top) and ((aNode.des1.position.y >= FPaintingArea.Bottom) or (aNode.des2.position.y >= FPaintingArea.Bottom)) then
        Result := True
    else if aNode.anc > nil then
    begin
      if (aNode.anc.position.y >= FPaintingArea.Bottom) and (aNode.position.y <= FPaintingArea.Top) then
        Result := True
      else if (aNode.anc.position.y <= FPaintingArea.Top) and (aNode.position.y >= FPaintingArea.Bottom) then
          Result := True;
    end;
  end;
end;

procedure TTreeCustomControl.SetDataCoverageDecimals(AValue: Integer);
begin
  if FDataCoverageDecimals = AValue then Exit;
  FDataCoverageDecimals := AValue;
end;

procedure TTreeCustomControl.SetDivTimeDecimals(AValue: Integer);
begin
  if FDivTimeDecimals = AValue then Exit;
  FDivTimeDecimals := AValue;
end;

procedure TTreeCustomControl.SetDrawBoxForHighlightedNames(AValue: Boolean);
begin
  if FDrawBoxForHighlightedNames = AValue then Exit;
  FDrawBoxForHighlightedNames := AValue;
  Invalidate;
end;

procedure TTreeCustomControl.SetHideOverlappingDivTimes(AValue: Boolean);
begin
  if FHideOverlappingDivTimes=AValue then Exit;
  FHideOverlappingDivTimes:=AValue;
end;

procedure TTreeCustomControl.SetIsDoingCalibrations(AValue: Boolean);
begin
  if FIsDoingCalibrations=AValue then Exit;
  FIsDoingCalibrations:=AValue;
end;

procedure TTreeCustomControl.SetIsSamplingTimes(AValue: Boolean);
begin
  if FIsSamplingTimes=AValue then Exit;
  FIsSamplingTimes:=AValue;
end;

procedure TTreeCustomControl.SetIsUPGMA(AValue: Boolean);
begin
  if FIsUPGMA=AValue then Exit;
  FIsUPGMA:=AValue;
end;

procedure TTreeCustomControl.SetLatestSampleTime(AValue: Extended);
begin
  if FLatestSampleTime=AValue then Exit;
  FLatestSampleTime:=AValue;
end;

procedure TTreeCustomControl.SetMakeBranchesEqualLength(AValue: Boolean);
begin
  if FMakeBranchesEqualLength = AValue then Exit;
  FMakeBranchesEqualLength := AValue;
  Invalidate;
end;

procedure TTreeCustomControl.SetNodeLabelFont(AValue: TFont);
begin
  FNodeLabelFont := AValue;
end;

procedure TTreeCustomControl.SetShowDataCoverage(AValue: Boolean);
begin
  if FShowDataCoverage = AValue then Exit;
  FShowDataCoverage := AValue;
  if FShowDataCoverage then
  begin
    FShowStats := False;
    FShowStatsRange := False;
    FShowNodeIds := False;
  end;
end;

procedure TTreeCustomControl.SetShowDivergenceTimes(AValue: boolean);
begin
  if FShowDivergenceTimes = AValue then Exit;
  FShowDivergenceTimes := AValue;
end;

procedure TTreeCustomControl.SetShowImages(AValue: Boolean);
var
  i: Integer = -1;
begin
  if Assigned(AttribList) and (AttribList.Count > 0) then
    for i := 0 to AttribList.Count - 1 do
      if AttribList[i].IsImage then
        AttribList[i].ShowImage := AValue;

  if Assigned(GroupAttrib) and (GroupAttrib.Count > 0) then
    for i := 0 to GroupAttrib.Count - 1 do
      if GroupAttrib[i].IsImage then
        GroupAttrib[i].ShowImage := AValue;
end;

procedure TTreeCustomControl.SetShowNodeIds(AValue: Boolean);
begin
  if FShowNodeIds = AValue then Exit;
  FShowNodeIds := AValue;
  if FShowNodeIds then
  begin
    FShowDataCoverage := False;
    FShowStats := False;
    FShowStatsRange := False;
  end;
end;

procedure TTreeCustomControl.SetShowRoot(AValue: boolean);
begin
  if FShowRoot = AValue then Exit;
  FShowRoot := AValue;
end;

procedure TTreeCustomControl.SetShowStats(AValue: boolean);
begin
  if FShowStats = AValue then Exit;
  FShowStats := AValue;
  if FShowStats then
  begin
    FShowStatsRange := False;
    FShowDataCoverage := False;
    FShowNodeIds := False;
  end;
end;

procedure TTreeCustomControl.SetShowStatsRange(AValue: Boolean);
begin
  if FShowStatsRange = AValue then Exit;
  FShowStatsRange := AValue;
  if FShowStatsRange then
  begin
    FShowStats := False;
    FShowDataCoverage := False;
    FShowNodeIds := False;
  end;
end;

procedure TTreeCustomControl.SetTreeDrawingTarget(AValue: TTreeDrawingTarget);
begin
  if FTreeDrawingTarget=AValue then Exit;
  FTreeDrawingTarget:=AValue;
  case FTreeDrawingTarget of
    tdtCanvas: FRenderer := FBitmapRenderer;
    tdtPdf: FRenderer := FPdfRenderer;
    tdtSvg: FRenderer := FSvgRenderer;
    tdtEmf: FRenderer := FEmfRenderer;
    else
      raise Exception.Create('invalid tree drawing target');
  end;
end;

function TTreeCustomControl.UpdatePaintingAreaCoords: Boolean;
var
  R, B, C: TRect;
begin
  Result := False;
  R := Canvas.ClipRect;
  C := Rect(0, 0, 0, 0);
  if IsRectEmpty(R) then exit;
  OffsetRect(R, ScrollLeft, ScrollTop);

  // Boundsrect of metafile
  B := Rect(0, 0, ScrollWidth, ScrollHeight);

  // Find clipped area
  IntersectRect(C, R, B);
  if IsRectEmpty(C) then exit;

  // And the area in the control
  //R := C;
  //OffsetRect(R, -ScrollLeft, -ScrollTop);
  FPaintingArea := C;
  Result := True;
end;

function TTreeCustomControl.PointNearRect(aRect: TRect; aPoint: TPoint): Boolean;
const
  DIST_TO_TEST = 200;
var
  ExpandedRect: TRect;
begin
  ExpandedRect := Rect(max(0, aRect.Left - DIST_TO_TEST), max(0, aRect.Top - DIST_TO_TEST), aRect.Right + DIST_TO_TEST, aRect.Bottom + DIST_TO_TEST);
  Result := PtInRect(ExpandedRect, aPoint);
end;

procedure TTreeCustomControl.DoSetDivergenceTime(value : double);
begin
    if not TreeExist then Exit;
    if not IsBranchLength then exit;
    if not NodeFocused then Exit;
    if value <= 0.0 then Exit;
    TimeFactor := value/Node[FocusedIndex].height;
end;


function TTreeCustomControl.GetDivergenceTime:double;
begin
    //if (not NodeFocused) or ((not IsSamplingTimes) and Node[FocusedIndex].Otu) then
    //    Result := 0.0
    //else
    //    Result := Node[FocusedIndex].height*TimeFactor +LatestTime;
  if not NodeFocused then
    Result := 0.0
  else
    Result := GetDivergenceTime(FocusedIndex);
end;

function TTreeCustomControl.GetDivergenceTime(NodeId: Integer): double;
begin
  Result := 0.0;
  if Assigned(FDivTimeFunc) then
    Result := FDivTimeFunc(NodeId);
end;

procedure TTreeCustomControl.GetEarliestAndLatestRtdtTimes(var earliestDate: Double; var latestDate: Double);
var
  i: Integer;
  aTime: Double = 0;
begin
  earliestDate := MaxDouble;
  latestDate := 0;
  if NoOfOTUs > 0 then
    for i := 1 to NoOfNodes do
    begin
      aTime := GetDivergenceTime(Node[i].index);
      if CompareValue(aTime, earliestDate, FP_CUTOFF) < 0 then
        earliestDate := aTime;
      if CompareValue(aTime, latestDate, FP_CUTOFF) > 0 then
        latestDate := aTime;
    end;
end;

function TTreeCustomControl.GetFormattedDivergenceTimeString(convertToDate: Boolean): String;
var
  temp: Extended;
begin
  try
    temp := GetDivergenceTime;
    if convertToDate then
      Result := RtdtTimeToDateString(temp)
    else
      Result := Trim(FormatDoubleSafe(GetDivergenceTime, DivTimeDecimals, 15));
  except
    on E:Exception do
      Result := '?';;
  end;
end;

function TTreeCustomControl.GetFormattedDivergenceTimeString(aTime: Extended; convertToDate: Boolean): String;
begin
  try
    if convertToDate then
      Result := RtdtTimeToDateString(aTime)
    else
      Result := Trim(FormatDoubleSafe(aTime, DivTimeDecimals, 15));
  except
    on E:Exception do
    begin
      ShowMessage('Application Error: ' + E.Message);
      Result := '?'
    end;
  end;
end;

function TTreeCustomControl.RtdtTimeToDateString(rtdtTime: Extended): String;
begin
  if CompareValue(rtdtTime, 0.0, FP_CUTOFF) >= 0 then
    Result := FormatDateTime('mmm dd, yyyy', FloatTimeToDateTime(rtdtTime))
  else
    Result := IntToStr(trunc(rtdtTime));
end;

function TTreeCustomControl.GetDaysElapsed: Int64;
begin
  Result := RtdtDaysElapsed(FLatestSampleTime, GetDivergenceTime);
end;

function TTreeCustomControl.GetMinDivergenceTime: Extended;
begin
  Result := 0.0;
  if NodeFocused and assigned(FMinTimeFunc) then
    Result := FMinTimeFunc(FocusedIndex);
end;

function TTreeCustomControl.GetMaxDivergenceTime: Extended;
begin
  Result := 0.0;
  if NodeFocused and assigned(FMaxTimeFunc) then
    Result := FMaxTimeFunc(FocusedIndex);
end;

procedure TTreeCustomControl.SetCondenseValue(value : integer);
begin
    if value = CondenseValue then Exit;
    FCondenseValue := value;
end;

function TTreeCustomControl.GetOTUName(i : integer):AnsiString;
begin
    if i = 0 then
        if FocusedIndex = 0 then
            Result := ''
        else
            Result := Node[FocusedIndex].name
    else
        Result := Node[i].name;
end;

procedure TTreeCustomControl.SetOTUName(index: integer; name: AnsiString);
begin
  Node[index].name := name;
  Node[index].width := StrLength(name, OTU_Font);
end;

procedure TTreeCustomControl.SetBranchLength(index: integer; branchLength: Double);
begin
  Node[index].branch.length := branchLength;
  SetMaxLength(Root);
end;

function TTreeCustomControl.GetCoords(i : integer):AnsiString;
begin
    if (i <> 0) and (Node[i].des1 <> nil) and (Node[i].des2 <> nil) then
     Result := '(' + IntToStr(Node[i].des1.index) + ' . ' + IntToStr(Node[i].des2.index) + ')';
end;

function TTreeCustomControl.GetOTUOrigionalName(i: integer): AnsiString;
begin
    if i = 0 then
        if FocusedIndex = 0 then
            Result := ''
        else
            Result := Node[FocusedIndex].oriName
    else
        Result := Node[i].oriName;
end;

function TTreeCustomControl.GetIndexOfName(Name: AnsiString): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=1 to NoOfOTUs do
  begin
      if AnsiCompareStr(Name, GetOTUName(i)) = 0 then
    begin
      result := i;
      break;
    end;
  end;
end;

function TTreeCustomControl.GetIndexOfOrigionalName(OrigName: AnsiString): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=1 to NoOfOTUs do
  begin
      if AnsiCompareStr(OrigName, GetOTUOrigionalName(i)) = 0 then
    begin
      result := i;
      break;
    end;
  end;
end;

function TTreeCustomControl.GetClusterName(i : integer):AnsiString;
var
  p: TpNode;
begin
  Result := '';
  if i = 0 then
    if FocusedIndex = 0 then
    begin
      Result := '';
      exit;
    end
    else
      i := FocusedIndex;

  if (i > NoOfOTUs) and (Node[i].anc <> nil) then
  begin
    if (Node[i].attrindex <> Node[i].anc.attrindex) and (Node[i].attrindex > 0) then
      Result := AttribList[Node[i].attrindex].Caption;
    exit;
  end;
  p := Node[i].anc;
  while p <> nil do
  begin
    if p.attrindex > 0 then
    begin
      result := AttribList[p.attrindex].Caption;
      break;
    end;
    p := p.anc;
  end;
end;

function TTreeCustomControl.GetGroupName(i : integer):AnsiString;
begin
  result := '';
  if not TreeExist then Exit;
  if GroupAttrib.Count = 0 then Exit;
  if (i > 0) and (i <= NoOfOTUs) then
    if Node[i].groupindex >= 0 then
      Result := GroupAttrib[Node[i].groupindex].Name;
end;



procedure TTreeCustomControl.SetClusterName(index: integer; name:AnsiString);
begin
  if not TreeExist then Exit;
  if (index > NoOfOTUs) and (index <= NoOfNodes) then
    Node[index].name := name;
end;

function TTreeCustomControl.GetNameFocused:boolean;
begin
  Result := (FocusedNameIndex > 0);
end;


procedure TTreeCustomControl.SetStats;
var i : integer;
begin
  Assert(not (FTreeBoxType = tttReltimeTree), 'error - should not set timetree to show node stats');
  if MaxStats = 0.0 then
  begin
    for i := 1 to NoOfNodes-1 do
      if Node[i].branch.stats < -0.0000000000001 then
        Node[i].branch.stat2 := -1
      else if (isRooted and ShowRoot) or (Node[i] <> Root) then
        Node[i].branch.stat2 := Trunc(Node[i].branch.stats+0.000000000001);
  end
  else
    for i := 1 to NoOfNodes-1 do
      if (isRooted and ShowRoot) or (Node[i] <> Root) then
        if Node[i].branch.stats < -0.0000000000001 then
          Node[i].branch.stat2 := -1
        else
          Node[i].branch.stat2 := Trunc(Node[i].branch.stats*100/MaxStats+0.000000000001);
end;

procedure TTreeCustomControl.FlipCluster;
begin
    if (not (NodeFocused or BranchFocused)) or (Node[FocusedIndex].OTU) then Exit;
    SwapNode(Node[FocusedIndex].des1, Node[FocusedIndex].des2);
    Refresh;
end;

procedure TurnNode(p : TpNode);
begin
    if not p.OTU then begin
        SwapNode(p.des1,p.des2);
        TurnNode(p.des1);
        TurnNode(p.des2);
    end;
end;

procedure TTreeCustomControl.FlipAllCluster;
begin
    if (not (NodeFocused or BranchFocused)) or (FocusedIndex <= NoOfOTUs) then Exit;
    TurnNode (Node[FocusedIndex]);
    Refresh;
end;

procedure TTreeCustomControl.MoveRoot(p: TpNode; midpoint: boolean);
begin
  ChangeRoot(Root, p, midpoint);
end;

procedure TTreeCustomControl.UpdateBrush(aBrush: TBrush; aStyle: TFPBrushStyle; aColor: TColor);
begin
  aBrush.Style := aStyle;
  aBrush.Color := aColor;
end;

procedure TTreeCustomControl.UpdatePen(aPen: TPen; PenStyle: TFpPenStyle;
  PenWidth: Integer; aColor: TColor; aEndCap: TPenEndCap;
  aJoinStyle: TPenJoinStyle);
begin
  aPen.Style := PenStyle;
  aPen.Width := PenWidth;
  aPen.Color := aColor;
  aPen.EndCap := aEndCap;
  aPen.JoinStyle := aJoinStyle;
end;

procedure TTreeCustomControl.UpdateOtuFont(a: TNodeAttrib);
begin
  if FOTU_Font.Size <> a.Font.Size then
    FOTU_Font.Size := a.Font.Size;
  if FOTU_Font.Style <> a.Font.Style then
    FOTU_Font.Style := a.Font.Style;
  if FOTU_Font.Name <> a.Font.Name then
    FOTU_Font.Name := a.Font.Name;
  if FOTU_Font.Color <> a.Font.Color then
    FOTU_Font.Color := a.Font.Color;
end;

procedure TTreeCustomControl.UpdatePrivateFont(a: TNodeAttrib);
begin
  if FPrivateFont.Size <> a.Font.Size then
    FPrivateFont.Size := a.Font.Size;
  if FPrivateFont.Style <> (a.Font.Style + [fsBold]) then
    FPrivateFont.Style := a.Font.Style + [fsBold];
  if FPrivateFont.Name <> a.Font.Name then
    FPrivateFont.Name := a.Font.Name;
  if FPrivateFont.Color <> a.Font.Color then
    FPrivateFont.Color := a.Font.Color;
end;

procedure TTreeCustomControl.UpdateCaptionFont(a: TNodeAttrib);
begin
  if FCaptionFont.Size <> a.CaptionFont.Size then
    FCaptionFont.Size := a.CaptionFont.Size;
  if FCaptionFont.Style <> a.CaptionFont.Style then
    FCaptionFont.Style := a.CaptionFont.Style;
  if FCaptionFont.Name <> a.CaptionFont.Name then
    FCaptionFont.Name := a.CaptionFont.Name;
  if FCaptionFont.Color <> a.Font.Color then
    FCaptionFont.Color := a.Font.Color;
end;


function TTreeCustomControl.NodeCoords(NodeIndex: Integer): TRect;
var
  aLeft: Integer = -1;
  aTop: Integer = -1;
  n: TpNode = nil;
begin
  Result := Rect(0, 0, 0, 0);
  if (NodeIndex < 1) or (NodeIndex > NoOfNodes) then
    Exit;
  n := Node[NodeIndex];
  if (not n.OTU) and n.compressed then
    aLeft := ((n.des1.position.x + xbase)) + Canvas.TextWidth(' ')
  else
    aLeft := ((n.position.x + xbase)) + Canvas.TextWidth(' ');
  aLeft := aLeft + GetNodeAttrib(n).LineWidth div 2;
  if GetTaxonMarker(n).Shape <> msNone then
      aLeft := aLeft + Abs(GetNodeAttrib(n).Font.Height*5);
  Canvas.Font.Assign(CharStateFont);
  if ShowCharState then
      aLeft := aLeft + Canvas.TextWidth(n.charstate) + Abs(CharStateFont.Height) div 2;
  if (not n.OTU) and n.compressed then
    aTop := ((n.des1.position.y + n.des2.position.y +2*ybase) div 2) - Abs(FOTU_Font.Height div 2)
  else
    aTop := ((n.position.y +ybase)) - Abs(FOTU_Font.Height div 2);
  Result.Left := aLeft;
  Result.Right := Result.Left;
  Result.Top := aTop;
  Result.Bottom := Result.Top;
end;

procedure TTreeCustomControl.SetShowGroupNames(aValue: Boolean);
var
  i: Integer = -1;
begin
  if Assigned(GroupAttrib) and (GroupAttrib.Count > 0) then
  begin
    for i := 0 to GroupAttrib.Count - 1 do
    begin
      GroupAttrib[i].ShowCaption := aValue;
      GroupAttrib[i].ShowBracket := aValue;
    end;
    Invalidate;
  end;
end;

function TTreeCustomControl.SaveSessionProperties(filename: String): Boolean;
var
  json: TJsonObject = nil;
  str: String = '';
  aFile: TextFile;
begin
  try
    try
      TreeBoxSessionPropsCI.Acquire;
      AssignFile(aFile, filename);
      Rewrite(aFile);
      json := TJsonObject.Create;
      GetSessionProperties(json);
      str := json.AsJSON;
      WriteLn(aFile, str);
    except
      on E:Exception do
      begin
        {$IFDEF DEBUG}
        ShowMessage('Exception when saving TTreeBoxSessionProperties - ' + E.Message + ' ' + filename);
        {$ENDIF}
      end;
    end;
  finally
    CloseFile(aFile);
    TreeBoxSessionPropsCI.Release;
    if Assigned(json) then
      json.Free;
  end;
end;

function TTreeCustomControl.LoadSessionProperties(filename: String): Boolean;
var
  json: TJsonObject = nil;
  jparser: TJSONParser  = nil;
  aData: TJSONData = nil;
  aList: TStringList = nil;
begin
  Result := False;
  if not FileExists(filename) then
    Exit;

  try
    try
      TreeBoxSessionPropsCI.Acquire;
      aList := TStringList.Create;
      aList.LoadFromFile(filename);
      if Trim(aList.Text) <> EmptyStr then
      begin
        jparser := TJsonParser.Create(aList.Text, []);
        aData := jparser.Parse;
        if not Assigned(aData) then
          raise Exception.Create('invalid json data');
        json := TJsonObject(aData);
        SetSessionProperties(json);
      end;
    except
      on E:Exception do
      begin
        {$IFDEF DEBUG}
        ShowMessage('exception when loading tree box session properties: ' + E.Message + ' ' + filename);
        {$ENDIF}
      end;
    end;
  finally
    TreeBoxSessionPropsCI.Release;
    if Assigned(aList) then
      aList.Free;
    if Assigned(jparser) then
      jparser.Free;
  end;
end;

procedure TTreeCustomControl.MakeBranchFromRoot;
begin
if (Root.position.x <> Root.des1.position.x) and
   (Root.position.x <> Root.des2.position.x) then
  exit;

  if BranchFocused or NodeFocused or TimeFocused then
    ClearFocus;

  if Root.position.x = Root.des1.position.x then
    FFocusedIndex := Root.des2.index
  else
    FFocusedIndex := Root.des1.index;
  FBranchFocused := true;

  MarkedPos.X := (Root.position.x*9 +Node[FFocusedIndex].position.x) div 10;
  MarkedPos.Y := (Root.position.y*9 +Node[FFocusedIndex].position.y) div 10;

  MakeRootOnBranch;

  ClearFocus;
end;

procedure TTreeCustomControl.MakeRootOnBranch;
var dLen, x0, x1, y0, y1 : double;
begin
    if FocusedIndex = 0 then Exit;
    if FocusedIndex = Root.index then Exit;

    if Node[FocusedIndex].Anc = Root then
    begin
      x0 := Root.position.x;
      y0 := Root.position.y;
      x1 := Node[FocusedIndex].position.x;
      y1 := Node[FocusedIndex].position.y;
      if TreeStyle = tsRadiation then
        if (x1 = x0) and (y1 = y0) then
          Exit
        else begin
          dLen := Node[FocusedIndex].branch.length
                 *sqrt((MarkedPos.x -x0)*(MarkedPos.x -x0)+(MarkedPos.y -y0)*(MarkedPos.y -y0))
                 /sqrt((x1 -x0)*(x1 -x0)+(y1 -y0)*(y1 -y0));
          if dLen > Node[FocusedIndex].branch.length then
            dLen := Node[FocusedIndex].branch.length;
        end
      else
        dLen := Node[FocusedIndex].branch.length*(MarkedPos.x -x0)/(x1 -x0);

      if Node[FocusedIndex] = Root.des1 then begin
        Root.des2.branch.length := Root.des2.branch.length +dLen;
        Root.des2.branch.maxlen2 := Root.des2.branch.maxlen2 +dLen;
      end
      else if Node[FocusedIndex] = Root.des2 then begin
        Root.des1.branch.length := Root.des1.branch.length +dLen;
        Root.des1.branch.maxlen2 := Root.des1.branch.maxlen2 +dLen;
      end;
      Node[FocusedIndex].branch.length := Node[FocusedIndex].branch.length -dLen;
      Node[FocusedIndex].branch.maxlen2 := Node[FocusedIndex].branch.maxlen2 -dLen;
    end
    else
    begin
      MoveRoot(Node[FocusedIndex], false);
      SetClusterSize(Root);
      SetClusterWidth(Root);
    end;
    SetClusterHeight;
    SetAttrindex;

    FocusOnNode(Root.Index);
    if Root.des1.Size >= Root.des2.Size then
      MarkTempOutgroup(Root.des2, Root.des1)
    else
      MarkTempOutgroup(Root.des1, Root.des2);
end;

function TTreeCustomControl.SearchMidPoint:integer;
var i : integer;
    r : double;
begin
    Result := Root.des1.index;
    for i := 1 to NoOfNodes do begin
        if Node[i] = Root then Continue;
        with Node[i].branch do begin
            r := length -Abs(maxlen1 - maxlen2);
            if r >= 0.0 then
                Result := i;
        end;
    end;
end;

procedure TTreeCustomControl.MakeRootOnMidPoint;
var bnum : integer;
begin
    if isRooted then Exit;
    if not isBranchLength then Exit;
    ClearFocus;
    bnum := SearchMidPoint;
    MoveRoot(Node[bnum],true);
    SetClusterSize(Root);
    SetClusterWidth(Root);
    SetClusterHeight;
    SetAttrindex;
end;

procedure TTreeCustomControl.AddOutgroup(index : integer);
begin
    if (index > 0) and (index <= NoOfOTUs) then
        Node[index].outgroup := true;
end;

procedure TTreeCustomControl.RemoveDivergenceTimeCalibration;
begin
  ShowDivergenceTimes := False;
  ShowTimeScale := False;
end;

procedure TTreeCustomControl.RemoveOutgroup(index : integer);
begin
  if (index > 0) and (index <= NoOfOTUs) then
    Node[index].outgroup := false;
end;

procedure TTreeCustomControl.SetOutgroup(index : integer; value : boolean);
begin
  if not TreeExist then Exit;
  if (index > 0) and (index <= NoOfOTUs) then
    Node[index].outgroup := value;
end;

function TTreeCustomControl.GetIsOutgroup:boolean;
var i : integer;
begin
    Result := false;
    for i := 1 to NoOfOTUs do
        if Node[i].outgroup then begin
            Result := true;
            Break;
        end;
end;

function TTreeCustomControl.GetOutgroup(index : integer):boolean;
begin
    if (index < 1) or (index > NoOfOTUs) then
        Result := false
    else
        Result := Node[index].outgroup;
end;

function TTreeCustomControl.GetOutgroupAncestorIndex: Integer;
var
  Mrca: TpNode;
  i: Integer;
begin
  Result := Root.index;
    if not isOutgroup then
      Exit;
  for i := 1 to NoOfOTUs do
      Node[i].flag := Node[i].outgroup;
  Mrca := SearchCommonAncestor(Root);
  if Assigned(Mrca) then
    Result := Mrca.index;
end;

procedure TTreeCustomControl.MarkOutgroupNodes;
var
  Mrca : TpNode;
  i : integer;

  procedure ProcessNode(ANode: TpNode);
  begin
    ANode.Outgroup := True;
    if Assigned(ANode.des1) then
      ProcessNode(ANode.des1);
    if Assigned(ANode.des2) then
      ProcessNode(ANode.des2);
  end;

begin
  for i := 1 to NoOfOTUs do
      Node[i].flag := Node[i].outgroup;
  Mrca := SearchCommonAncestor(Root);
  ProcessNode(Mrca);
end;

function TTreeCustomControl.MarkInternalOutgroupNodes(outgroupNodes: TStringList): TpNode;
var
  i: Integer;

  procedure DoMarkNodes(n: TpNode);
  begin
    n.tempOutgroup := True;
    if Assigned(n.des1) then
      DoMarkNodes(n.des1);
    if Assigned(n.des2) then
      DoMarkNodes(n.des2);
  end;

begin
  Result := nil;
  if FNoOfOTUs > 0 then
  begin
    for i := 1 to FNoOfOTUs do
    begin
      if outgroupNodes.IndexOf(Node[i].name) >= 0 then
        Node[i].flag := True
      else
        Node[i].flag := False;
      Node[i].tempOutgroup := False;
    end;
    for i := FNoOfOTUs + 1 to NoOfNodes do
    begin
      Node[i].flag := False;
      Node[i].tempOutgroup := False
    end;
  end;

  Result := FindCommonAncestorOfFlaggedTaxa(Root);
  if Assigned(Result) then
    DoMarkNodes(Result);
end;

function TTreeCustomControl.GetFirstOutgroupMemberIndex: Integer;
var
  i: Integer;
begin
  Result := Node[1].index;
  if not isOutgroup then
    Exit;
  for i := 1 to NoOfOTUs do
      if Node[i].outgroup then
      begin
        Result := Node[i].index;
        break;
      end;
end;

function TTreeCustomControl.GetCurrentOutgroup(index : integer):boolean;
var p, a : TpNode;
begin
    p := Node[index];
    a := p.anc;
    while a <> Root do begin
        p := a;
        a := p.anc;
    end;
    if p = a.des2 then
        Result := true
    else
        Result := false;
end;

procedure TTreeCustomControl.MakeRootByOutgroup(OutgroupOnBottom: Boolean=False);
var
  newroot : TpNode;
  i : integer;
begin
  if isRooted then
    Exit;
  if not isOutgroup then
    Exit;
  for i := 1 to NoOfOTUs do
      Node[i].flag := Node[i].outgroup;
  newroot := SearchCommonAncestor(Root);
  if newroot = Root then Exit;
  MoveRoot(newroot, true);
  SetClusterSize(Root);
  SetClusterWidth(Root);
  if Root.des1.flag then
      TurnNode(Root);
  if OutgroupOnBottom then
    PutOutgroupOnBottom;
  SetClusterHeight;
end;

procedure TTreeCustomControl.MakeRootByInternalOutgroup(outgroupNodes: TStringList; OutgroupOnBottom: Boolean);
var
  newroot : TpNode;
begin
  if isRooted then
    Exit;
  if not IsInternalOutgroup then
    Exit;
  newroot := MarkInternalOutgroupNodes(outgroupNodes);
  if newroot = Root then Exit;
  MoveRoot(newroot, False);
  SetClusterSize(Root);
  SetClusterWidth(Root);
  if Root.des1.tempOutgroup then
      TurnNode(Root);
  if OutgroupOnBottom then
    PutOutgroupOnBottom;
  SetClusterHeight;
end;

procedure TTreeCustomControl.PutOutgroupOnBottom;

  function ProcessNode(ANode: TpNode): Boolean;
  begin
    Result := ANode.tempOutgroup;
    if not Result then
    begin
      if ANode.Otu then
      begin
        Result := ANode.outgroup;
      end
      else
      begin
        Result := ProcessNode(ANode.des1);
        if not Result then
          Result := ProcessNode(ANode.des2);
      end;
    end;
  end;

  function OutgroupIsDes1: Boolean;
  begin
    Result := ProcessNode(Root.des1);
  end;

  function OutgroupIsDes2: Boolean;
  begin
    Result := ProcessNode(Root.des2);
  end;

begin
  if OutgroupIsDes1 then
    TurnNode(Root);
end;

procedure TTreeCustomControl.InitScale;
var
  r: double;
begin
    Scale := 2*Root.height/10;

{
    Scale := 0.0;
    for i := 1 to NoOfNodes do begin
        if Node[i] = Root then Continue;
        with Node[i].branch do begin
            r := maxlen1 + maxlen2 -length;
            if r > Scale then Scale := r;
        end;
    end;
}
    if Scale = 0.0 then
      Exit;

    r := Scale/Power(10,Floor(log10(Scale)));
    if Trunc(r+0.5) >= 8 then
        Scale := Power(10,Floor(log10(Scale))+1)
    else if Trunc(r) >= 3 then
        Scale := 5*Power(10,Floor(log10(Scale)))
    else if (Trunc(r) = 2) or (Trunc(r+0.5) = 2) then
        Scale := 2*Power(10,Floor(log10(Scale)))
    else
        Scale := Power(10,Floor(log10(Scale)));

    FScaleTick := Scale;
    FTimeTick := 0.0;
    FTimeFactor := 0.0;
    FTimeText := '';
    TimeScale := 0.0;

    if order(Scale) >= 1 then
        ScaleText := FloatToStrF(Scale,DecimalDisplayMode,15, 0)
    else
        ScaleText := FloatToStrF(Scale,DecimalDisplayMode, 15, Abs(order(Scale)));
end;

procedure TTreeCustomControl.GetTreeData(tree: TTreeData; UseTimeFactor: Boolean=False);
var i: integer;
begin
    tree.NoOfOTUs := NoOfOTUs;
    tree.isBLen := isBranchLength;
    if IsLinearized then
      tree.isSE := false
    else
      tree.isSE := isSE;
    tree.isStats := isStats;
    for i := 0 to NoOfOTUs-2 do
    begin
      tree.NodeArray[i].des1 := Node[NoOfOTUs+i+1].des1.index-1;
      tree.NodeArray[i].des2 := Node[NoOfOTUs+i+1].des2.index-1;
      tree.DataCoverage[i] := Node[NoOfOTUs+i+1].dataCoverage;
    end;
    for i := 1 to NoOfOtus do
      tree.IsOutgroupMember[i-1] := Node[i].outgroup;

    for i := 0 to NoOfNodes-2 do
    begin
      if tree.isBLen then
        if IsLinearized then
        begin
          if Node[i+1] <> Root then
            tree.BLenArray[i] := Node[Node[i+1].anc.index].height-Node[i+1].height;
        end
        else
          tree.BLenArray[i] := Node[i+1].branch.length;
        if UseTimeFactor then
          tree.BLenArray[i] := tree.BLenArray[i]*FTimeFactor;
      if tree.isSE then
        tree.SE[i] := Node[i+1].branch.SE;
      if tree.isStats then
        tree.Stats[i] := Node[i+1].branch.stats;
    end;
end;

procedure TTreeCustomControl.GetTreeDataNoOutgroup(var tree: TTreeData; var namesList: TStringList; UseTimeFactor: Boolean);
var
  i: integer;
begin
  namesList.Clear;
  for i := 1 to NoOfOtus do
    if not Node[i].outgroup then
      namesList.Add(Node[i].Name);
  tree.NoOfOTUs := NoOfOTUs;
  tree.isBLen := isBranchLength;
  if IsLinearized then
    tree.isSE := false
  else
    tree.isSE := isSE;
  tree.isStats := isStats;
  for i := 0 to NoOfOTUs-2 do
  begin
    tree.NodeArray[i].des1 := Node[NoOfOTUs+i+1].des1.index-1;
    tree.NodeArray[i].des2 := Node[NoOfOTUs+i+1].des2.index-1;
    tree.DataCoverage[i] := Node[NoOfOTUs+i+1].dataCoverage;
  end;
  for i := 1 to NoOfOtus do
    tree.IsOutgroupMember[i-1] := Node[i].outgroup;

  for i := 0 to NoOfNodes-2 do
  begin
    if tree.isBLen then
      if IsLinearized then
      begin
        if Node[i+1] <> Root then
          tree.BLenArray[i] := Node[Node[i+1].anc.index].height-Node[i+1].height;
      end
      else
        tree.BLenArray[i] := Node[i+1].branch.length;
      if UseTimeFactor then
        tree.BLenArray[i] := tree.BLenArray[i]*FTimeFactor;
    if tree.isSE then
      tree.SE[i] := Node[i+1].branch.SE;
    if tree.isStats then
      tree.Stats[i] := Node[i+1].branch.stats;
  end;
  tree.RemoveOutgroupMembers;
end;

function TTreeCustomControl.GetNodeAttrib(p: TpNode): TNodeAttrib;
begin
  if IsAutoCompressedClusters and (p.compressedIndex >= 0) then
    Result := CompressedNodeAttributes[p.compressedIndex]
  else if p.attrindex < 0 then
    result := GroupAttrib[p.groupindex]
  else
    result := AttribList[p.attrindex];
end;

function TTreeCustomControl.GetTaxonMarker(p: TpNode): TNodeMarker;
var
  q: TpNode;
begin
  result.Shape := msNone;
  result.Color := clBlack;
  if (p.index > NoOfOTUs) and not p.compressed then { an uncompressed internal node}
    exit;
  if p.marker.Shape = msNone then
    exit;

  if UseGroupAttrib then
    if p.attrindex < 0 then { this node uses group attributes instead of its node attributes}
      if GroupAttrib[p.groupindex].ShowTaxonMarker and EquivalentMarkers(p.marker, GroupAttrib[p.groupindex].Marker) then
      begin { this group shows markers and the node does not have a marker that overrides the group marker}
        result := p.marker;
        exit;
      end;

  q := p;
  while (q <> Root) and (q.attrindex < 0) do
    q := q.anc;
  if q.attrindex >= 0 then
    if AttribList[q.attrindex].ShowTaxonMarker then
    begin
      result := p.marker;
      exit;
    end;
end;

function TTreeCustomControl.GetMarkerWidth(p: TpNode): Integer;
var
  q: TpNode = nil;
  aMarker: TNodeMarker;
  size: Integer = 0;
  markerFound: Boolean = False;
begin
  Result := 0;
  if (p.index > NoOfOTUs) and not p.compressed then
    exit;
  if p.marker.Shape = msNone then
    Exit;

  if UseGroupAttrib then
  begin
    if p.attrindex < 0 then
      if GroupAttrib[p.groupindex].ShowTaxonMarker and EquivalentMarkers(p.marker, GroupAttrib[p.groupindex].Marker) then
      begin
        aMarker := p.marker;
        markerFound := True;
      end;
  end;

  if not markerFound then
  begin
    q := p;
    while (q <> Root) and (q.attrindex < 0) do
      q := q.anc;
    if q.attrindex >= 0 then
      if AttribList[q.attrindex].ShowTaxonMarker then
      begin
        aMarker := p.marker;
        markerFound := True;
      end;
  end;

  if markerFound then
  begin
    if p.OTU and (not p.compressed) then
      size := Abs(GetNodeAttrib(p).Font.Height)
    else
      size := Abs(GetNodeAttrib(p).CaptionFont.Height);

    case aMarker.Shape of
      msOpenCircle, msFilledCircle: Result := size div 4;
      msOpenSquare, msFilledSquare: Result := size div 2;
      msOpenDiamond, msFilledDiamond: Result := Round(size/Sqrt(6));
      msOpenUpTriangle, msFilledUpTriangle: Result := size div 2;
      msOpenDownTriangle, msFilledDownTriangle: Result := size div 2;
    end;
  end;
end;

function TTreeCustomControl.EquivalentMarkers(m1, m2: TNodeMarker): boolean;
begin
  result := (m1.Shape = m2.Shape) and (m1.Color = m2.Color);
end;

function TTreeCustomControl.GetTaxonName(Taxon: Integer): String;
begin
  Result := EmptyStr;
  if Taxon <= FNoOfOtus then
    Result := Node[Taxon].name;
end;

function TTreeCustomControl.GetLineWidth(p: TpNode): integer;
var
  q: TpNode;
  a: TNodeAttrib;
begin
  if p = Root then
    result := AttribList[0].LineWidth
  else
  begin
    a := GetNodeAttrib(p);

    if (a.BranchOption = boFullBranch) or
       (a.BranchOption = boHalfBranch) or
       ((a.BranchOption = boBranchOnly) and (p.attrindex <> p.anc.attrindex)) then
      result := a.LineWidth
    else
    begin
      q := p.anc;
      while (q <> Root) do
      begin
        a := GetNodeAttrib(q);
        if not ((a.BranchOption = boBranchOnly) or q.hidden) then
          break;
        q := q.anc;
      end;
      result := a.LineWidth;
    end;
  end;
end;



procedure TTreeCustomControl.SetPosition;
var yy, n : integer;
    q1,q2 : TpNode;

  procedure SetXEqualLengthBranches(p: TpNode);
  var
    aLen: Integer = -1;
  begin
    aLen := Round(Screen.PixelsPerInch/3);
    with p^ do
    begin
        if p = Root then
          position.x := 0
        else
          position.x := anc.position.x + aLen;

        if (anc <> nil) and (position.x < anc.position.x) then
            position.x := anc.position.x;
        if (not OTU) and compressed then
        begin
          des1.position.x := position.x;
          des2.position.x := position.x;
        end
        else
        begin
            if p <> Root then
              if position.x = anc.position.x then
                hidden := true;
            if Assigned(p.des1) then
              SetXEqualLengthBranches(p.des1);
            if Assigned(p.des2) then
              SetXEqualLengthBranches(p.des2);
        end;
        if CompareValue(position.X, FMaxXPosition, FP_CUTOFF) > 0 then
          FMaxXPosition := position.X;
    end;
  end; { SetXEqualLengthBranches}

    procedure SetX(p : TpNode);
    var
      maxDist: Double;
    begin
      if FMakeBranchesEqualLength then
      begin
        FMaxXPosition := 0;
        SetXEqualLengthBranches(p);
        Exit;
      end;

        with p^ do
        begin
            if p = Root then
                position.x := 0
            else if Topoflag then
                if OTU or compressed then
                    position.x := Round(MaxDepth*xunit)
                else
                    position.x := Round((MaxDepth -depth)*xunit)
            else if IsLinearized or ForceLinearized then
                position.x := Round((xmax -height)*xunit)
            else
                position.x := anc.position.x + Round(branch.length*xunit);

            if (anc <> nil) and (position.x < anc.position.x) then
                position.x := anc.position.x;
            if OTU or compressed then
            begin
              if (not OTU) and (not Topoflag) then
                if IsLinearized or ForceLinearized then begin
                  des1.position.x := Round(xmax*xunit);
                  des2.position.x := des1.position.x;
                end
                else
                begin
                  maxDist := max(height - des1.height, height - des2.height);
                  des1.position.x := position.x + Round(maxDist*xunit);
                  des2.position.x := des1.position.x;
                end;
            end
            else
            begin
              if {Multflag and} (p <> Root) then
                if position.x = anc.position.x then
                  hidden := true;
              SetX(des1);
              SetX(des2);
            end;
        end;
    end; { SetX }

    procedure SetCurSizeStraight(p : TpNode);
    begin
        if p.OTU or (Topoflag and p.compressed) then
            p.cursize := 1
        else if p.compressed then
          p.cursize := p.width/yunit
        else
        begin
            SetCurSizeStraight(p.des1);
            SetCurSizeStraight(p.des2);
            p.cursize := p.des1.cursize +p.des2.cursize;
        end;
    end;

    procedure SetXStraight(p : TpNode);
    begin
        if p = Root then
            p.position.x := 0
        else if p.OTU or p.compressed then
        begin
            p.position.x := Round(MaxDepth*xunit);
            if not p.OTU then
            begin
                p.des1.position.x := p.position.x;
                p.des2.position.x := p.position.x;
            end;
        end
        else begin
                q1 := p;
                while (q1 <> Root) and (q1.anc.depth = p.depth) do q1 := q1.anc;
                p.position.x := Round((1-(q1.cursize-1)/(n-1))*MaxDepth*xunit)
        end;
        if not (p.OTU or p.compressed) then
        begin
          if {Multflag and} (p <> Root) then
            if p.position.x = p.anc.position.x then
              p.hidden := true;
          SetXStraight(p.des1);
          SetXStraight(p.des2);
        end;
    end;

    procedure SetXCurve(p : TpNode);
    var x0,x1,y0,y1: integer;
        a: double;
    begin
      if p.OTU or p.compressed then Exit;
      if p <> Root then
        with p.position do
        begin
          q1 := p;
          if p = p.anc.des1 then
            if p.position.y < p.anc.position.y then
              while (q1 <> Root)
              and (   (q1.position.y < q1.anc.position.y)
                   or ((q1.position.y = q1.anc.position.y) and (q1 = q1.anc.des1))) do
                q1 := q1.anc
            else
              q1 := q1.anc
          else
            if p.position.y > p.anc.position.y then
              while (q1 <> Root)
              and (   (q1.position.y > q1.anc.position.y)
                   or ((q1.position.y = q1.anc.position.y) and (q1 = q1.anc.des2))) do
                q1 := q1.anc
            else
              q1 := q1.anc;

          x0 := q1.position.x;
          y0 := q1.position.y;

          q2 := p;
          if y < y0 then
            while not (q2.OTU or q2.compressed) do q2 := q2.des1
          else
            while not (q2.OTU or q2.compressed) do q2 := q2.des2;
          x1 := q2.position.x;
          y1 := q2.position.y;

          if p.depth = p.anc.depth then
            x := p.anc.position.x
          else begin
            a := abs(y1-y0)*sqrt(3)/(x1-x0);
            if abs(y-y0) > abs(y1-y0) then begin
            end
            else
              if 2*abs(y-y0) < abs(y-y1) then
                x := round(x0 +1/a*sqrt(4/9*(y1-y0)*(y1-y0) -(1/3*y0+ 2/3*y1 -y)*(1/3*y0+ 2/3*y1 -y)))
              else
                x := round(x1 -1/a*sqrt(16/9*(y1-y0)*(y1-y0) -(4/3*y0 -1/3*y1 -y)*(4/3*y0 -1/3*y1 -y)));
          end;
        end;
      if {Multflag and} (p <> Root) then
        if p.position.x = p.anc.position.x then
          p.hidden := true;
      SetXCurve(p.des1);
      SetXCurve(p.des2);
    end;

    procedure SetY(p : TpNode);
    var
      scaleFactor: Integer = 2;
    begin
      if p.OTU then begin
        p.position.y := yy +(yunit div scaleFactor);
        yy := yy +yunit;
      end
      else if p.compressed then
        if Topoflag then begin
          p.position.y := yy +(yunit div scaleFactor);
          yy := yy +yunit;
          p.des1.position.y := p.position.y -(yunit div scaleFactor);
          p.des2.position.y := p.position.y +(yunit div scaleFactor);
        end
        else begin
          p.position.y := yy +((p.width+gunit) div (scaleFactor));
          yy := yy +p.width+gunit;
          p.des1.position.y := p.position.y -(gunit*p.size div (scaleFactor));
          p.des2.position.y := p.position.y +(gunit*p.size div (scaleFactor));
        end
      else begin
          SetY(p.des1);
          SetY(p.des2);
              q1 := p;
              q2 := p;
              repeat
                  q1 := q1.des1;
              until (q1.position.x > p.position.x) or (q1.OTU or q1.compressed);
              repeat
                  q2 := q2.des2;
              until (q2.position.x > p.position.x) or (q2.OTU or q2.compressed);
          p.position.y := (q1.position.y + q2.position.y) div scaleFactor;
      end;
    end; { SetY }

    procedure SetYStraight(p: TpNode);

        procedure SetYStraightFromBottom(p: TpNode);
        begin
            with p^ do begin
                if p.OTU then begin
                  p.position.y := yy +(yunit div 2);
                  yy := yy +yunit;
                end
                else if p.compressed then
                  if Topoflag then begin
                    p.position.y := yy +(yunit div 2);
                    yy := yy +yunit;
                    p.des1.position.y := p.position.y -(yunit div 2);
                    p.des2.position.y := p.position.y +(yunit div 2);
                  end
                  else begin
                    p.position.y := yy +(p.width div 2);
                    yy := yy +p.width;
                    p.des1.position.y := p.position.y -(gunit*p.size div 2);
                    p.des2.position.y := p.position.y +(gunit*p.size div 2);
                  end
                else begin
                    SetYStraightFromBottom(des1);
                    SetYStraightFromBottom(des2);
                    if Topoflag then begin
                        q1 := p;
                        while not (q1.OTU or q1.compressed) do q1 := q1.des1;
                        q2 := p;
                        while not (q2.OTU or q2.compressed) do q2 := q2.des2;
                    end
                    else begin
                        q1 := p;
                        while (q1.position.x = p.position.x) and (not(q1.OTU or q1.compressed)) do
                          q1 := q1.des1;
                        while not (q1.OTU or q1.compressed) do
                          q1 := q1.des2;
                        q2 := p;
                        while (q2.position.x = p.position.x) and (not(q2.OTU or q2.compressed)) do
                          q2 := q2.des2;
                        while not (q2.OTU or q2.compressed) do q2 := q2.des1;
                    end;
                    position.y := (q1.position.y + q2.position.y) div 2;
                end;
            end;
        end;

        procedure SetYStraightFromTop(p: TpNode);
        begin
          with p^ do
            if not OTU and compressed then
              position.y := anc.position.y -(anc.position.y-position.y)*(position.x-anc.position.x) div (des1.position.x-anc.position.x)
            else if not OTU then begin
              SetYStraightFromTop(des1);
              SetYStraightFromTop(des2);
            end;
        end;

    begin
      SetYStraightFromBottom(p);
      if (BranchStyle = bsStraight) and (not Topoflag) then
        SetYStraightFromTop(p);
    end;

    procedure SetYForMultifurcation(p : TpNode);
    begin
        if p <> Root then
            if (TreeStyle = tsCircle) then begin
                if p.avglen = p.anc.avglen then begin
                  if Topoflag or (not p.compressed) then begin
                    p.angle := p.anc.angle;
                    p.position.x := p.anc.position.x;
                    p.position.y := p.anc.position.y;
                  end;
                end
            end
            else if p.position.x = p.anc.position.x then
                p.position.y := p.anc.position.y;
        if not p.des1.OTU then SetYForMultifurcation(p.des1);
        if not p.des2.OTU then SetYForMultifurcation(p.des2);
    end;

    procedure SetCurSizeAngle(p : TpNode);
    begin
        if p.OTU or (Topoflag and p.compressed) then
            p.cursize := 1
        else if p.compressed then
        begin
          if IsLinearized or ForceLinearized then
            p.cursize := p.size*gunit/yunit +1
          else if p.height < 0.00000000001 then  // added on April 25, 2007
            p.cursize := 1
          else
            p.cursize := p.width/yunit*Root.height/p.height;
//            p.cursize := ArcTan(p.avglen*xunit/(p.width-yunit))/ArcTan(p.avglen*xunit/(p.width))+1
          if p.cursize < 1 then
            p.cursize := 1
        end
        else
        begin
            SetCurSizeAngle(p.des1);
            SetCurSizeAngle(p.des2);
            p.cursize := p.des1.cursize +p.des2.cursize;
        end;
    end;

    procedure SetAnglePosition;

      procedure SetAvgLen(p : TpNode);
      begin
        if p.OTU then
        begin
          if p.branch.length > 0.0 then
            p.avglen := p.branch.length +p.namesize.x/xunit
          else
            p.avglen := p.namesize.x/xunit;
        end
        else if p.compressed then
        begin
          if p.branch.length > 0.0 then
            p.avglen := p.height +p.branch.length
          else
            p.avglen := p.height;
          if p.width < yunit then
            p.avglen := p.avglen +p.namesize.x/xunit;
        end
        else
        begin
          SetAvgLen(p.des1);
          SetAvgLen(p.des2);
          p.avglen := (p.des1.avglen + p.des2.avglen)/2;
          if p.branch.length > 0.0 then
            p.avglen := p.avglen +p.branch.length;
        end;
      end;

      procedure SetNodeAngle(p : TpNode);
      var h, s0, s1, b1, b2, n1, n2: double;
      begin
        if p.branch.length <= 0.0 then
		begin
          s0 := p.sector;
          s1 := s0;
        end
        else
		begin
          b1 := p.branch.length;
          h := p.avglen;
          s0 := p.sector;
          s1 := arcsin(h*sin(s0)/sqrt(h*h +b1*b1 -2*b1*h*cos(s0)));
          if s1 < s0 then
            s1 := PI -s1;
        end;
        n1 := p.des1.cursize;// p.des1.position.y;
        n2 := p.des2.cursize;// p.des2.position.y;
        if (n1+n2) < 0.00000000001 then
          if (p.des1.avglen <= 0.0) or (p.des2.avglen <= 0.0) then
          begin
            p.des1.sector := s1/2;
            p.des2.sector := s1/2;
          end
          else
          begin
            b1 := p.des1.avglen;
            b2 := p.des2.avglen;
            p.des1.sector := s1/b1/(1/b1 + 1/b2);
            p.des2.sector := s1/b2/(1/b1 + 1/b2);
          end
        else if (p.des1.avglen <= 0.0) or (p.des2.avglen <= 0.0) then
        begin
          p.des1.sector := s1*n1/(n1 + n2);
          p.des2.sector := s1*n2/(n1 + n2);
        end
        else
        begin
          b1 := p.des1.avglen;
          b2 := p.des2.avglen;
          p.des1.sector := s1*n1/b1/(n1/b1 + n2/b2);
          p.des2.sector := s1*n2/b2/(n1/b1 + n2/b2);
        end;
        p.des1.angle := p.angle +s1 -p.des1.sector;
        p.des2.angle := p.angle -s1 +p.des2.sector;
        if p.des1.branch.length > 0.0 then
          b1 := p.des1.branch.length*xunit
        else
          b1 := 0.0;
        if p.des2.branch.length > 0.0 then
          b2 := p.des2.branch.length*xunit
        else
          b2 := 0.0;
        p.des1.position.x := p.position.x +round(cos(p.des1.angle)*b1);
        p.des1.position.y := p.position.y -round(sin(p.des1.angle)*b1);
        p.des2.position.x := p.position.x +round(cos(p.des2.angle)*b2);
        p.des2.position.y := p.position.y -round(sin(p.des2.angle)*b2);
        if not(p.des1.OTU or p.des1.compressed) then
          SetNodeAngle(p.des1);
        if not(p.des2.OTU or p.des2.compressed) then
          SetNodeAngle(p.des2);
      end;

    var i : integer;
        n1, n2: double;
    begin
      SetAvgLen(Root);
      SetCurSizeAngle(Root);

      Root.position.x := 0;
      Root.position.y := 0;

      n1 := Root.des1.cursize; //Root.des1.position.y;
      n2 := Root.des2.cursize; //Root.des2.position.y;
      if Topoflag or (Root.des1.avglen <= 0.0) or (Root.des2.avglen <= 0.0) then begin
        Root.des1.sector := PI*n1/(n1 + n2);
        Root.des2.sector := PI*n2/(n1 + n2);
      end
      else begin
        Root.des1.sector := PI*n1/Root.des1.avglen/(n1/Root.des1.avglen + n2/Root.des2.avglen);
        Root.des2.sector := PI*n2/Root.des2.avglen/(n1/Root.des1.avglen + n2/Root.des2.avglen);
      end;
      if (isRooted and ShowRoot) then begin
        Root.des1.sector := Root.des1.sector/3;
        Root.des2.sector := Root.des2.sector/3;
        Root.des1.angle := Root.des1.sector;
        Root.des2.angle := -Root.des2.sector;
      end
      else begin
        Root.des1.angle := (1/2 -StartAngle/6)*PI;
        Root.des2.angle := (3/2 -StartAngle/6)*PI;
      end;
      if Root.des1.compressed then begin
        Root.des1.position.x :=  round(cos(Root.des1.angle)*(Root.des1.branch.length+Root.des1.height)*xunit);
        Root.des1.position.y := -round(sin(Root.des1.angle)*(Root.des1.branch.length+Root.des1.height)*xunit);
      end
      else begin
        Root.des1.position.x :=  round(cos(Root.des1.angle)*Root.des1.branch.length*xunit);
        Root.des1.position.y := -round(sin(Root.des1.angle)*Root.des1.branch.length*xunit);
      end;
      if Root.des2.compressed then begin
        Root.des2.position.x :=  round(cos(Root.des2.angle)*(Root.des2.branch.length+Root.des2.height)*xunit);
        Root.des2.position.y := -round(sin(Root.des2.angle)*(Root.des2.branch.length+Root.des2.height)*xunit);
      end
      else begin
        Root.des2.position.x :=  round(cos(Root.des2.angle)*Root.des2.branch.length*xunit);
        Root.des2.position.y := -round(sin(Root.des2.angle)*Root.des2.branch.length*xunit);
      end;
      Canvas.Font.Assign(OTU_font);
      if not Root.des1.OTU then
        SetNodeAngle(Root.des1);
      if not Root.des2.OTU then
        SetNodeAngle(Root.des2);
      for i := NoOfOTUs+1 To NoOfNodes do
      begin
//        if Node[i].hidden then continue;
        if Node[i].compressed then
        begin
          Node[i].des1.position.x := Node[i].position.x +round(cos(Node[i].angle)*Node[i].height*xunit)
                                                        +round(cos(Node[i].angle+PI/2)*Node[i].size*gunit/2);
          Node[i].des1.position.y := Node[i].position.y -round(sin(Node[i].angle)*Node[i].height*xunit)
                                                        -round(sin(Node[i].angle+PI/2)*Node[i].size*gunit/2);
          Node[i].des2.position.x := Node[i].position.x +round(cos(Node[i].angle)*Node[i].height*xunit)
                                                        -round(cos(Node[i].angle+PI/2)*Node[i].size*gunit/2);
          Node[i].des2.position.y := Node[i].position.y -round(sin(Node[i].angle)*Node[i].height*xunit)
                                                        +round(sin(Node[i].angle+PI/2)*Node[i].size*gunit/2);
        end;
      end;
    end;

    procedure SetCirclePosition;
    var q1, q2: TpNode;
        OriAngle, CurrAngle, dAngle: double;

      procedure SetAvgLen(p : TpNode);
      begin
        with p^ do begin
          if p = Root then
            avglen := Radius*CenterMargin/100
          else if Topoflag then
          begin
            if OTU or compressed then
            begin
              avglen := Radius;
              if not OTU then begin
                des1.avglen := Radius;
                des2.avglen := Radius;
              end
            end
            else
              avglen := Radius -depth*xunit;
            if p.depth = p.anc.depth then
              p.hidden := true;
          end
          else if IsLinearized or ForceLinearized then
          begin
            if OTU then
              avglen := Radius
            else
              avglen := Radius -height*xunit;
            if avglen <= anc.avglen then
            begin
              avglen := anc.avglen;
              if (not compressed) and (not OTU) then
                hidden := true;
            end;
            if not OTU and compressed then
            begin
              des1.avglen := Radius;
              des2.avglen := Radius;
            end;
          end
          else
          begin
            if branch.length > 0.0000000000001 then
              avglen := anc.avglen +branch.length*xunit
            else
            begin
              avglen := anc.avglen;
              if (not compressed) and (not OTU) then
                hidden := true;
            end;
            if not OTU and compressed then
            begin
              des1.avglen := avglen +height*xunit;
              des2.avglen := avglen +height*xunit;
            end;
          end;
          if not (OTU or compressed) then
          begin
            SetAvgLen(des1);
            SetAvgLen(des2);
          end;
        end;
      end;

      procedure SetAngle(p: TpNode);
      begin
        if p.OTU or p.compressed then
          if Abs(OriAngle - CurrAngle) < 0.00000000001 then
          begin
            p.angle := CurrAngle;
            CurrAngle := CurrAngle -dAngle*p.cursize/2;
          end
          else
          begin
            p.angle := CurrAngle -dAngle*p.cursize/2;
            CurrAngle := CurrAngle -dAngle*p.cursize;
          end
        else
        begin
          SetAngle(p.des1);
          SetAngle(p.des2);
//          if Multflag then
//          begin
            q1 := p;
            q2 := p;
            repeat
              q1 := q1.des1;
            until (q1.avglen > p.avglen) or q1.OTU or q1.compressed;
            repeat
              q2 := q2.des2;
            until (q2.avglen > p.avglen) or q2.OTU or q2.compressed;
            p.angle := (q1.angle + q2.angle)/2;
//          end
//          else
//            p.angle := (p.des1.angle + p.des2.angle)/2;
        end;
      end;

    var
      i: integer;
    begin
      if Topoflag then
          xunit := xunit*Radius/MinTreeWidth*(1.0 -CenterMargin/100)
      else
          xunit := xunit*Radius/FTreeWidth*(1.0 -CenterMargin/100);

      SetAvgLen(Root);
      SetCurSizeAngle(Root);
      if CurrentNoOfOTUs > 18 then
          dAngle := 17/9*PI/(Root.cursize-1)
      else
          dAngle := 2*PI/Root.cursize;
      OriAngle := (1/2 -StartAngle/6)*PI;
      CurrAngle := OriAngle;
      SetAngle(Root);

      for i := 1 to NoOfNodes do
        with Node[i]^ do
        begin
          if hidden then Continue;
          position.x :=  round(cos(angle)*avglen);
          position.y := -round(sin(angle)*avglen);
          if not OTU and compressed then
          begin
            des1.position.x :=  round(cos(angle)*des1.avglen) +round(cos(angle+PI/2)*gunit*size/2);
            des1.position.y := -round(sin(angle)*des1.avglen) -round(sin(angle+PI/2)*gunit*size/2);
            des2.position.x :=  round(cos(angle)*des2.avglen) -round(cos(angle+PI/2)*gunit*size/2);
            des2.position.y := -round(sin(angle)*des2.avglen) +round(sin(angle+PI/2)*gunit*size/2);
          end;
        end;
    end;

    procedure ResetHidden(p: TpNode);
    begin
      p.hidden := false;
      if not (p.compressed or p.OTU) then
      begin
        ResetHidden(p.des1);
        ResetHidden(p.des2);
      end;
    end;

begin
    if not TreeExist then Exit;
    FMaxXPosition := 0;
    ResetHidden(Root);
    if not IsResizing then
      ProgressCallback(5);
    if IsBranchLength or ((IsLinearized or ForceLinearized) and (Root.Height > 0.000000000001)) then
    begin
      if (IsLinearized or ForceLinearized) and (Root.Height > 0.000000000001) then
        xmax := Root.Height
      else
        xmax := Max(Root.des1.branch.maxlen2, Root.des2.branch.maxlen2);
      if xmax < 0.000000000001 then begin
        Topoflag := true;
      end;
    end
    else
      Topoflag := true;
    if not IsResizing then
      ProgressCallback(10);
    if Topoflag then
    begin
        if IsCondensed then
            SetDepth(Root, FCondenseValue)
        else
            SetDepth(Root, 0);
        MaxDepth := Max(Root.depth, 1);
        xunit := MinTreeWidth div MaxDepth;
        if xunit = 0.0 then
            xunit := 1.0;
    end
    else if (abs(PixelsPerUnit) < 0.00000000000001) then
    begin
        xunit := FTreeWidth/xmax;
        FPixelsPerUnit := xunit;
    end
    else
    begin
        xunit := PixelsPerUnit;
        FTreeWidth := Ceil(xmax*PixelsPerUnit);
    end;
    if not IsResizing then
      ProgressCallback(20);
    yy := 0;

    if TreeStyle = tsCircle then
        SetCirclePosition
    else if TreeStyle = tsRadiation then
        SetAnglePosition
    else begin
        if (BranchStyle = bsStraight) and Topoflag then begin
            SetCurSizeStraight(Root);
            n := Round(Root.cursize);
            SetXStraight(Root);
        end
        else
            SetX(Root);
        case BranchStyle of
           bsRectangular : SetY(Root);
           bsCurved :     if Topoflag then
                            SetY(Root)
                          else
                            SetYStraight(Root);
           bsStraight :  SetYStraight(Root);
        else
           SetY(Root);
        end;
    end;
    if not IsResizing then
      ProgressCallback(40);
    if (TreeStyle = tsCircle) or ((TreeStyle = tsTraditional) and (BranchStyle <> bsRectangular)) then
      SetYForMultifurcation(Root);
    if not IsResizing then
      ProgressCallback(60);
    if (TreeStyle = tsTraditional) and (BranchStyle = bsCurved) and Topoflag then
      SetXCurve(Root);
    if not IsResizing then
      ProgressCallback(80);
    if TreeStyle = tsTraditional then
      SetCaptionPosition;
    if not IsResizing then
      ProgressCallback(100);
    SetYmax;
end; { SetPosition }

procedure TTreeCustomControl.SetCaptionPosition;
var
  i: Integer = -1;
begin
   if (AttribList.Count <= 1) and (GroupAttrib.Count = 0) then Exit;

   for i := 1 to NoOfNodes do
   begin
     if (Node[i].attrindex = 0) and (Node[i].groupindex = -1) then
       Node[i].capdepth := 0
     else if Node[i].compressed then
       Node[i].capdepth := 0
     else
       Node[i].capdepth := 1;
     Node[i].Bracket.Top := 0;
     Node[i].Bracket.Bottom := 0;
     Node[i].Bracket.Left := 0;
   end;
   Canvas.Font.Assign(CharStateFont);
   SetCaptionX(Root);
   SetCaptionY(Root);

   if AlignCaption then
     AlignCaptions;
end;

procedure TTreeCustomControl.AlignCaptions;
var
  max: integer;

  procedure SetXAlign(p: TpNode; d: integer);
  var
    a: TNodeAttrib;
  begin
    if not (p.OTU or p.compressed) then
    begin
      SetXAlign(p.des1, d);
      SetXAlign(p.des2, d);
    end;
    if p = Root then exit;
    if p.capdepth <> d then exit;
    if p.attrindex = p.anc.attrindex then
    begin
      if p.anc.Bracket.Left < p.bracket.Left then
        p.anc.Bracket.Left := p.bracket.Left;
    end
    else
    begin
      a := GetNodeAttrib(p);
      if p.anc.bracket.Left < p.bracket.Left+a.CaptionSize then
        p.anc.bracket.Left := p.bracket.Left+a.CaptionSize;

    end;
  end;

  procedure SetGroupDepth(p: TpNode; gd: integer);
  var
    a: TNodeAttrib;
  begin
    if not (p.OTU or p.compressed) then
    begin
      SetGroupDepth(p.des1, gd);
      SetGroupDepth(p.des2, gd);
    end;
    if p = Root then exit;
    if p.attrindex < 0 then
      p.capdepth := gd;
    if p.attrindex = p.anc.attrindex then
    begin
      if p.anc.capdepth < p.capdepth then
        p.anc.capdepth := p.capdepth;
    end
    else
    begin
      a := GetNodeAttrib(p);
      if a.ShowLabel or a.ShowBracket then
        if p.anc.capdepth <= p.capdepth then
          p.anc.capdepth := p.capdepth +1;
    end;
  end;

var
  d,i,j: integer;
  a: TNodeAttrib;
begin
  if UseGroupAttrib and (GroupAttrib.Count > 0) then
  begin
    d := 0;
    for i := 1 to NoOfNodes do
      if (Node[i].attrindex < 0) and (Node[i].capdepth > d) then
        d := Node[i].capdepth;
    SetGroupDepth(Root, d);
  end;

  d := 0;
  for i := 1 to NoOfNodes do
    if Node[i].capdepth > d then
      d := Node[i].capdepth;

  for i := 1 to d do
  begin
    max := 0;
    for j := 1 to NoOfNodes do
    begin
      if Node[j] = Root then continue;
      if Node[j].capdepth <> i then continue;
      if Node[j].attrindex = 0 then continue;
      if (Node[j].attrindex = Node[j].anc.attrindex) and
         (Node[j].groupindex = Node[j].anc.groupindex) then
        continue;

      a := GetNodeAttrib(Node[j]);
      //if a.ShowLabel or a.ShowBracket or (a.ShowImage and not a.Image.Empty) then
        if Node[j].Bracket.Left +abs(a.CaptionFont.Height) > max then
          max := Node[j].Bracket.Left +abs(a.CaptionFont.Height);
    end;
    for j := 1 to NoOfNodes do
    begin
      if Node[j] = Root then continue;
      if Node[j].capdepth <> i then continue;
      if Node[j].attrindex = 0 then continue;
      if (Node[j].attrindex = Node[j].anc.attrindex) and
         (Node[j].groupindex = Node[j].anc.groupindex) then
        continue;

      a := GetNodeAttrib(Node[j]);
      //if a.ShowLabel or a.ShowBracket or (a.ShowImage and not a.Image.Empty) then
        Node[j].Bracket.Left := max -abs(a.CaptionFont.Height);
    end;
    SetXAlign(Root, i);
  end;
end;

procedure TTreeCustomControl.SetCaptionY(p: TpNode);
var
  d: integer;
  q: TpNode;
begin
  if not (p.OTU or p.compressed) then
  begin
    SetCaptionY(p.des1);
    SetCaptionY(p.des2);
  end;

  if p = Root then exit;
  //if p.attrindex = p.anc.attrindex then exit;

  q := p;
  while not (q.OTU or q.compressed) do
    q := q.des1;
  if q.compressed and not q.OTU and not Topoflag then
    q := q.des1;
  if q.compressed then
    d := abs(GetNodeAttrib(q).CaptionFont.Height) div 2
  else
    d := abs(GetNodeAttrib(q).Font.Height) div 2;
  if d > (yunit div 2) then
    d := yunit div 2;
  p.Bracket.Top := q.position.y - d;

  q := p;
  while not (q.OTU or q.compressed) do
    q := q.des2;
  if q.compressed and not q.OTU and not Topoflag then
    q := q.des2;

  if q.compressed then
    d := abs(GetNodeAttrib(q).CaptionFont.Height) div 2
  else
    d := abs(GetNodeAttrib(q).Font.Height) div 2;
  if d > (yunit div 2) then
    d := yunit div 2;
  p.Bracket.Bottom := q.position.y + d;
end;

procedure TTreeCustomControl.SetCaptionX(p: TpNode);
var
  a: TNodeAttrib = nil;
begin
  if not (p.OTU or p.compressed) then
  begin
    SetCaptionX(p.des1);
    SetCaptionX(p.des2);
  end;

  if p <> Root then
  begin
    a  := GetNodeAttrib(p);

    if p.compressed then
    begin
      if p.OTU or Topoflag then
        p.Bracket.Left := p.position.X
      else
        p.Bracket.Left := p.des1.position.X
    end
    else if p.OTU then
    begin
      if FMakeBranchesEqualLength and FAlignOtuNames then
        p.Bracket.Left := FMaxXPosition + GetLineWidth(p) + Abs(a.Font.Height)
      else if FAlignOtuNames then
        p.Bracket.Left := Round(xmax*xunit) + GetLineWidth(p) + abs(a.Font.Height)
      else
        p.Bracket.Left := p.position.X +GetLineWidth(p) +abs(a.Font.Height);
      if a.ShowTaxonName then
        p.Bracket.Left := p.Bracket.Left +p.namesize.x
      else if a.ShowSpeciesName then
        p.Bracket.Left := p.Bracket.left + P.speciesNameSize.x;
      if GetTaxonMarker(p).Shape <> msNone then
        p.Bracket.Left := p.Bracket.Left +Abs(a.Font.Height)*5;
      if ShowCharState then
      begin
        Canvas.Font.Assign(CharStateFont);
        p.Bracket.Left := p.Bracket.Left +Canvas.TextWidth(p.charstate) +Abs(CharStateFont.Height);
      end;
    end;

    if p.attrindex = p.anc.attrindex then
    begin
      if p.anc.capdepth < p.capdepth then
        p.anc.capdepth := p.capdepth;
      if p.anc.Bracket.Left < p.bracket.Left then
        p.anc.bracket.Left := p.bracket.Left;
    end
    else
    begin
      if p.compressed then
      begin
        if p.anc.capdepth = 0 then
          p.anc.capdepth := 1;
      end
      else if a.CaptionSize > 0 then
      begin
        if p.capdepth = 0 then
          p.capdepth := 1;
        if p.anc.capdepth <= p.capdepth then
          p.anc.capdepth := p.capdepth +1;
      end
      else
      begin
        if p.anc.capdepth < p.capdepth then
          p.anc.capdepth := p.capdepth;
      end;
      if p.anc.Bracket.Left < p.bracket.Left +a.CaptionSize then
        p.anc.Bracket.Left := p.bracket.Left +a.CaptionSize;
    end;
  end;
end;

procedure TTreeCustomControl.SetYmax;
var
  i: Integer;
  yMaxIndex: Integer = -1;
  n: TpNode = nil;
begin
  ymax := 0;
  for i := 1 to FNoOfOTUs do
    if Node[i].position.y > ymax then
    begin
      ymax := Node[i].position.y;
      yMaxIndex := i;
    end;
  if (yMaxIndex >= 0) and InCompressedClade(Node[yMaxIndex]) then
  begin
    n := GetCompressedAncestor(Node[yMaxIndex]);
    ymax := n.position.y +(gunit*n.size div 2);
  end;
end;

procedure TTreeCustomControl.SetTreeSize;
var
  max, min : TPoint;
  a: TNodeAttrib;
  w,h,x,y,i : integer;
  tempW: Integer;
  Stat: Double;
  d0: Double = 0;
  d1: Double = 0;
  StatStr: String;
  tempScrollLeft, tempScrollTop: Integer;

  function UseNodeForStatsPos(aNode: TpNode): Boolean;
  begin
    Result := False;
     if Node[i].hidden then Exit;
     if Node[i] = Root then
       Exit
     else if Node[i] = Root.des1 then
     begin
       if Root.des2.OTU or (Root.des2.position.x > Node[i].position.x) then
         Exit;
     end
     else if Node[i] = Root.des2 then
     begin
       if Root.des1.OTU or (Root.des1.position.x > Node[i].position.x) then
         Exit;
     end;
     Result := True;
  end;

begin
    if not TreeExist then Exit;
    if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then
    begin
        xbase := Abs(OTU_Font.Height);
        ybase := Abs(OTU_Font.Height);
        min.x := 0;
        min.y := 0;
        max.x := 0;
        max.y := 0;

        for i := 1 to NoOfNodes do
        begin
          if not (Node[i].OTU or Node[i].compressed) then Continue;
          if Node[i].hidden then Continue;

          a := GetNodeAttrib(Node[i]);
          w := abs(a.Font.Height) +GetLineWidth(Node[i])*2;

          if a.ShowTaxonName then
            w := w +Node[i].namesize.x
          else if a.ShowSpeciesName then
            w := w + Node[i].speciesNameSize.x; { TODO 1 -oglen -cgenedups : handle namesize for species names }

          if GetTaxonMarker(Node[i]).Shape <> msNone then
            if Node[i].Compressed then
              w := w +Abs(a.CaptionFont.Height)*5
            else
              w := w +Abs(a.Font.Height)*5;

          h := Abs(a.Font.Height)*6;
          if HorzTaxonName then
            if (i > NoOfOTUs) and Node[i].compressed then
            begin
              if cos(Node[i].angle) < 0.0 then
                x := (Node[i].des1.position.x +Node[i].des2.position.x) div 2 -w
              else
                x := (Node[i].des1.position.x +Node[i].des2.position.x) div 2 +w;
              if sin(Node[i].angle) < 0.0 then
                y := (Node[i].des1.position.y +Node[i].des2.position.y) div 2 +h
              else
                y := (Node[i].des1.position.y +Node[i].des2.position.y) div 2 -h;
            end
            else
            begin
              if cos(Node[i].angle) < 0.0 then
                x := Node[i].position.x -w
              else
                x := Node[i].position.x +w;
              if sin(Node[i].angle) < 0.0 then
                y := Node[i].position.y +h
              else
                y := Node[i].position.y -h;
            end
          else if (i > NoOfOTUs) and Node[i].compressed then
          begin
            x := (Node[i].des1.position.x +Node[i].des2.position.x) div 2 +round(cos(Node[i].angle)*w);
            y := (Node[i].des1.position.y +Node[i].des2.position.y) div 2 -round(sin(Node[i].angle)*w);
          end
          else
          begin
            x := Node[i].position.x +round(cos(Node[i].angle)*w);
            y := Node[i].position.y -round(sin(Node[i].angle)*w);
          end;
          if x < min.x then
            min.x := x
          else if x > max.x then
            max.x := x;
          if y < min.y then
            min.y := y
          else if y > max.y then
            max.y := y;
          if (not IsResizing) and ((i mod Governer) = 0) then
            ProgressCallback(Round(i / NoOfNodes * 100));
        end;
        for i := 1 to NoOfNodes do begin
            Node[i].position.x := Node[i].position.x -min.X;
            Node[i].position.y := Node[i].position.y -min.Y;
        end;

        Origin.X := -min.X;
        Origin.Y := -min.Y;
        max.x := max.x -min.x +2*xbase;
        max.y := max.y -min.y +2*ybase;
    end
    else
    begin
      if not IsResizing then
        ProgressCallback(10);
      repeat
        max.x := 0;
        max.y := 0;

        for i := 1 to NoOfOTUs do
        begin
            if Node[i].hidden then Continue;
            w := Node[i].position.x + Canvas.TextWidth(Node[i].name);
            if w > max.x then max.x := w;
            if Node[i].position.y > max.y then max.y := Node[i].position.y;
        end;
        Canvas.Font.Assign(CharStateFont);
        for i := 1 to NoOfNodes do
        begin
            if Node[i].hidden then Continue;
            if Node[i].compressed and (not Node[i].OTU) and (not Topoflag) then
              w := Node[i].des1.position.x
            else
              w := Node[i].position.x;
            w := w +GetLineWidth(Node[i])*2;

            if Node[i].compressed or Node[i].OTU then
            begin
              a := GetNodeAttrib(Node[i]);
              if a.ShowTaxonName then
                w := w +Node[i].namesize.x
              else if a.ShowSpeciesName then
                w := w + Node[i].speciesNameSize.x;
              if GetTaxonMarker(Node[i]).Shape <> msNone then
                if Node[i].compressed then
                  w := w +Abs(a.CaptionFont.Height)*5
                else
                  w := w +Abs(a.Font.Height)*5;
              if Node[i].OTU and ShowCharState then
                w := w +Canvas.TextWidth(Node[i].charstate) +Abs(CharStateFont.Height)*2;
            end;

            if w > max.x then
              max.x := w;

            h := Node[i].position.y;
            if not Topoflag and Node[i].compressed and (not Node[i].OTU) then
              if h < Node[i].des2.position.Y then
                h := Node[i].des2.position.Y;
            if h > max.y then max.y := h;
        end;
        for i := 1 to NoOfNodes do
        begin
            if Node[i] <> Root then
            begin
              a := GetNodeAttrib(Node[i]);
              if (not Node[i].compressed) and
                 ((Node[i].attrindex <> Node[i].anc.attrindex) or (Node[i].groupindex <> Node[i].anc.groupindex))then
              begin
                w := Node[i].bracket.Left + a.CaptionSize;
                if w > max.x then
                  max.x := w;
              end;
            end;
        end;
        if IsLinearized or FShowSamplingTimeScale then
        begin
            Canvas.Font.Assign(ScaleFont);
            i := -1; //@SK mega2b4 to shut up compiler
            if IsScale and ShowScale then
            begin
                i := Pos('.', FScaleText);
                if i > 0 then
                    i := Length(FScaleText)-i;
                w := Round(xmax*xunit)
                    +Canvas.TextWidth(FloatToStrF(0.0, DecimalDisplayMode, 15, i))*2
                    +Canvas.TextWidth(FScaleUnit+'XXX')*4;
               if w > max.x then
                 max.x := w;
            end;
            if IsTimeScale and (ShowTimeScale or ShowSamplingTimeScale) then
            begin
                if not ShowScale then
                begin
                    i := Pos('.', FTimeText);
                    if i > 0 then
                        i := Length(FTimeText)-i;
                end;
                w := Round(xmax*xunit)
                    +Canvas.TextWidth(FloatToStrF(0.0, DecimalDisplayMode, 15, i))*2
                    +Canvas.TextWidth(FTimeUnit+'XXX')*4;
                if w > max.x then
                  max.x := w;
            end;
        end;

        if (isRooted and ShowRoot) then
            xbase := 40 + LENGTH_OF_ROOT_BRANCH
        else
            xbase := 40;

        if ShowNodeIds then
        begin
          w := 0;
          Canvas.Font.Assign(StatsFont);
          for i := NoOfOTUs+1 to NoOfNodes do
          begin
            if Node[i].hidden then
              Continue;
//            if Node[i] = Root then
//              Continue
            if Node[i] = Root.des1 then
            begin
              if Root.des2.OTU or (Root.des2.position.x > Node[i].position.x) then
                Continue;
            end
            else if Node[i] = Root.des2 then
            begin
              if Root.des1.OTU or (Root.des1.position.x > Node[i].position.x) then
                Continue;
            end;
            if w > Node[i].position.x -(Canvas.TextWidth(IntToStr(Node[i].branch.stat2))*4 +BranchPen.Width*2 +10) then
                w := Node[i].position.x -(Canvas.TextWidth(IntToStr(Node[i].branch.stat2))*4 +BranchPen.Width*2 +10);
          end;
          if xbase +w < 40 then
            xbase := 40 -w;
        end
        else if ShowStats then begin
            w := 0;
            tempW := 0;
            Canvas.Font.Assign(StatsFont);
            for i := NoOfOTUs+1 to NoOfNodes do
            begin
              if UseNodeForStatsPos(Node[i]) then
              begin
                tempW := (Canvas.TextWidth(IntToStr(Node[i].branch.stat2)) + BranchPen.Width*2 + 10);
                if w > Node[i].position.x - tempW then
                    w := Node[i].position.x - tempW;
              end;
            end;
            if xbase +w < 40 then
              xbase := 40 -w;
        end
        else if ShowDivergenceTimes then
        begin
            w := 0;
            Canvas.Font.Assign(TimesFont);
            for i := NoOfOTUs+1 to NoOfNodes do
            begin
              if Node[i].hidden then Continue;
              if Node[i] = Root then
                Continue
              else if Node[i] = Root.des1 then begin
                if Root.des2.OTU or (Root.des2.position.x > Node[i].position.x) then Continue;
              end
              else if Node[i] = Root.des2 then begin
                if Root.des1.OTU or (Root.des1.position.x > Node[i].position.x) then Continue;
              end;
              Stat := Node[i].Height * TimeFactor + LatestTime;
              StatStr := FloatToStrF(Stat, DecimalDisplayMode, 8, DivTimeDecimals);

              if w > (Node[i].position.x - (Canvas.TextWidth(StatStr)*4 +BranchPen.Width*2 +10)) then
                  w := Node[i].position.x -(Canvas.TextWidth(StatStr)*4 +BranchPen.Width*2 +10);
            end;
            if (xbase + w) < 40 then
              xbase := 40 - w;
        end
        else if ShowDataCoverage then
        begin
            w := 0;
            Canvas.Font.Assign(TimesFont);
            for i := NoOfOTUs+1 to NoOfNodes do
            begin
              if Node[i].hidden then Continue;
              if Node[i] = Root then
                Continue
              else if Node[i] = Root.des1 then begin
                if Root.des2.OTU or (Root.des2.position.x > Node[i].position.x) then Continue;
              end
              else if Node[i] = Root.des2 then begin
                if Root.des1.OTU or (Root.des1.position.x > Node[i].position.x) then Continue;
              end;
              Stat := Node[i].DataCoverage;
              StatStr := FloatToStrF(Stat, DecimalDisplayMode, 8, DivTimeDecimals);

              if w > (Node[i].position.x - (Canvas.TextWidth(StatStr)*4 +BranchPen.Width*2 +10)) then
                  w := Node[i].position.x -(Canvas.TextWidth(StatStr)*4 +BranchPen.Width*2 +10);
            end;
            if ((xbase + w) < 40) and (w > 0) then
              xbase := 40 - w;
        end;

        if isLinearized and (not Topoflag) and ShowHeightErrbar and assigned(HeightErrBarProc) then
          for i := NoOfOTUs+1 to NoOfNodes do
          begin
            if Node[i] = Root then continue;
            if Node[i].position.X = Node[i].anc.position.X then continue;
            HeightErrBarProc(Node[i].index, d0,d1);
            w := Node[i].position.X - round(d0*xunit);
            if w < 0 then
              xbase := math.max(xbase, round(d0*xunit))
            else if ((xbase + w) < 40) and (w > 0) then
              xbase := 40 -w;
            w := Node[i].position.X +round(d1*xunit) +10;
            if w > max.x then
              max.x := w;
          end;

        ybase := Abs(OTU_Font.Height);
        if isStats then
        begin
            for i := NoOfOTUs+1 to NoOfNodes do
            begin
                if Node[i] = Root then Continue;
                if Node[i].hidden then Continue;
                h := Node[i].position.y +Abs(StatsFont.Height) +StatsMargin.Y +2;
                if h > max.y then max.y := h;
                h := Node[i].position.y -Abs(StatsFont.Height) -StatsMargin.Y -2;
                if h < 0  then
                    if ybase < Abs(h) then
                        ybase := Abs(h);
            end;
        end
        else if isTimes then
        begin
          for i := NoOfOTUs+1 to NoOfNodes do
          begin
              if Node[i] = Root then Continue;
              if Node[i].hidden then Continue;
              h := Node[i].position.y +Abs(TimesFont.Height) + TimesMargin.Y +2;
              if h > max.y then max.y := h;
              h := Node[i].position.y -Abs(TimesFont.Height) - TimesMargin.Y -2;
              if h < 0  then
                  if ybase < Abs(h) then
                      ybase := Abs(h);
          end;
        end;

        if isBranchLength and ShowBLen then
        begin
          if ybase < (Abs(BLensFont.Height) + 5) then
            ybase := (Abs(BLensFont.Height) + 5);
        end;
        Inc(ybase, 5);

        max.x := max.x +xbase +40;

        if IsLinearized then
            if ShowScale and IsTimeScale and ShowTimeScale then
                max.y := max.y +ybase*1 +yunit +Abs(ScaleFont.Height)*10 +16
            else if ShowScale or (IsTimeScale and ShowTimeScale) then
                max.y := max.y +ybase*1 +yunit +Abs(ScaleFont.Height)*10 +16
            else
                max.y := max.y +ybase*1
        else if ShowSamplingTimeScale and (not Topoflag) then
            max.y := max.y + ybase*1 + yunit  + Abs(ScaleFont.Height)*10 + 16
        else if IsScale and ShowScale and (not Topoflag) then
            max.y := max.y +ybase*1 +yunit +Abs(ScaleFont.Height)*6 +16
        else
            max.y := max.y +ybase*1 +16;

        if max.Y >= MAX_BITMAP_DIMENSION then
        begin
          SetPixelsPerOTU(math.max(trunc((yunit-(max.Y-MAX_BITMAP_DIMENSION)/NoOfOTUs)/4), 1));
          SetPosition;
        end;
      until (yunit = 1) or (max.Y < MAX_BITMAP_DIMENSION);  // Because of the upper limit of MinHeight is 32767
    end;
    FMinWidth := max.X div 4;
    FMinHeight := max.Y div 4;

    if AutoSize then
    begin
      if ShowNodeIds then
        tempW := max.X + Canvas.TextWidth('(100000)')
      else
        tempW := max.X;
      Width := Math.min(Screen.WorkAreaWidth, tempW);
      //Height := max.Y;
      tempScrollLeft := ScrollLeft;
      tempScrollTop := ScrollTop;
      SetScrollBounds(0, 0, max.X, max.Y);
      if (tempScrollLeft > 0) or (tempScrollTop <> 0) then
        ScrollTo(tempScrollLeft, tempScrollTop);
    end;
    FmaxTreeWidth:=max.x;
    FmaxTreeHeight:=max.y;
end;

procedure TTreeCustomControl.BuildTree;
begin
    if (not TreeExist) or SkipDrawing then
      Exit;
    SetPosition;
    SetTreeSize;
    if IsScale and ShowScale then
      if Scale < 0.0000000000001 then
        InitScale;
end;

procedure TTreeCustomControl.BuildNoDraw;
begin
  if (not TreeExist) or SkipDrawing then
    Exit;
  SetPosition;
  SetTreeSize;
  if IsScale and ShowScale then
    if Scale < 0.0000000000001 then
      InitScale;
end;

procedure TTreeCustomControl.Refresh;
var
  StartTime: TDateTime;
begin
  if (not TreeExist) or SkipDrawing then
    Exit;
  try
    if (not IsResizing) and Assigned(BeginRefreshCallback) then
      BeginRefreshCallback;
    Governer := Max(1, Round(NoOfNodes / 100)); { will result in a maximum of 100 progress updates}
    StartTime := Time;
    BuildTree;
    LoadFromCanvas(Canvas, Rect(0, 0, Width, Height));
    if (not IsResizing) and Assigned(StatusCallback) then
      StatusCallback(rpIdle);
  finally
    if (not IsResizing) and Assigned(EndRefreshCallback) then
      EndRefreshCallback;
    Invalidate;
  end;
end;

function TTreeCustomControl.FindEarliestAndLatestDates(var earliestDate: String; var latestDate: String): Boolean;
begin
  Result := False;
  if (not NodeFocused) and (not BranchFocused) then
    Exit;
  Result := GetEarliestAndLatestDates(Node[FocusedIndex], earliestDate, latestDate);
end;

function TTreeCustomControl.FindEarliestAndLatestDates(var earliestDate: Double; var latestDate: Double): Boolean;
begin
  Result := GetEarliestAndLatestDates(Root, earliestDate, latestDate);
end;

function TTreeCustomControl.FocusedNodeHeight: Extended;
begin
  if NodeFocused then
    Result := Node[FocusedIndex].height
  else
    Result := 0.0;
end;

function TTreeCustomControl.NodeHeight(index: Integer): Extended;
begin
  if (index >= 1) and (index < NoOfNodes) then
    Result := Node[index].height
  else
    Result := 0.0;
end;

function TTreeCustomControl.StrLength(TheStr: AnsiString; TheFont: TFont): Integer;
var
  BM: TBitmap;
begin
  Result := 0;
  BM := TBitmap.Create;
  BM.Canvas.Font := TheFont;
  Result := BM.Canvas.TextWidth(TheStr);
  BM.Free;
end;

function TTreeCustomControl.StrHeight(TheStr: AnsiString; TheFont: TFont): Integer;
var
  BM: TBitmap;
begin
  Result := 0;
  BM := TBitmap.Create;
  BM.Canvas.Font := TheFont;
  Result := BM.Canvas.TextHeight(TheStr);
  BM.Free;
end;

function TTreeCustomControl.GetTaxaDisplayInfoList(aNode: TpNode): TNodeDisplayInfoList;

  procedure ProcessNode(n: TpNode);
  var
    aInfo: TNodeDisplayInfo = nil;
  begin
    if Assigned(n.des1) then
      ProcessNode(n.des1);
    if Assigned(n.des2) then
      ProcessNode(n.des2);
    if n.OTU then
    begin
      aInfo := TNodeDisplayInfo.Create(OTUName[n.index], n.index, Result.Count + 1);
      Result.Add(aInfo);
    end;
  end;

begin
  Result := TNodeDisplayInfoList.Create;
  ProcessNode(aNode);
end;

function TTreeCustomControl.GetInternalNodeDisplayInfoList(aNode: TpNode): TNodeDisplayInfoList;

  procedure ProcessNode(n: TpNode);
  var
    aInfo: TNodeDisplayInfo = nil;
  begin
    if Assigned(n.des1) then
      ProcessNode(n.des1);
    if Assigned(n.des2) then
      ProcessNode(n.des2);
    if not n.OTU then
    begin
      aInfo := TNodeDisplayInfo.Create(n.name, n.index, Result.Count + 1);
      Result.Add(aInfo);
    end;
  end;

begin
  Result := TNodeDisplayInfoList.Create;
  ProcessNode(aNode);
end;

function TTreeCustomControl.DoSearch(taxaInfo: TNodeDisplayInfoList; const Query: AnsiString; const Contains: Boolean; const HighlightAll: Boolean; const QueryChanged: Boolean; var focusedResultIndex: Integer): Integer;
var
  numMatches: Integer = -1;
  aInfo: TNodeDisplayInfo = nil;
  aNodeCoords: TRect;
  i: Integer = -1;
  FocusOnIndex: Integer = -1;
  clientX: Integer = 0;
  clientY: Integer = 0;

  procedure ClearHighlights;
  var
    j: Integer = -1;
  begin
    for j := 1 to NoOfOTUs do
      SetCustomHilightColorOTUName(j, False, clWhite);
    if ShowNodeLabels then
      for j := NoOfOTUs + 1 to NoOfNodes do
        SetCustomHilightColorOTUName(j, False, clWhite);
  end;

begin
  Result := -1;
  if Query = EmptyStr then
  begin
    ClearHighlights;
    Invalidate;
    Exit;
  end;

  if QueryChanged then
  begin
    ClearHighlights;
    numMatches := FTaxaNameSearch.NewSearch(Query, taxaInfo, not Contains);
    if numMatches > 0 then
      FocusOnIndex := FTaxaNameSearch.CurrentMatchNodeId;
  end
  else
  begin
    numMatches := FTaxaNameSearch.NumMatches;
    if FTaxaNameSearch.HasNext then
      FocusOnIndex := FTaxaNameSearch.AdvanceToNext
    else
    begin
      Result := -1;
      ShowMessage(Format('No more matches found for "%s" among tip labels', [Query]));
      Exit;
    end;
  end;

  Result := numMatches;
  ClearFocus;
  if Query = EmptyStr then
  begin
    Invalidate;
    Exit;
  end;

  if HighlightAll then
  begin
    for i := 1 to FTaxaNameSearch.NumNodes do
    begin
      aInfo := FTaxaNameSearch[i - 1];
      if aInfo.IsMatch then
        SetCustomHilightColorOTUName(aInfo.NodeIndex, True, clMagenta);
    end;
  end;

  if numMatches > 0 then
  begin
    focusedResultIndex := FTaxaNameSearch.CurrentMatchNumber;
    Assert(FocusOnIndex <> -1);
    aNodeCoords := NodeCoords(FocusOnIndex);
    FocusOnNode(FocusOnIndex);
    if not NodeIsInViewableArea(FocusOnIndex) then
    begin
      ClientToBox(aNodeCoords.Left, aNodeCoords.Top, clientX, clientY);
      ScrollToYCoord(clientY - Height div 2);
    end;
    FFocusedNameIndex := FocusOnIndex;
  end
  else
    ShowMessage(Format('Search query "%s" not found among tip labels', [Query]));

  if @FOnSearch <> nil then
    FOnSearch(Self);
  Invalidate;
end;

function TTreeCustomControl.SearchTipNames(const Query: AnsiString; const Contains: Boolean; const HighlightAll: Boolean; const QueryChanged: Boolean; var focusedResultIndex: Integer): Integer;
var
  taxaInfo: TNodeDisplayInfoList = nil;
  i: Integer = -1;
begin
  try
    taxaInfo := GetTaxaDisplayInfoList(Root); { this list has taxa ordered the same as they appear in the tree, not the original alignment - needed because rooting the tree, swapping branches, etc... reorders how taxa appear in the tree display}
    Assert(taxaInfo.Count = NoOfOTUs);
    Result := DoSearch(taxaInfo, Query, Contains, HighlightAll, QueryChanged, focusedResultIndex);
  finally
    if Assigned(taxaInfo) then
    begin
      for i := 0 to taxaInfo.Count - 1 do
        taxaInfo[i].Free;
      taxaInfo.Free;
    end;
  end;
end;

procedure TTreeCustomControl.DrawTree;
{$IFDEF DARWIN}
var
  i, j:Integer;
{$ENDIF}
begin
  if SkipDrawing or FIsDrawing then
    Exit;
  try
    FIsDrawing := True;
    {$IFDEF DARWIN}
    SetLength(FSavedLines, FmaxTreeWidth);
    for i:=0 to Length(FSavedLines)-1 do
    begin
      SetLength(FSavedLines[i], FmaxTreeHeight);
      for j:=0 to Length(FSavedLines[i])-1 do
        FSavedLines[i, j] := False;
    end;
    {$ENDIF}
    if not FDrawFullTree then
      UpdatePaintingAreaCoords;
    FMaxOTUPos := 0;
    FLastOtuCoords := Rect(0, 0, 0, 0);
    FNewOtuCoords := Rect(0, 0, 0, 0);
    FTempRect := Rect(0, 0, 0, 0);
    FNodesProcessed := 0;
    StatusCallback(rpDrawBranches);
    DrawBackground;
    DrawBranches;

    if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then
      DrawMarkerAngle
    else
    begin
      DrawMarker;
      DrawGeneDupMarker; { doing these separately because the group attributes can mess things up}
    end;

    if ShowDivergenceTimes then
    begin
      //StatusCallback(rpDrawDivTimes);
      DrawDivergenceTimes;
    end
    else if ShowCalibrationTimes then
      DrawCalibrations;

    if ShowNodeLabels and (TreeStyle = tsTraditional) then
      DrawNodeLabels
    else if ShowNodeIds and (TreeStyle = tsTraditional) then
    begin
      //StatusCallback(rpDrawNodeIds);
      DrawNodeIds;
    end
    else if ShowDataCoverage then
    begin
      //StatusCallback(rpDrawDataCoverage);
      DrawDataCoverage;
    end
    else if isStats and (ShowStats or ShowStatsRange) and (TreeStyle = tsTraditional) then
    begin
      //StatusCallback(rpDrawStats);
      DrawStat;
    end;

    if (not isCondensed) and isBranchLength and ShowBLen and (TreeStyle = tsTraditional) then
    begin
      //StatusCallback(rpDrawBranchLengths);
      DrawBranchLength;
    end;

    if ShowCharState then
    begin
      //StatusCallback(rpDrawCharState);
      DrawCharState;
    end;
    //StatusCallback(rpDrawScale);
    if TreeStyle = tsCircle then
        DrawScaleCircle
    else
        DrawScale;
    if FAlignOtuNames then
      DrawExtendedLinesToOtuNames;
    StatusCallback(rpIdle);
  finally
    FIsDrawing := False;
  end;
end;

procedure TTreeCustomControl.DrawTree(aCanvas: TCanvas);
var
  origCanvas: TCanvas;
begin
  if SkipDrawing then
    Exit;
  try
    Assert(FTreeDrawingTarget = tdtCanvas, 'Drawing to an external canvas is only supported for default drawing');
    origCanvas := Canvas;
    FRenderer.Canvas := aCanvas;
    Canvas := aCanvas;
    ScrollTo(0, 0);
    Invalidate;
    DrawTree;
  finally
    Canvas := origCanvas;
    FRenderer.Canvas := origCanvas;
  end;
end;

procedure TTreeCustomControl.GetCoordsForNodeInfo(aNode: TpNode; aText: String; var x: Integer; var y: Integer);
var
  textsize, dh, dv : integer;
begin
  with aNode^ do
  begin
    textsize := Canvas.TextWidth(aText);
    if StatsPosition = bipAboveBranch then
      dh := VertLineWidth(des1)
    else if StatsPosition = bipBelowBranch then
      dh := VertLineWidth(des2)
    else if aNode = anc.des1 then
      dh := VertLineWidth(des1)
    else
      dh := VertLineWidth(des2);
    if StatsMargin.X < -(textsize + dh) then
        dh := -(textsize + dh)
    else if StatsMargin.X > (position.x -anc.position.x +dh) then
        dh := position.x -anc.position.x +dh
    else
        dh := StatsMargin.X +dh;
    x := position.x -textsize -dh;

    dv := StatsMargin.Y +GetLineWidth(aNode);
    if (StatsPosition = bipAboveBranch)
    or ((StatsPosition = bipAutomatic) and (ShowBLen and IsBranchLength)) then
        y := position.y - Abs(StatsFont.Height) - dv
    else if StatsPosition = bipBelowBranch then
        y := position.y + dv
    else
        if position.y <= anc.position.y then
            y := position.y - Abs(StatsFont.Height) - dv
        else
            y := position.y + dv;
  end;
end;

procedure TTreeCustomControl.GetCoordsForDivTime(aNode: TpNode; aText: String; var x: Integer; var y: Integer);
var
  dh, dv, dy : integer;
begin
  with aNode^ do
  begin
    { factoring in xbase, ybase in calculations should be done by default }
    if OTU then Exit;
    dh := VertLineWidth(des2)*2;
    if ShowCalibratedNodeMarker then
      dh := dh + Round(Abs(GetNodeAttrib(aNode).Font.Height)/2);
    x := position.x + dh + xbase;
    dv := Round(Abs(TimesFont.Height) / 2);
    if (IsLinearized or IsSamplingTimes) and (not Topoflag) and (not aNode.outgroup) and ShowHeightErrBar then
    begin
      dy := FCurAttrib.LineWidth*3;
      if dy > yunit then
        dy := yunit;
      dy := dy*2;
      y := position.y + ybase + dy;
    end
    else
      y := position.y + ybase - dv;
  end;
end;

function TTreeCustomControl.GetBestBLenPosition(aNode: TpNode; aText: String): TPoint;
var
  textsize, d, y: Integer;
  r: Double;
begin
  with aNode^ do
  begin
    textsize := Canvas.TextWidth(aText);
    d := GetLineWidth(aNode)*2 +3*Abs(BLensFont.Height) div 2;
    Result.x := position.x - textsize - ((position.x - anc.position.x - textsize) div 2);
    if (BranchStyle = bsRectangular) or (BranchStyle = bsCurved) or (BranchStyle = bsStraight) then
    begin
      if BLenDisplayedInsideBranch(aNode) then
        Result.x := max(Result.X, aNode.anc.position.x + 4)
      else if not aNode.OTU then
        Result.x := min(Result.X, position.x - textsize - 4);
    end;


    if (BranchStyle = bsStraight) and (Result.x > anc.position.x) then
    begin
      if (position.x - anc.position.x) <> 0 then
        r := (position.x - Result.x)/(position.x - anc.position.x)
      else
        r := 0;
      if (BLenPosition = bipBelowBranch) or
         ((BLenPosition = bipAutomatic) and ((isStats and ShowStats) or ShowNodeIds or ShowDivergenceTimes)) then
      begin
        if aNode = anc.des1 then
          y := position.y +Round((anc.position.y -position.y)*r) + Abs(BLensFont.Height)*5 +d
        else
            y := anc.position.y -Round((anc.position.y -position.y)*r) +Abs(BLensFont.Height)*5 +d;
      end
      else if BLenPosition = bipAboveBranch then
        if aNode = anc.des1 then
          y := anc.position.y -Round((anc.position.y -position.y)*r) -d
        else
          y := position.y +Round((anc.position.y -position.y)*r) -d
      else if aNode = anc.des1 then
        y := anc.position.y -Round((anc.position.y -position.y)*r) -d
      else if aNode = anc.des2 then
        if OTU and (anc.anc <> nil) then
          y := position.y +Round((anc.position.y -position.y)*r) -d
        else if anc.position.y = position.y then
          y := anc.position.y + d div 4
        else
          y := anc.position.y -Round((anc.position.y -position.y)*r) + d
    end
    else if (BLenPosition = bipBelowBranch) or
            ((BLenPosition = bipAutomatic) and ((isStats and ShowStats) or ShowNodeIds or ShowDivergenceTimes or ShowDataCoverage)) then
      y := position.y +d div 4
    else if BLenPosition = bipAboveBranch then
      y := position.y -d
    else if aNode = anc.des1 then
      y := position.y -d
    else if aNode = anc.des2 then
      if OTU and (anc.anc <> nil) then
        y := position.y -d
      else
        y := position.y +Abs(BLensFont.Height) +d div 4;
    end;
  Result.X := Result.X + xbase;
  Result.y := y + ybase;
end;

function TTreeCustomControl.BLenDisplayedInsideBranch(aNode: TpNode): Boolean;
begin
  Result := False;
  if (not Assigned(aNode.anc)) or (aNode <> aNode.anc.des2) then
    Exit;
  Result := (BLenPosition = bipAboveBranch);
  if not Result then
    Result := not ((BLenPosition = bipAutomatic) and ((isStats and ShowStats) or ShowNodeIds or ShowDivergenceTimes));
end;

function TTreeCustomControl.DistanceBetweenNodes(node1: TpNode; node2: TpNode): Double;
var
  point1, point2: TPoint;
begin
  point1.x := node1.position.x;
  point1.y := node1.position.y;
  point2.x := node2.position.x;
  point2.y := node2.position.y;
  Result := DistanceFormula(point1, point2);
end;

procedure TTreeCustomControl.RotateText(R: TRect; aText: String; RotateType: TRotateType; Orientation: Integer=0);
var
  TextExtent: TSize;
  OrigOrientation: Integer;
begin
  TextExtent := Canvas.TextExtent(AText);
  OrigOrientation := Canvas.Font.Orientation;
  case RotateType of
    rtNone:
    begin
      Canvas.Font.Orientation := 0;
      MTextOut((R.Right - R.Left - TextExtent.cx) div 2, (R.Bottom - R.Left - TextExtent.cy) div 2, AText);
    end;
    rtCounterClockWise:
    begin
      Canvas.Font.Orientation := 900;
      MTextOut((R.Right - R.Left - TextExtent.cy) div 2, (R.Bottom - R.Left + TextExtent.cx) div 2, AText);
    end;
    rtFlip:
    begin
      Canvas.Font.Orientation := 1800;
      MTextOut((R.Right - R.Left + TextExtent.cx) div 2,
        (R.Bottom - R.Left + TextExtent.cy) div 2, AText);
    end;
    rtClockWise:
    begin
      Canvas.Font.Orientation := -900;
      MTextOut((R.Right - R.Left + TextExtent.cy) div 2,(R.Bottom - R.Left - TextExtent.cx) div 2, AText);
    end;
    rtCustom:
    begin
      Canvas.Font.Orientation := Orientation;
      MTextOut(R.Left, R.Top, aText, Orientation);
    end;
  end;
  Canvas.Font.Orientation := OrigOrientation;
end;

procedure TTreeCustomControl.DrawBackground;
var
  aRect: TRect;
begin
  aRect := Rect(0, 0, ClientWidth, ClientHeight);
  if ScrollWidth > aRect.Right then
    aRect.Right := ScrollWidth;
  if ScrollHeight > aRect.Bottom then
    aRect.Bottom := ScrollHeight;
  Canvas.Brush := FOpenBrush;
  Canvas.FillRect(aRect);
end;

procedure TTreeCustomControl.DrawFocus;
var
  x0,y0,x1,y1,x2,x3,y3,x4,y4,d,dx,dy,n,i : integer;
  r: double;
  q1,q2: TpNode;
  p: array[0..45] of TPoint;
  clientCoords: array[0..45] of TPoint;
  aRect: TRect;
begin
  if FTreeDrawingTarget <> tdtCanvas then Exit;
  if not TreeExist then Exit;
  if FocusedIndex = 0 then Exit;
  if Node[FocusedIndex].hidden then Exit;

  if (Length(FCalibratedNodes) > 0) and (FShowDivergenceTimes or ShowCalibratedNodeMarker) then
    DrawCalibratedNodeMarkers;

  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := clLime;
  Canvas.Brush.Color := clLime;

  if IsGeneDups and NodeFocused and ((Node[FocusedIndex].IsGeneDuplication) or (Node[FocusedIndex].IsSpeciationEvent))then
    Canvas.Pen.Width := 3
  else
    Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;
  {$IFNDEF DARWIN}
  Canvas.Pen.Mode := pmNotXor;
  {$ELSE}
  Canvas.Brush.Style := bsClear;
  {$ENDIF}

  d := BranchPen.Width div 2 + 7;
  if CompareValue(ScalingFactor, 1, FP_CUTOFF) > 0 then
  begin
    d := Round(d*ScalingFactor);
    Canvas.Pen.Width := Round(Canvas.Pen.Width*ScalingFactor);
  end;

  {$IFDEF DARWIN}
  Canvas.Pen.Width := 3;
  {$ENDIF}

  if NodeFocused then
  begin
    p[0].x := (Node[FocusedIndex].position.x +xbase);
    p[0].y := (Node[FocusedIndex].position.y +ybase) -d -1;
    p[1].x := (Node[FocusedIndex].position.x +xbase) +d +1;
    p[1].y := (Node[FocusedIndex].position.y +ybase);
    p[2].x := (Node[FocusedIndex].position.x +xbase);
    p[2].y := (Node[FocusedIndex].position.y +ybase) +d +1;
    p[3].x := (Node[FocusedIndex].position.x +xbase) -d -1;
    p[3].y := (Node[FocusedIndex].position.y +ybase);
    ConvertToClientCoords(p, clientCoords, 4);
    MPolygon(Slice(clientCoords,4));
  end
  else if BranchFocused then
  begin
    {$IFDEF DARWIN}
    d := d - Canvas.Pen.Width;
    {$ENDIF}
    if TreeStyle = tsCircle then
      if Node[FocusedIndex] = Root then
        if (isRooted and ShowRoot) then begin
          x0 := Round((Root.position.x -round(cos(Root.angle)*100) +xbase)/4);
          y0 := Round((Root.position.y +round(sin(Root.angle)*100) +ybase)/4);
          x1 := Round((Root.position.x +xbase)/4);
          y1 := Round((Root.position.y +ybase)/4);
          dx := Round(d*(y1-y0)/sqrt((x1-x0)*(x1-x0)+(y1-y0)*(y1-y0)));
          dy := Round(d*(x1-x0)/sqrt((x1-x0)*(x1-x0)+(y1-y0)*(y1-y0)));

          p[0].x := x0 +dx -dy;
          p[0].y := y0 -dy -dx;
          p[1].x := x0 -dx -dy;
          p[1].y := y0 +dy -dx;
          p[2].x := x1 -dx +dy;
          p[2].y := y1 +dy +dx;
          p[3].x := x1 +dx +dy;
          p[3].y := y1 -dy +dx;
          ConvertToClientCoords(p, clientCoords, 4);
          MPolygon(Slice(clientCoords, 4));
        end
        else begin
          q1 := Root.des1;
          q2 := Root.des2;
            while (not q1.OTU) and (q1.avglen = Root.avglen) do
              q1 := q1.des1;
            while (not q2.OTU) and (q2.avglen = Root.avglen) do
              q2 := q2.des2;
          p[0].x := round((Origin.X -Root.avglen +xbase +4)/4) -d;
          p[0].y := round((Origin.Y -Root.avglen +ybase +4)/4) -d;
          p[1].x := round((Origin.X +Root.avglen +xbase +4)/4) +d;
          p[1].y := round((Origin.Y +Root.avglen +ybase +4)/4) +d;
          p[2].x := round((Origin.x +cos(q2.angle)*(Root.avglen +4*d) +xbase)/4);
          p[2].y := round((Origin.y -sin(q2.angle)*(Root.avglen +4*d) +ybase)/4);
          p[3].x := round((Origin.x +cos(q1.angle)*(Root.avglen +4*d) +xbase)/4);
          p[3].y := round((Origin.y -sin(q1.angle)*(Root.avglen +4*d) +ybase)/4);
          ConvertToClientCoords(p, clientCoords, 4);
          Canvas.Arc(clientCoords[0].x, clientCoords[0].y, clientCoords[1].x, clientCoords[1].y, clientCoords[2].x, clientCoords[2].y, clientCoords[3].x, clientCoords[3].y);
          p[0].x := round((Origin.X -Root.avglen +xbase)/4) +d;
          p[0].y := round((Origin.Y -Root.avglen +ybase)/4) +d;
          p[1].x := round((Origin.X +Root.avglen +xbase +4)/4) -d;
          p[1].y := round((Origin.Y +Root.avglen +ybase +4)/4) -d;
          p[2].x := round((Origin.x +cos(q2.angle)*(Root.avglen -4*d) +xbase)/4);
          p[2].y := round((Origin.y -sin(q2.angle)*(Root.avglen -4*d) +ybase)/4);
          p[3].x := round((Origin.x +cos(q1.angle)*(Root.avglen -4*d) +xbase)/4);
          p[3].y := round((Origin.y -sin(q1.angle)*(Root.avglen -4*d) +ybase)/4);
          ConvertToClientCoords(p, clientCoords, 4);
          Canvas.Arc(clientCoords[0].x, clientCoords[0].y, clientCoords[1].x, clientCoords[1].y, clientCoords[2].x, clientCoords[2].y, clientCoords[3].x, clientCoords[3].y);
          x0 := round((Origin.x +cos(q1.angle)*(Root.avglen) +xbase)/4);
          y0 := round((Origin.y -sin(q1.angle)*(Root.avglen) +ybase)/4);
          dx := Round(d*cos(q1.angle));
          dy := Round(d*sin(q1.angle));
          p[0].x := x0 +dx;
          p[0].y := y0 -dy;
          p[1].x := x0 +dx -dy;
          p[1].y := y0 -dy -dx;
          p[2].x := x0 -dx -dy;
          p[2].y := y0 +dy -dx;
          p[3].x := x0 -dx;
          p[3].y := y0 +dy;
          ConvertToClientCoords(p, clientCoords, 4);
          MPolyline(Slice(clientCoords, 4));
          x0 := round((Origin.x +cos(q2.angle)*(Root.avglen) +xbase)/4);
          y0 := round((Origin.y -sin(q2.angle)*(Root.avglen) +ybase)/4);
          dx := Round(d*cos(q2.angle));
          dy := Round(d*sin(q2.angle));
          p[0].x := x0 +dx;
          p[0].y := y0 -dy;
          p[1].x := x0 +dx +dy;
          p[1].y := y0 -dy +dx;
          p[2].x := x0 -dx +dy;
          p[2].y := y0 +dy +dx;
          p[3].x := x0 -dx;
          p[3].y := y0 +dy;
          ConvertToClientCoords(p, clientCoords, 4);
          MPolyline(Slice(clientCoords,4));
        end
      else
        with Node[FocusedIndex]^ do
        begin
          x0 := Round((position.x -round(cos(angle)*(avglen-anc.avglen)) +xbase));
          y0 := Round((position.y +round(sin(angle)*(avglen-anc.avglen)) +ybase));
          x1 := Round((position.x +xbase));
          y1 := Round((position.y +ybase));
          if (x1=x0) and (y1=y0) then
          begin
            dx := round(cos(angle)*d);
            dy := round(sin(angle)*d);
          end
          else
          begin
            dx := Round(d*(y1-y0)/sqrt((x1-x0)*(x1-x0)+(y1-y0)*(y1-y0)));
            dy := Round(d*(x1-x0)/sqrt((x1-x0)*(x1-x0)+(y1-y0)*(y1-y0)));
          end;

          p[0].x := x0 +dx -dy;
          p[0].y := y0 -dy -dx;
          p[1].x := x0 -dx -dy;
          p[1].y := y0 +dy -dx;
          p[2].x := x1 -dx +dy;
          p[2].y := y1 +dy +dx;
          p[3].x := x1 +dx +dy;
          p[3].y := y1 -dy +dx;
          ConvertToClientCoords(p, clientCoords, 4);
          MPolygon(Slice(clientCoords,4));
        end
    else { end circle tree}
      if Node[FocusedIndex] = Root then
      begin
        if (isRooted and ShowRoot) then begin
            p[0].x := Round((xbase - LENGTH_OF_ROOT_BRANCH)) -d;
            p[0].y := Root.position.y +ybase -d;
            p[1].x := p[0].x;
            p[1].y := Root.position.y +ybase +d;
            p[2].x := Root.position.x +xbase +d;
            p[2].y := p[1].y;
            p[3].x := p[2].x;
            p[3].y := p[0].y;
        end
        else if (TreeStyle = tsTraditional) and (BranchStyle = bsRectangular) then
        begin
            q1 := Root.des1;
            q2 := Root.des2;
              while (not q1.OTU) and (q1.position.x = Root.position.x) do
                q1 := q1.des1;
              while (not q2.OTU) and (q2.position.x = Root.position.x) do
                q2 := q2.des2;
            p[0].x := Root.position.x +xbase -d;
            p[0].y := q1.position.y +ybase -d;
            p[1].x := p[0].x;
            p[1].y := q2.position.y +ybase +d;
            p[2].x := Root.position.x +xbase +d;
            p[2].y := p[1].y;
            p[3].x := p[2].x;
            p[3].y := p[0].y;
        end
        else
        begin
            p[0].x := Root.position.x +xbase -d;
            p[0].y := Root.position.y +ybase -d;
            p[1].x := p[0].x;
            p[1].y := Root.position.y +ybase +d;
            p[2].x := Root.position.x +xbase +d;
            p[2].y := p[1].y;
            p[3].x := p[2].x;
            p[3].y := p[0].y;
        end;
        ConvertToClientCoords(p, clientCoords, 4);
        MPolygon(Slice(clientCoords,4));
      end
      else
        with Node[FocusedIndex]^ do
        begin
            if (TreeStyle = tsTraditional) and (BranchStyle = bsRectangular) then begin
                p[0].x := anc.position.x +xbase -d;
                p[0].y := position.y +ybase -d;
                p[1].x := p[0].x;
                p[1].y := position.y +ybase +d;
                p[2].x := position.x +xbase +d;
                p[2].y := p[1].y;
                p[3].x := p[2].x;
                p[3].y := p[0].y;
                ConvertToClientCoords(p, clientCoords, 4);
                aRect := Rect(p[0].x, p[0].y, p[2].x, p[1].y);
                //MRoundRectangle(aRect, 2, 2);
                MPolygon(Slice(clientCoords,4));
            end
            else if (TreeStyle = tsTraditional) and (BranchStyle = bsCurved) then
            begin
              if Topoflag then
              begin
                q1 := Node[FocusedIndex];
                if q1 = q1.anc.des1 then
                  if q1.position.y < q1.anc.position.y then
                    while (q1 <> Root) and (q1.position.y <= q1.anc.position.y) do
                    begin
                      if q1 = q1.anc.des2 then
                      begin
                        q1 := q1.anc;
                        Break;
                      end;
                      q1 := q1.anc;
                    end
                  else
                    q1 := q1.anc
                else
                  if q1.position.y > q1.anc.position.y then
                    while (q1 <> Root) and (q1.position.y >= q1.anc.position.y) do
                    begin
                      if q1 = q1.anc.des1 then
                      begin
                        q1 := q1.anc;
                        Break;
                      end;
                      q1 := q1.anc;
                    end
                  else
                    q1 := q1.anc;
                q2 := Node[FocusedIndex];
                if q2.position.y < q1.position.y then
                  while not (q2.OTU or q2.compressed) do q2 := q2.des1
                else
                  while not (q2.OTU or q2.compressed) do q2 := q2.des2;
              end
              else begin
                q1 := Node[FocusedIndex].anc;
                q2 := Node[FocusedIndex];
              end;
              x0 := q1.position.x +xbase;
              y0 := q1.position.y +ybase;
              y1 := q2.position.y +ybase;
              x1 := q2.position.x +xbase;
              x3 := Node[FocusedIndex].anc.position.x +xbase;
              y3 := Node[FocusedIndex].anc.position.y +ybase;
              x4 := Node[FocusedIndex].position.x +xbase;
              y4 := Node[FocusedIndex].position.y +ybase;
              x2 := round((2*q1.position.x +q2.position.x +3*xbase)/3);
              if (Abs(y3-y4) < d) or ((x4-x3) < d) then
              begin
                  dx := Round(d*(y4-y3)/sqrt((x4-x3)*(x4-x3)+(y4-y3)*(y4-y3)));
                  dy := Round(d*(x4-x3)/sqrt((x4-x3)*(x4-x3)+(y4-y3)*(y4-y3)));
                  p[0].x := x3 +dx -dy;
                  p[0].y := y3 -dy -dx;
                  p[1].x := x3 -dx -dy;
                  p[1].y := y3 +dy -dx;
                  p[2].x := x4 -dx +dy;
                  p[2].y := y4 +dy +dx;
                  p[3].x := x4 +dx +dy;
                  p[3].y := y4 -dy +dx;
                  ConvertToClientCoords(p, clientCoords, 4);
                  MPolygon(Slice(clientCoords,4));
              end
              else
              begin
                  dx := Round(d*(y4-y3)/sqrt((x4-x3)*(x4-x3)+(y4-y3)*(y4-y3)));
                  dy := Round(d*(x4-x3)/sqrt((x4-x3)*(x4-x3)+(y4-y3)*(y4-y3)));
                  r := abs(y1-y0)*sqrt(3)/(x1-x0);
                  n := ceil((x4-x3)/25*3);
                  if n < 3 then
                    n := 3
                  else if n > 15 then
                    n := 15;
                  for i := 1 to 3*n-1 do
                    p[i].x := x3 +round((x4-x3)/n/3*i);
                  if y0 > y1 then
                    for i := 1 to 3*n-1 do
                    begin
                      if p[i].x < x2 then
                        p[i].y := round(1/3*y0 +2/3*y1 +sqrt(4/9*(y1-y0)*(y1-y0)-r*r*(p[i].x-x0)*(p[i].x-x0)))
                      else
                        p[i].y := round(4/3*y0 -1/3*y1 -sqrt(16/9*(y1-y0)*(y1-y0) -r*r*(p[i].x-x1)*(p[i].x-x1)));
                     end
                  else
                    for i := 1 to 3*n-1 do
                    if p[i].x < x2 then
                      p[i].y := round(1/3*y0 +2/3*y1 -sqrt(4/9*(y1-y0)*(y1-y0)-r*r*(p[i].x-x0)*(p[i].x-x0)))
                    else
                      p[i].y := round(4/3*y0 -1/3*y1 +sqrt(16/9*(y1-y0)*(y1-y0) -r*r*(p[i].x-x1)*(p[i].x-x1)));
                  p[0].x := x3;
                  p[0].y := y3;
                  p[3*n].x := x4;
                  p[3*n].y := y4;
                  for i := 0 to 3*n do begin
                    p[i].x := p[i].x -dx;
                    p[i].y := p[i].y +dy;
                  end;
                  ConvertToClientCoords(p, clientCoords, 3*n+1);
                  MPolyBezier(clientCoords, 3*n+1, False, True);
                  for i := 0 to 3*n do begin
                    p[i].x := p[i].x +2*dx;
                    p[i].y := p[i].y -2*dy;
                  end;
                  ConvertToClientCoords(p, clientCoords, 3*n+1);
                  MPolyBezier(clientCoords, 3*n+1, False, True);
                  p[0].x := x3 -dx;
                  p[0].y := y3 +dy;
                  p[1].x := x3 +dx;
                  p[1].y := y3 -dy;
                  ConvertToClientCoords(p, clientCoords, 2);
                  MPolyline(Slice(clientCoords,2));
                  p[0].x := x4 -dx;
                  p[0].y := y4 +dy;
                  p[1].x := x4 +dx;
                  p[1].y := y4 -dy;
                  ConvertToClientCoords(p, clientCoords, 2);
                  MPolyline(Slice(clientCoords,2));
              end;
            end
            else
            begin
              x0 := anc.position.x +xbase;
              y0 := anc.position.y +ybase;
              x1 := position.x +xbase;
              y1 := position.y +ybase;
              dx := Round(d*(y1-y0)/sqrt((x1-x0)*(x1-x0)+(y1-y0)*(y1-y0)));
              dy := Round(d*(x1-x0)/sqrt((x1-x0)*(x1-x0)+(y1-y0)*(y1-y0)));

              p[0].x := x0 +dx -dy;
              p[0].y := y0 -dy -dx;
              p[1].x := x0 -dx -dy;
              p[1].y := y0 +dy -dx;
              p[2].x := x1 -dx +dy;
              p[2].y := y1 +dy +dx;
              p[3].x := x1 +dx +dy;
              p[3].y := y1 -dy +dx;
              MPolygon(Slice(p,4));
            end
        end;
  end;
end;

procedure TTreeCustomControl.DrawScale;
begin
  if Topoflag then Exit;
  if (not FShowTimeScale) and (not FShowScale) then Exit;
  UpdatePen(FScalePen, psSolid, FScalePen.Width, ScalePen.Color, pecSquare, pjsMiter);
  Canvas.Pen := FScalePen;
  Canvas.Brush.Style := bsClear;
  if IsLinearized or ForceLinearized or FShowSamplingTimeScale then
      DrawScaleForLinearized
  else
      DrawScaleForNonLinearized;
end;

procedure TTreeCustomControl.DrawScaleForNonLinearized;
var
  dx,dy,x,y: Integer;
  clientX: Integer = -1;
  clientY: Integer = -1;
  baseY: Integer;
  points : array[0..1] of TPoint;
  clientCoords: array[0..1] of TPoint;
  textsize : integer;
  aText: String;
  aTop: Integer = 0;
  aBottom: Integer = 0;
begin
    if not ShowScale then Exit;
    if (isRooted and ShowRoot) then
        dx := 160
    else
        dx := 60;
    dy := 4;
    baseY := ymax + abs(FOTU_Font.Height)*2 + DEFAULT_PPOTU;
    { horizontal line}
    points[0].x := dx;
    points[0].y := baseY;
    points[1].x := Round(Scale*xunit) +dx;
    points[1].y := points[0].y;
    ConvertToClientCoords(points, clientCoords, 2);
    MPolyLine(slice(clientCoords,2));

    { left vertical line}
    points[0].x := dx;
    points[0].y := baseY - dy;
    points[1].x := dx;
    points[1].y := baseY + dy;
    ConvertToClientCoords(points, clientCoords, 2);
    MPolyLine(slice(clientCoords,2));
    aTop := points[0].y;

    { right vertical line}
    points[0].x := Round(Scale*xunit) +dx;
    points[1].x := Round(Scale*xunit) +dx;
    ConvertToClientCoords(points, clientCoords, 2);
    MPolyLine(slice(clientCoords,2));

    Canvas.Font.Assign(ScaleFont);
    if FScaleUnit = '' then
      aText := FScaleText
    else
      aText := FScaleText+' '+FScaleUnit;

    x := Round(Scale*xunit) +2*dx;
    textsize := Canvas.TextWidth(aText);
    x := (x -textsize) DIV 2;
    y := points[1].y + 15;
    aBottom := y + abs(Canvas.Font.Height);
    BoxToClient(x, y, clientX, clientY);
    MTextOut(clientX,clientY,aText);
    FScaleBarHeight := aBottom - aTop;
end;

procedure TTreeCustomControl.DrawScaleForLinearized;
var
  majorTick, minorTick, d : integer;
  text : String;
  leftBound,tickHeight,x,y: Integer;
  rightBound: Integer = -1;
  clientX: Integer = -1;
  clientY: Integer = -1;
  linecoords : array[0..1] of TPoint;
  tickCoords: array[0..1] of TPoint;
  clientCoords: array[0..1] of TPoint;
  textsize : integer;
  offset: Integer = -1;
  aFactor: Double;
  aTimeScale: Double = 0.0;
  linePosY: Integer = -1;
  aTop: Integer = 0;
  aBottom: Integer = 0;

  procedure DoDrawLine;
  begin
    ConvertToClientCoords(linecoords, clientCoords, 2);
    MPolyLine(slice(clientCoords,2));
  end;

  procedure DoDrawTick;
  begin
    ConvertToClientCoords(tickCoords, clientCoords, 2);
    MPolyLine(slice(clientCoords, 2));
  end;

begin
  Assert(FIsUPGMA or (CompareValue(FTimeFactor, 0.0, FP_CUTOFF) <> 0), 'invalid time factor');
  if not (ShowScale or (IsTimeScale and (ShowTimeScale or ShowSamplingTimeScale))) then
    Exit;
  if IsTimeScale then
  begin
    leftBound := Root.des1.position.x + xbase;
    rightBound := leftBound + Round(xmax*xunit) - (Root.des1.position.x - Root.position.x);
  end
  else
  begin
    leftBound := xbase;
    rightBound := leftBound + Round(xmax*xunit);
  end;

  tickHeight := 6;
  Canvas.Font.Assign(ScaleFont);
  if FIsUPGMA then
    aFactor := 1
  else
    aFactor := abs(FTimeFactor);
  linePosY := ymax + ybase + Abs(ScaleFont.Height)*3;
  linecoords[0].x := leftBound;
  linecoords[0].y := linePosY;
  linecoords[1].x := rightBound;
  linecoords[1].y := linePosY;

  tickCoords[0].x := leftBound;
  tickCoords[1].x := leftBound;
  tickCoords[0].y := linePosY;
  tickCoords[1].y := linePosY;
  aTop := tickCoords[0].y;

  DoDrawLine;

  if ShowScale and (Scale > 0.0) then { for UPGMA trees}
  begin
      d := Pos('.', FScaleText);
      if d > 0 then
          d := Length(FScaleText)-d;

      for majorTick := 0 to Trunc(xmax/Scale) do
      begin
          tickCoords[0].x := Round((xmax -Scale*majorTick)*xunit) +leftBound;
          tickCoords[1].x := tickCoords[0].x;
          if IsTimeScale and ShowTimeScale then
              tickCoords[0].y := linecoords[0].y
          else
              tickCoords[0].y := linecoords[0].y -tickHeight;
          tickCoords[1].y := linecoords[0].y +tickHeight;
          DoDrawTick;
          if majorTick = 0 then
            aTop := tickCoords[0].y;
          x := tickCoords[0].x;
          y := tickCoords[1].y;

          if (ScaleTick > 0.0) and (ScaleTick < Scale) then
              for minorTick := 1 to Trunc(Scale/ScaleTick) do
              begin
                  if xmax < (Scale*majorTick+ScaleTick*minorTick) then Break;
                  tickCoords[0].x := Round((xmax -(Scale*majorTick+ScaleTick*minorTick))*xunit) +leftBound;
                  tickCoords[1].x := tickCoords[0].x;
                  if IsTimeScale and ShowTimeScale then
                      tickCoords[0].y := linecoords[0].y
                  else
                      tickCoords[0].y := linecoords[0].y - (tickHeight-4);
                  tickCoords[1].y := linecoords[0].y +(tickHeight-4);
                  DoDrawTick;
              end;

          text := FloatToStrF(Scale*majorTick,DecimalDisplayMode,15, d);
          textsize := Canvas.TextWidth(text);
          x := x - (textsize div 2);
          y := y + 5;
          BoxToClient(x, y, clientX, clientY);
          MTextOut(clientX,clientY,text);
      end;
      text := ScaleUnit;
      offset := textsize;
      x := Round(xmax*xunit) +leftBound + textsize;
      aBottom := y + abs(Canvas.Font.Height);
      BoxToClient(x, y, clientX, clientY);
      MTextOut(clientX,clientY, ScaleUnit);
  end;

  if IsTimeScale and ShowTimeScale then
  begin
    { draw the left tick}
    //tickCoords[0].y := linecoords[0].y - tickHeight;
    //tickCoords[1].y := linecoords[0].y + tickHeight;
    //DoDrawTick;

    { draw the right tick}
    tickCoords[0].x := rightBound;
    tickCoords[1].x := rightBound;
    DoDrawTick;

    aTimeScale := abs(TimeScale);
    d := Pos('.', FTimeText);
    if d > 0 then
        d := Length(FTimeText) - d;

    { draw the time ticks}
    for majorTick := 0 to Trunc(xmax*aFactor/aTimeScale) do
    begin
      tickCoords[0].x := rightBound - Round((aTimeScale*majorTick/aFactor)*xunit);
      tickCoords[1].x := tickCoords[0].x;
      tickCoords[0].y := linecoords[0].y - tickHeight;
      if CompareValue(tickCoords[0].x, leftBound, FP_CUTOFF) < 0 then
        break;
      if ShowScale then
      begin
        tickCoords[1].y := linecoords[0].y;
        y := tickCoords[0].y - Abs(ScaleFont.Height) - 5;
      end
      else
      begin
        tickCoords[1].y := linecoords[0].y + tickHeight;
        y := tickCoords[1].y + 5;
      end;
      if majorTick = 0 then
        aTop := tickCoords[0].y;
      x := tickCoords[0].x;
      DoDrawTick;

      { draw minor ticks if requested}
      if (TimeTick > 0.0) and (CompareValue(TimeTick, aTimeScale, FP_CUTOFF) < 0) then
          for minorTick := 1 to Trunc(aTimeScale/TimeTick) do
          begin
            tickCoords[0].x := Round(x - TimeTick*minorTick/aFactor*xunit);
            tickCoords[1].x := tickCoords[0].x;
            tickCoords[0].y := linecoords[0].y - (tickHeight-4);
            if CompareValue(tickCoords[0].x, leftBound, FP_CUTOFF) < 0 then
              Break;
            if ShowScale then
                tickCoords[1].y := linecoords[0].y
            else
                tickCoords[1].y := linecoords[0].y + (tickHeight - 4);
            DoDrawTick;
          end;

      text := FloatToStrF(TimeScale*majorTick+LatestTime, DecimalDisplayMode,15, d);
      Canvas.Font.Assign(ScaleFont);
      textsize := Canvas.TextWidth(text);
      x := x - (textsize Div 2);
      aBottom := y + abs(Canvas.Font.Height);
      BoxToClient(x, y, clientX, clientY);
      MTextOut(clientX, clientY,text);
    end;

    y := y + Canvas.TextHeight(text) + tickHeight;
    offset := max(0, Round((rightBound - leftBound - Canvas.TextWidth(FTimeUnit))/2));
    x := leftBound + offset;
    BoxToClient(x, y, clientX, clientY);
    MTextOut(clientX, clientY, FTimeUnit);
  end;
  if (aBottom - aTop) > 0 then
    FScaleBarHeight := aBottom - aTop
  else
    FScaleBarHeight := 0;
end;

procedure TTreeCustomControl.DrawScaleCircle;
const
  DELTA = 0.00001;
var
  points : array[0..1] of TPoint;
  clientCoords: array[0..1] of TPoint;
  x,y, clientX, clientY : integer;
  text : AnsiString;
  textsize : integer;
  i, j, d, w, decimals : integer;
  angle, margin : double;
  p : TpNode;
  ay0, ay1, ax0, ax1: Integer;
begin
    if Topoflag or (not ShowScale) then Exit;
    if Scale = 0.0 then Exit;

    margin := Radius*4*CenterMargin/100;
    angle := (1/2 -StartAngle/6)*PI;
    UpdatePen(FScalePen, psSolid, FScalePen.Width, ScalePen.Color, pecSquare, pjsMiter);
    Canvas.Pen := FScalePen;
    Canvas.Font.Assign(ScaleFont);
    Canvas.Brush.Style := bsClear;
    decimals := Pos('.', FScaleText);
    if decimals > 0 then
        decimals := Length(FScaleText)-decimals;

    p := Root;
    while (not p.OTU) and (not p.compressed) do p := p.des1;
    if (not p.OTU) and p.compressed then p := p.des1;
    if p.hidden then
      d := Round(Abs(OTU_Font.Height)*6/4)
    else
      d := Abs(OTU_Font.Height);
    w := Abs(ScaleFont.Height);

    if IsLinearized then
    begin
        points[0].x := Origin.X +round(cos(angle)*Radius*4 +cos(angle +1/2*PI)*d) +xbase;
        points[0].y := Origin.Y -round(sin(angle)*Radius*4 +sin(angle +1/2*PI)*d) +ybase;
        if (isRooted and ShowRoot) then
        begin
            points[1].x := Origin.X +round(cos(angle)*(margin -0.1*Radius*4) +cos(angle +1/2*PI)*d) +xbase;
            points[1].y := Origin.Y -round(sin(angle)*(margin -0.1*Radius*4) +sin(angle +1/2*PI)*d) +ybase;
        end
        else
        begin
            points[1].x := Origin.X +round(cos(angle)*margin +cos(angle +1/2*PI)*d) +xbase;
            points[1].y := Origin.Y -round(sin(angle)*margin +sin(angle +1/2*PI)*d) +ybase;
        end;
        ConvertToClientCoords(points, clientCoords, 2);
        MPolyLine(slice(clientCoords,2));
        for i := 0 to trunc(xmax/Scale) do
        begin
            points[0].x := Origin.X +round(Cos(angle)*(Radius*4 -(Radius*4-margin)*Scale*i/xmax) +Cos(angle +1/2*PI)*(d+w)) +xbase;
            points[0].y := Origin.Y -round(Sin(angle)*(Radius*4 -(Radius*4-margin)*Scale*i/xmax) +Sin(angle +1/2*PI)*(d+w)) +ybase;
            points[1].x := Origin.X +round(Cos(angle)*(Radius*4 -(Radius*4-margin)*Scale*i/xmax) +Cos(angle +1/2*PI)*d) +xbase;
            points[1].y := Origin.Y -round(Sin(angle)*(Radius*4 -(Radius*4-margin)*Scale*i/xmax) +Sin(angle +1/2*PI)*d) +ybase;
            ConvertToClientCoords(points, clientCoords, 2);
            MPolyLine(slice(clientCoords,2));
            if (ScaleTick > 0.0) and (ScaleTick < Scale) then
                for j := 1 to Trunc(Scale/ScaleTick) do
                begin
                    if xmax < (Scale*i+ScaleTick*j) then Break;
                    points[0].x := Origin.X +round(Cos(angle)*(Radius*4 -(Radius*4-margin)*(Scale*i+ScaleTick*j)/xmax) +Cos(angle +1/2*PI)*(d+2/3*w)) +xbase;
                    points[0].y := Origin.Y -round(Sin(angle)*(Radius*4 -(Radius*4-margin)*(Scale*i+ScaleTick*j)/xmax) +Sin(angle +1/2*PI)*(d+2/3*w)) +ybase;
                    points[1].x := Origin.X +round(Cos(angle)*(Radius*4 -(Radius*4-margin)*(Scale*i+ScaleTick*j)/xmax) +Cos(angle +1/2*PI)*d) +xbase;
                    points[1].y := Origin.Y -round(Sin(angle)*(Radius*4 -(Radius*4-margin)*(Scale*i+ScaleTick*j)/xmax) +Sin(angle +1/2*PI)*d) +ybase;
                    ConvertToClientCoords(points, clientCoords, 2);
                    MPolyLine(slice(clientCoords,2));
                end;

            text := FloatToStrF(Scale*i, DecimalDisplayMode,15, decimals);
            textsize := Canvas.TextWidth(text)*4;
            if Sin(angle) >= 0.0 then
            begin
                x := Origin.X +round(Cos(angle)*(Radius*4 -(Radius*4-margin)*Scale/xmax*i -w) +Cos(angle +1/2*PI)*((d+3/2*w)+textsize)) +xbase;
                y := Origin.Y -round(Sin(angle)*(Radius*4 -(Radius*4-margin)*Scale/xmax*i -w) +Sin(angle +1/2*PI)*((d+3/2*w)+textsize)) +ybase;
            end
            else
            begin
                x := Origin.X +round(Cos(angle)*(Radius*4 -(Radius*4-margin)*Scale/xmax*i +w) +Cos(angle +1/2*PI)*(d+3/2*w)) +xbase;
                y := Origin.Y -round(Sin(angle)*(Radius*4 -(Radius*4-margin)*Scale/xmax*i +w) +Sin(angle +1/2*PI)*(d+3/2*w)) +ybase;
            end;
            BoxToClient(x, y, clientX, clientY);
            MTextOut(clientX, clientY,text);
        end;
    end
    else
    begin
        points[0].x := p.position.x +round(cos(angle +1/2*PI)*d) +xbase;
        points[0].y := p.position.y -round(sin(angle +1/2*PI)*d) +ybase;
        points[1].x := p.position.x -round(cos(angle)*(xunit*Scale) -cos(angle +1/2*PI)*d) +xbase;
        points[1].y := p.position.y +round(sin(angle)*(xunit*Scale) -sin(angle +1/2*PI)*d) +ybase;
        y := Round((points[0].y + points[1].y) / 2)- (d div 2);
        ay0 := points[0].y;
        ay1 := points[1].y;
        ax0 := points[0].x;
        ax1 := points[1].x;
        ConvertToClientCoords(points, clientCoords, 2);
        MPolyLine(slice(clientCoords,2));
        points[0].x := p.position.x +round(cos(angle +1/2*PI)*(d+w)) +xbase;
        points[0].y := p.position.y -round(sin(angle +1/2*PI)*(d+w)) +ybase;
        points[1].x := p.position.x +round(cos(angle +1/2*PI)*d) +xbase;
        points[1].y := p.position.y -round(sin(angle +1/2*PI)*d) +ybase;
        ConvertToClientCoords(points, clientCoords, 2);
        MPolyLine(slice(clientCoords,2));

        points[0].x := p.position.x -round(cos(angle)*(xunit*Scale) -cos(angle +1/2*PI)*d) +xbase;
        points[0].y := p.position.y +round(sin(angle)*(xunit*Scale) -sin(angle +1/2*PI)*d) +ybase;
        points[1].x := p.position.x -round(cos(angle)*(xunit*Scale) -cos(angle +1/2*PI)*(d+w)) +xbase;
        points[1].y := p.position.y +round(sin(angle)*(xunit*Scale) -sin(angle +1/2*PI)*(d+w)) +ybase;
        ConvertToClientCoords(points, clientCoords, 2);
        MPolyLine(slice(clientCoords,2));

        text := FloatToStrF(Scale, DecimalDisplayMode,15, decimals);
        textsize := Canvas.TextWidth(text);
        if Sin(angle) >= 0.0 then
        begin
            x := p.position.x -round(cos(angle)*(xunit*Scale +2*w)/2 -cos(angle +1/2*PI)*((d+1/2*w)+textsize)) +xbase;
            y := p.position.y +round(sin(angle)*(xunit*Scale +2*w)/2 -sin(angle +1/2*PI)*((d+1/2*w)+textsize)) +ybase;

        end
        else
        begin
            x := p.position.x -round(cos(angle)*(xunit*Scale -2*w)/2 -cos(angle +1/2*PI)*(d+1/2*w)) +xbase;
            y := p.position.y +round(sin(angle)*(xunit*Scale -2*w)/2 -sin(angle +1/2*PI)*(d+1/2*w)) +ybase;
        end;
        if CompareValue(abs(sin(angle)), 1, DELTA) = 0 then
          y := ay0 + Round((ay1 - ay0 - Canvas.TextHeight(text)) / 2)
        else if CompareValue(abs(cos(angle)), 1, DELTA) = 0 then
          x := ax0 + Round((ax1 - ax0 - Canvas.TextWidth(text)) / 2);
        BoxToClient(x, y, clientX, clientY);
        MTextOut(clientX, clientY, text);
    end;
end;

procedure TTreeCustomControl.Draw;
begin
  if (not TreeExist) or SkipDrawing or (not Visible) then
    Exit;
  DrawTree;
  if NodeFocused or BranchFocused then
      DrawFocus;
  if (Length(FCalibratedNodes) > 0) and (FShowDivergenceTimes or ShowCalibratedNodeMarker) then
    DrawCalibratedNodeMarkers;
  EndRefreshCallback;
end;

procedure TTreeCustomControl.DrawCalibratedNodeMarkers;
var
  d,i : Integer;
  TempIndex: Integer;
  p: array[0..3] of TPoint;
  ClientCoords: array[0..3] of TPoint;
  nodeIsFocused: Boolean = False;
  minTime: Double = -1;
  maxTime: Double = -1;
  isSelected: Boolean = True;
begin
    if (not ShowCalibratedNodeMarker) or (not TreeExist) or (Length(FCalibratedNodes) = 0) then
      Exit;
    if FIsSamplingTimes then
      Exit;
    for i := Low(FCalibratedNodes) to High(FCalibratedNodes) do
    begin
      TempIndex := FCalibratedNodes[i];
      if (TempIndex < 0) then
           Exit;
      if (TempIndex > NoOfNodes-1) or (not Assigned(Node[TempIndex])) then
         Continue;
      if Node[TempIndex].hidden then continue;
      d := BranchPen.Width div 2 + 2;

      if Assigned(FCalibrationFunc) then
        FCalibrationFunc(TempIndex, minTime, maxTime, isSelected);
      if isSelected then
        Canvas.Pen.Color := clRed
      else
        Canvas.Pen.Color := clGrayText;

      nodeIsFocused := (FocusedIndex = TempIndex) and (not BranchFocused);
      if nodeIsFocused then
      begin
        {$IFNDEF DARWIN}
        Canvas.Pen.Mode := pmNotXor;
        {$ENDIF}
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clLime;
        Canvas.Pen.Width := 3;
        DrawCircle(Node[TempIndex].position.x + xbase, Node[TempIndex].position.y + ybase, d, Canvas.Pen, Canvas.Brush);
      end
      else if Node[TempIndex].outgroup then
      begin
          Canvas.Pen.Color := clGrayText;
          Canvas.Pen.Width := 1;
      end
      else
      begin
        Canvas.Pen.Width := 2;
        Canvas.Brush.Style := bsClear;
        Canvas.Brush.Color := clWhite;
      end;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Mode := pmCopy;
      DrawCircle(Node[TempIndex].position.x + xbase, Node[TempIndex].position.y + ybase, d, Canvas.Pen, Canvas.Brush);
    end;
end;

procedure TTreeCustomControl.DrawCharState;
var
  x,y: Integer;
  clientX: Integer = -1;
  clientY: Integer = -1;
  d, i : integer;
begin
  if IsDoingCalibrations then Exit;
  if not((TreeStyle = tsTraditional) and (BranchStyle = bsRectangular)) then Exit;

  Canvas.Font.Assign(CharStateFont);
  Canvas.Brush.Style := bsClear;
  for i := 1 to NoOfNodes do
  begin
    with Node[i]^ do
    begin
      if not NeedsPainting(Node[i]) then Continue;
      if (Node[i] = Root) and (not isRooted) then Continue;
      if Node[i].Hidden then Continue;
      if IsLivingSequences and (i > NoOfOtus) then Continue;
      //d := Canvas.TextWidth(charstate);
      d := 4;
      if Node[i].OTU or Node[i].compressed then
        d := d + GetLineWidth(Node[i])
      else
        d := d + VertLineWidth(Node[i]);
      x := position.x + d;
      y := position.y - round(Canvas.TextHeight(charstate) / 2);
      BoxToClient(x+xbase, y+ybase, clientX, clientY);
      MTextOut(clientX, clientY, charstate);
    end;
  end;
end;

procedure TTreeCustomControl.DrawOTUName(p: TpNode);
const
  DELTA = 0.00001;
var
  x,y,d : integer;
  AFont,pfont : TFont;
  PrivateNameRect: TRect;
  aName: String;
  a: TNodeAttrib;
  clientX: Integer = -1;
  clientY: Integer = -1;
  point: array[0..3] of TPoint;
  clientCoords: array[0..3] of TPoint;
begin
  try
    aName := p.name;
    a := GetNodeAttrib(p);
    if (not a.ShowTaxonName) and (not a.ShowSpeciesName) then Exit;
    if p.compressed  and (GetCompressedClusterSize(p) <> 1) then { get appropriate font. It is either the otu font or caption font}
      AFont := a.CaptionFont
    else
      AFont := a.Font;

    Canvas.Font.Assign(AFont);
    Canvas.Brush.Style := bsClear;
    d := GetLineWidth(p)*2; { need to look at GetLineWidth}
    if (TreeStyle = tsRadiation) or (TreeStyle = tsCircle) then
    begin
      if (not p.OTU) and p.compressed then
        x := (p.des1.position.x + p.des2.position.x) div 2
      else
        x := p.position.x;
      if cos(p.angle) < 0.0 then
        x := x -p.namesize.x;

      if GetTaxonMarker(p).Shape <> msNone then
      begin
        d := d +Round((1+abs(cos(p.angle)))*Abs(AFont.Height*1));
        if CompareValue(cos(p.angle), 0.0, DELTA) = 0 then
          x := x - Round(p.namesize.x/2)
        else if cos(p.angle) < 0.0 then
          x := x - d
        else
          x := x + d;
      end
      else
      begin
        if abs(cos(p.angle)) < sqrt(2)/2 then
          if cos(p.angle) < 0.0 then
            x := x +round((1-abs(cotan(p.angle)))*p.namesize.x/2) -d
          else
            x := x -round((1-abs(cotan(p.angle)))*p.namesize.x/2) +d
        else if cos(p.angle) < 0.0 then
          x := x -d
        else
          x := x +d;
      end
    end
    else { then rectangular}
    begin
      if GetTaxonMarker(p).Shape <> msNone then
        d := d +Abs(AFont.Height*2);
      if ShowCharState then
        d := d +Canvas.TextWidth(p.charstate) +Abs(CharStateFont.Height);
      if (not p.OTU) and p.compressed and (not Topoflag) then
        x := p.des1.position.x +d
      else
        x := p.position.x +d;
      if FIsDoingCalibrations and ShowCalibratedNodeMarker then
        x := x + 4;
      if FAlignOtuNames and (not FTopologyOnly) then
        if FMakeBranchesEqualLength then
          x := FMaxXPosition + GetMarkerWidth(p)
        else
          x := Round(xmax*xunit) + GetMarkerWidth(p);
    end;

    if (TreeStyle = tsRadiation) or (TreeStyle = tsCircle) then
    begin
      if (not p.OTU) and p.compressed then
        y := (p.des1.position.y + p.des2.position.y) div 2
      else
        y := p.position.y;
      if GetTaxonMarker(p).Shape <> msNone then
      begin
        if sin(p.angle) < 0.0 then { below the x-axis}
          y := y +Round(Abs(sin(p.angle)*AFont.Height*2))
        else
          y := y -Round(Abs(sin(p.angle)*AFont.Height*3));
      end
      else if abs(cos(p.angle)) < sqrt(2)/2 then { the point is closer to the y-axis than it is to the x-axis}
      begin
        if sin(p.angle) < 0.0 then { below the x-axis}
          y := y  +round((1-abs(cotan(p.angle)))*abs(AFont.Height) / 2)
        else { above the x-axis}
          y := y -round((2-abs(cotan(p.angle)))*abs(AFont.Height)*1);
      end
      else { the point is closer to the y-axis}
      begin
        if sin(p.angle) < 0.0 then { below the x-axis}
          y := y -round(abs(tan(p.angle)*AFont.Height)*2)
        else { above the x-axis}
          y := y -round(abs(tan(p.angle)*AFont.Height)*2)
      end;
    end { end radiation and circular style}
    else if (not p.OTU) and p.compressed then
      y := (p.des1.position.y + p.des2.position.y) div 2 - Round(Canvas.TextHeight(a.Caption)/2)
    else
      y := p.position.y  - round(Canvas.TextHeight(p.name)/2);

    if TreeStyle = tsTraditional then
      if p.compressed then
      begin
        if p.OTU then
          aname := p.name
        else if FShowCompressedClusterSize then
          aName := Format(' %s (%.0n taxa)', [a.Caption, GetCompressedClusterSize(p)*1.0])
        else
          aname := a.Caption;
      end
      else
      begin
        if a.ShowSpeciesName then
          aname := p.SpeciesName
        else
          aname := p.name;
        if ShowNodeIds then
          aname := '(' + IntToStr(p.index) + ') ' + aname
        else
          aname := ' '+ aname

      end
    else if cos(p.angle) < 0.0 then
    begin
      if a.ShowSpeciesName then
        aname := p.SpeciesName
      else
        aname := p.name;
    end
    else
    begin
      if a.ShowSpeciesName then
        aname := ' '+p.SpeciesName
      else
        aname := ' ' + p.name;
    end;
    if p.hilighted then
    begin
      if p.CustomHighlightColor <> clWhite then
      begin
        Canvas.Font.Color := p.CustomHighlightColor;
      end
      else
        Canvas.Font.Color := HilightColor;
    end;

    if p.PrivateName <> EmptyStr then
    begin
      pfont := TFont.Create;
      pfont.Assign(AFont);
      pfont.Style := pfont.Style + [fsBold];

      PrivateNameRect := Rect(x+xbase,y+ybase-(StrHeight(p.PrivateName, pfont)), x+xbase+(StrLength(p.PrivateName + ' ', pfont)), y+ybase);
      FreeAndNil(pfont);
      BoxToClient(x+xbase, y+ybase, clientX, clientY);
      if HideOverlappingTaxa then
      begin
        if (TreeStyle = tsTraditional) and (clientY < FMaxOTUPos) then
          Exit;
        if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then
        begin
          FNewOtuCoords.Top := ClientY;
          FNewOtuCoords.Left := ClientX;
          FNewOtuCoords.Bottom := FNewOtuCoords.Top + Canvas.TextHeight(p.PrivateName);
          FNewOtuCoords.Right := FNewOtuCoords.Left + Canvas.TextWidth(p.PrivateName);
          if IntersectRect(FTempRect, FNewOtuCoords, FLastOtuCoords) then
            Exit;

          FLastOtuCoords.TopLeft := FNewOtuCoords.TopLeft;
          FLastOtuCoords.BottomRight := FNewOtuCoords.BottomRight;
        end;
      end;
      if FDrawBoxForHighlightedNames and p.hilighted and (p.PrivateName <> UserAddedStr) and (p.PrivateName <> UndefinedStr) then
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Style := psDot;
        Canvas.Pen.Mode := pmCopy;
        Canvas.Pen.Width := 2;
        Canvas.Pen.Color := clRed;
        point[0].X := clientX;
        point[0].Y := clientY - 2;

        point[1].X := clientX + Canvas.TextWidth(aname) + 4;
        point[1].Y := point[0].Y;

        point[2].X := point[1].X;
        point[2].Y := clientY + Canvas.TextHeight(aname) + 2;

        point[3].X := point[0].X;
        point[3].Y := point[2].Y;

        ConvertToClientCoords(point, clientCoords, 4);
        MRectangle(clientCoords);
      end;
      MTextOut(clientX, clientY, p.PrivateName);
      FMaxOTUPos := clientY + abs(aFont.Height);
      if ShowMappedTreeNames or (p.PrivateName = UserAddedStr) or (p.PrivateName = UndefinedStr) then
      begin
        if not p.hilighted then
          Canvas.Font.Color := clSilver;
        BoxToClient(PrivateNameRect.Right, y+ybase, clientX, clientY);
        MTextOut(clientX, clientY, aname);
      end;
    end
    else
    begin
      BoxToClient(x+xbase, y+ybase, clientX, clientY);
      if HideOverlappingTaxa then
      begin
        if (TreeStyle = tsTraditional) and (clientY < FMaxOTUPos) then
          Exit;
        if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then
        begin
          FNewOtuCoords.Top := ClientY+1;
          FNewOtuCoords.Left := ClientX;
          FNewOtuCoords.Bottom := FNewOtuCoords.Top + Canvas.TextHeight(aname)-2;
          FNewOtuCoords.Right := FNewOtuCoords.Left + Canvas.TextWidth(aName);
          if IntersectRect(FTempRect, FNewOtuCoords, FLastOtuCoords) then
            Exit;
          FLastOtuCoords.TopLeft := FNewOtuCoords.TopLeft;
          FLastOtuCoords.BottomRight := FNewOtuCoords.BottomRight;
        end;
      end;
      if FDrawBoxForHighlightedNames and p.hilighted then
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Mode := pmCopy;
        Canvas.Pen.Width := 2;
        Canvas.Pen.Color := clRed;

        point[0].X := clientX;
        point[0].Y := clientY - 2;

        point[1].X := clientX + Canvas.TextWidth(aname) + 4;
        point[1].Y := point[0].Y;

        point[2].X := point[1].X;
        point[2].Y := clientY + Canvas.TextHeight(aname) + 2;

        point[3].X := point[0].X;
        point[3].Y := point[2].Y;

        ConvertToClientCoords(point, clientCoords, 4);
        MRectangle(clientCoords);
      end;

      MTextOut(clientX, clientY, aname);
      FMaxOTUPos := clientY + abs(AFont.Height);
    end;
  Except on E: Exception do
    ShowMessage(E.Message + '; DrawOTUName has encountered an error, please report this to MEGA''s authors');
  end;
end;

procedure TTreeCustomControl.DrawOTUNameRadiation(p: TpNode);
var
  x,y,dx,dy : integer;
  r: double;
  AFont : TFont;
  aName: String;
  orientation: Integer;
  clientX: Integer = -1;
  clientY: Integer = -1;
  target: TRect;
begin
    if not FMyNodeAttrib.ShowTaxonName then Exit;
    if (not p.OTU) and p.compressed then
      AFont := FMyNodeAttrib.CaptionFont
    else
      AFont := FMyNodeAttrib.Font;

    dx := abs(AFont.Height) +GetLineWidth(p)*2;
    if GetTaxonMarker(p).Shape <> msNone then
      dx := dx - Abs(AFont.Height*4);
    dy := Abs(AFont.Height)*2;


    if FRevflg then
      r := p.angle + PI
    else
      r := p.angle;

    orientation := Round(r/2/PI*3600); { convert from radians to 10th degrees}
    Canvas.Font := AFont;

    if (not p.OTU) and p.compressed then
    begin
      x := (p.des1.position.x + p.des2.position.x) div 2;
      y := (p.des1.position.y + p.des2.position.y) div 2;
    end
    else
    begin
      x := p.position.x;
      y := p.position.y;
    end;

    dx := -1 * Round(dx/2);
    dy := -1 * Round(dy/3);
    if not FRevflg then
    begin
      if FTreeDrawingTarget <> tdtCanvas then
      begin
        x := x + Round(Sin(r)*P.namesize.y) + xbase + Round(Cos(r)*dx +Sin(r)*dy) + Round(p.namesize.y*Cos(r));
        y := y + Round(Cos(r)*p.namesize.y) + ybase - Round(Sin(r)*dx -Cos(r)*dy) - Round(p.namesize.y*Sin(r));
      end
      else
      begin
        x := x +xbase + Round(Cos(r)*dx +Sin(r)*dy) + Round(p.namesize.y*Cos(r));
        y := y +ybase - Round(Sin(r)*dx -Cos(r)*dy) - Round(p.namesize.y*Sin(r));
      end;
    end
    else
    begin
      if FTreeDrawingTarget <> tdtCanvas then
      begin
        if FTreeDrawingTarget = tdtPdf then
        begin
          x := x + xbase - Round(cos(r)*p.namesize.x/2) - Round(p.namesize.y*Cos(r));
          y := y + ybase + Round(sin(r)*p.namesize.x/2) + Round(p.namesize.y*Sin(r));
        end
        else
        begin
          x := x + xbase - Round(cos(r)*p.namesize.x) - Round(p.namesize.y*Cos(r));
          y := y + ybase + Round(sin(r)*p.namesize.x) + Round(p.namesize.y*Sin(r));
        end;
      end
      else
      begin
        x := x +xbase - Round(Cos(r)*dx -Sin(r)*dy) - Round(cos(r)*p.namesize.x) - Round(p.namesize.y*Cos(r));
        y := y +ybase + Round(Sin(r)*dx +Cos(r)*dy) + Round(sin(r)*p.namesize.x) + Round(p.namesize.y*Sin(r));
      end;
    end;

    if p.compressed then
      aname := FMyNodeAttrib.Caption
    else
      aname := p.name;
    BoxToClient(x, y, clientX, clientY);
    Canvas.Brush.Style := bsClear;
    target := Rect(clientX, clientY, clientX + StrLength(aname, Canvas.Font), clientY + StrLength(aname, Canvas.Font));
    RotateText(target, aname, rtCustom, Orientation);
end;

procedure TTreeCustomControl.DrawCaption(p: TpNode; a: TNodeAttrib);
var
    x, y: Integer;
    clientX: Integer = -1;
    clientY: Integer = -1;
    point: array[0..3] of TPoint;
    clientCoords: array[0..3] of TPoint;
    dx: Integer = 0;
    fHeight: Integer;
    bHeight, bMidpoint: Integer;
begin
  fHeight := Abs(a.CaptionFont.Height);
  bHeight := (p.Bracket.Bottom-p.Bracket.Top);
  bMidpoint := Round((p.Bracket.Top +p.Bracket.Bottom)/2);
  Canvas.Pen := ScalePen;
  Canvas.Brush.Style := bsClear;
  x := p.Bracket.Left;
  if ShowNodeIds then
    dx := Canvas.TextWidth(Format('(%d)', [p.index]));

  if a.ShowBracket then
  begin
    Canvas.Pen.Width := a.BracketLineWidth;
    Canvas.Pen.Color := a.BracketColor;
    x := x + a.BracketLineWidth*2 + dx;
    case a.BracketStyle of
      brsSquare:
        begin
          point[0].x := p.Bracket.Left + a.BracketLineWidth + xbase + dx;
          point[0].y := p.Bracket.Top + ybase;
          point[1].x := point[0].x + Round(fHeight/3);
          point[1].y := point[0].y;
          point[2].x := point[1].x;
          point[2].y := p.Bracket.Bottom + ybase;
          point[3].x := point[0].x;
          point[3].y := point[2].y;
          ConvertToClientCoords(point, clientCoords, 4);
          MPolyLine(slice(clientCoords,4));
        end;
      brsLine:
        begin
          point[0].x := p.Bracket.Left + a.BracketLineWidth + xbase + dx + Round(fHeight/3);
          point[0].y := p.Bracket.Top + ybase;
          point[1].x := point[0].x;
          point[1].y := p.Bracket.Bottom + ybase;
          ConvertToClientCoords(point, clientCoords, 2);
          MPolyLine(slice(clientCoords,2));
        end;
    end;
  end;
  if a.ShowLabel then
  begin
    if a.IsImage and a.ShowImage then
    begin
      case a.GraphicAlign of
        gaRight:
          begin
            x := x + fHeight;
            y := bMidpoint - Round(fHeight/2);
          end;
        gaLeft:
          begin
            x := x + fHeight*2 + a.Bitmap.Width;
            y := bMidpoint - Round(fHeight/2);
          end;
        gaTop:
          begin
            if p.namesize.x < a.Bitmap.Width then
              x := x + fHeight + Round(a.Bitmap.Width/2) - Round(p.namesize.x/2)
            else
              x := x + fHeight;
            if (a.Bitmap.Height + fHeight) > bHeight then
              y := p.Bracket.Top + a.Bitmap.Height + fHeight
            else
              y := bMidpoint + Round(a.Bitmap.Height/2) + fHeight;

          end;
        gaBottom:
          begin
            if p.namesize.x < a.Bitmap.Width then
              x := x + fHeight + Round(a.Bitmap.Width/2) - Round(p.namesize.x/2)
            else
              x := x + fHeight;
            if (a.Bitmap.Height + fHeight) > bHeight then
              y := p.Bracket.Top + fHeight
            else
              y := bMidpoint - Round(a.Bitmap.Height/2);
          end;
      end;
    end
    else
    begin
      x := x + fHeight;
      y := bMidpoint - Round(fHeight/2);
      if fHeight = bHeight then
        y := y - (fHeight div 4);
    end;
    x := x + xbase;
    y := y + ybase;
    Canvas.Font.Assign(a.CaptionFont);
    BoxToClient(x, y, clientX, clientY);
    MTextOut(clientX, clientY, a.Caption);
  end;
end;

procedure TTreeCustomControl.DrawCompressedArea(p: TpNode; a: TNodeAttrib);
var
  point: array[0..3] of TPoint;
  clientCoords: array[0..3] of TPoint;
begin
  Canvas.Pen := EdgePen;
  Canvas.Brush.Color := Canvas.Pen.Color;
  Canvas.Brush.Style := a.FillStyle;
  point[0].x := p.position.x +xbase;
  point[0].y := p.position.y +a.LineWidth +ybase;
  point[1].x := p.position.x +xbase;
  point[1].y := p.position.y -a.LineWidth +ybase;
  point[2].x := p.des1.position.x +xbase;
  point[2].y := p.des1.position.y +ybase;
  point[3].x := p.des2.position.x +xbase;
  point[3].y := p.des2.position.y +ybase;
  ConvertToClientCoords(point, clientCoords, 4);
  MPolygon(slice(clientCoords, 4));
end;

procedure TTreeCustomControl.DrawCompressedAreaRadiation(p: TpNode);
var
  point : array[0..3] of TPoint;
  clientCoords: array[0..3] of TPoint;
begin
  Canvas.Pen := FEdgePen;
  Canvas.Brush.Style := GetNodeAttrib(p).FillStyle;
  point[0].x := p.position.x +Round(sin(p.angle)*FMyNodeAttrib.LineWidth) +xbase;
  point[0].y := p.position.y +Round(cos(p.angle)*FMyNodeAttrib.LineWidth) +ybase;
  point[1].x := p.position.x -Round(sin(p.angle)*FMyNodeAttrib.LineWidth) +xbase;
  point[1].y := p.position.y -Round(cos(p.angle)*FMyNodeAttrib.LineWidth) +ybase;
  point[2].x := p.des1.position.x +xbase;
  point[2].y := p.des1.position.y +ybase;
  point[3].x := p.des2.position.x +xbase;
  point[3].y := p.des2.position.y +ybase;
  ConvertToClientCoords(point, clientCoords, 4);
  MPolygon(slice(clientCoords, 4));
end;

procedure TTreeCustomControl.DrawBitmap(p: TpNode; a: TNodeAttrib);
var
  imageHeight,imageWidth,x,y: integer;
  clientX: Integer = -1;
  clientY: Integer = -1;
  bracketHeight: Integer;
  bracketMidPoint: Integer;
  fontHeight: Integer;
begin
  imageWidth := a.Image.Width;
  imageHeight := a.Image.Height;
  bracketHeight := (p.Bracket.Bottom - p.Bracket.Top);
  bracketMidpoint := Round((p.Bracket.Top +p.Bracket.Bottom)/2);
  fontHeight := abs(a.CaptionFont.Height);

  x := p.Bracket.Left;
  if not a.IsCompressed and a.ShowBracket then
    x := x + a.BracketLineWidth*2;

  if a.ShowLabel then
  begin
    if a.GraphicAlign = gaRight then
      x := x + p.namesize.X + fontHeight
    else if a.GraphicAlign = gaLeft then
      x := x + fontHeight
    else if p.namesize.X > imageWidth then
      x := x + Round((p.namesize.X - imageWidth)/2);
  end
  else
    x := x + fontHeight;
  if a.IsCompressed and a.ShowTaxonMarker and (a.Marker.Shape <> msNone) then
    x := x + fontHeight;

  if p.OTU or p.compressed then
  begin
    if bracketHeight < imageHeight then
      y := p.bracket.Top
    else
      y := p.position.y - Round(imageHeight/2);
  end
  else if a.ShowLabel then
  begin
    if a.GraphicAlign = gaTop then
    begin
      if (imageHeight + fontHeight) > bracketHeight then
        y := p.Bracket.Top
      else
        y := bracketMidpoint - (imageHeight + fontHeight)
    end
    else if a.GraphicAlign = gaBottom then
    begin
      if (imageHeight + fontHeight) > bracketHeight then
        y := p.Bracket.Top + fontHeight
      else
        y := bracketMidpoint - imageHeight + fontHeight
    end
    else if imageHeight > bracketHeight then
      y := p.Bracket.Top
    else
      y := bracketMidPoint - Round(imageHeight/2)
  end
  else if imageHeight > bracketHeight then
    y := p.Bracket.Top
  else
    y := bracketMidpoint - Round(imageHeight/2);

  x := x + xbase;
  y := y + ybase;
  BoxToClient(x, y, clientX, clientY);
  MDrawImage(clientX, clientY, a.Image);
end;

procedure TTreeCustomControl.DrawExtendedLinesToOtuNames;
var
  i: Integer = 0;
  n: TpNode = nil;
  aLine: array [0..1] of TPoint;
  clientCoords: array[0..1] of TPoint;
begin
  if (not FAlignOtuNames) or FTopologyOnly or (FTreeStyle = tsRadiation) or (FTreeStyle = tsCircle) then
    Exit;
  Canvas.Pen.Style := psDot;
  Canvas.Pen.Color := clSilver;
  Canvas.Pen.Width := 1;
  if NoOfOTUs > 0 then
    for i := 1 to NoOfOTUs do
    begin
      n := Node[i];
      if not n.hidden then
      begin
        aLine[0].X := n.position.x + xbase + GetMarkerWidth(n);
        aLine[0].Y := n.Position.y + ybase;
        if FMakeBranchesEqualLength then
          aLine[1].X := FMaxXPosition + xbase
        else
          aLine[1].X := xbase + Round(xmax*xunit);
        aLine[1].Y := aLine[0].Y;
        ConvertToClientCoords(aLine, clientCoords, Length(aLine));
        MPolyline(clientCoords);
      end;
    end;
end;

procedure TTreeCustomControl.DrawBranchLength;
var
  str: AnsiString = '';
  clientX: Integer = -1;
  clientY: Integer = -1;
  i: integer = -1;
  Coords: TPoint;
begin
    if (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then Exit;
    Canvas.Font.Assign(BLensFont);
    Canvas.Brush.Style := bsClear;
    for i := 1 to NoOfNodes do
    begin
      if not NeedsPainting(Node[i]) then
        Continue;
      if (Node[i].hidden) or (Node[i] = Root) then Continue;

      with Node[i]^ do
      begin
        if not Topoflag then
          if (IsLinearized or ForceLinearized) and (anc.height-height < BLenCutOff) then
            Continue
          else if (CompareValue(branch.length, BLenCutOff, 0.00000001) < 0) then
            Continue;

        if FTreeBoxType = tttReltimeTree then
        begin
          if CompareValue(TimeFactor, 1.0, 0.00000001) <> 0 then
          begin
            if FIsSamplingTimes then
              str := FloatToStrF(GetDivergenceTime(index) - GetDivergenceTime(anc.index), ffFixed, 15, BLenDecimals)
            else
              str := FloatToStrF(GetDivergenceTime(anc.index) - GetDivergenceTime(index), ffFixed, 15, BLenDecimals)
          end
          else
            str := FloatToStrF(anc.height-height, ffFixed, 15, BLenDecimals)
        end
        else
          str := FloatToStrF(branch.length, ffFixed, 15, BLenDecimals);

        if branch.length < 0 then
          Canvas.Font.Color := clRed
        else if CompareValue(branch.length, 0.0, 0.0000001) = 0 then
          Canvas.Font.Color := clGrayText
        else
          Canvas.Font.Color := FBLensFont.Color;
        Coords := GetBestBLenPosition(Node[i], str);
        BoxToClient(Coords.X, Coords.Y, clientX, clientY);
        MTextOut(clientX, clientY, str);
      end;
    end;
end;

procedure TTreeCustomControl.DrawBranches;
begin
  Canvas.Pen := GetBranchPen;
  ChangeAttrib(0);
  DrawBranch(Root);
end;

procedure TTreeCustomControl.DrawBranch(p: TpNode);
var
  q: TpNode;
begin
  inc(FNodesProcessed);
  if p <> Root then
    if TreeStyle = tsCircle then
    begin
      if (p.anc.cursize > (CurrentNoOfOTUs div 4)) and
        ((p.cursize <= (CurrentNoOfOTUs div 4)) or p.OTU or p.compressed) then
        if cos(p.angle) < 0 then
          FRevflg := true
        else
          FRevflg := false;
    end
    else if TreeStyle = tsRadiation then
      if (p.anc.sector > PI/2) and
        ((p.sector <= PI/2) or p.OTU or p.compressed) then
        if cos(p.angle) < 0 then
          FRevflg := true
        else
          FRevflg := false;

  if p <> Root then
    if p.anc.attrindex <> p.attrindex then
      ChangeAttrib(p.attrindex);

  if not (p.OTU or p.compressed) then
  begin
    DrawBranch(p.des1);
    DrawBranch(p.des2);
  end;

  FMyNodeAttrib := GetNodeAttrib(p);
  if p = Root then
    FAttribIndex := 0
  else if (GetNodeAttrib(p).BranchOption = boBranchOnly) or
          (GetNodeAttrib(p).BranchOption = boNoBranch) then
  begin
    q := p.anc;
    while (q <> Root) and ((GetNodeAttrib(q).BranchOption = boBranchOnly) or q.hidden) do
      q := q.anc;
    FAttribIndex := q.attrindex;
  end
  else
    FAttribIndex := p.attrindex;
  ChangeAttrib(p.attrindex);
  Canvas.Pen := GetBranchPen;
  if TreeStyle = tsCircle then
    DrawBranchCircle(p)
  else if TreeStyle = tsRadiation then
    DrawBranchRadiation(p)
  else
    case BranchStyle of
      bsRectangular : DrawBranchRectangle(p);
      bsCurved      : DrawBranchCurve(p);
      bsStraight    : DrawBranchStraight(p);
    end;

  if p <> Root then
    if p.attrindex <> p.anc.attrindex then
      ChangeAttrib(p.anc.attrindex);
end;

procedure TTreeCustomControl.DrawBranchRectangle(p: TpNode);
var
  d: integer;
  a1,a2: TNodeAttrib;
  dwPenWidth: Integer;
  point : array[0..3] of TPoint;
  clientCoords: array[0..3] of TPoint;
  q: TpNode;

  procedure DrawHeightErrBar;
  var
    err0,err1: double;
    dy: integer;
  begin
      if p.OTU or (p.outgroup and HasCalibrations) then exit;

      err0 := 0;
      err1 := 0;
      if assigned(HeightErrBarProc) then
        HeightErrBarProc(p.index,err0,err1);
    dwPenWidth := FCurAttrib.LineWidth;
    UpdatePen(FBranchPen, FCurAttrib.LineStyle, dwPenWidth, FCurAttrib.LineColor, pecSquare, pjsMiter);
    UpdateBrush(Canvas.Brush, bsSolid, $E8E8E8);
    Canvas.Pen := GetBranchPen;
    dy := FCurAttrib.LineWidth*3;
    if dy > yunit then
      dy := yunit;

    point[0].x := min(Round(xmax*xunit + xbase), p.position.x -round(err0*xunit) +xbase);
    point[0].y := p.position.y +dy +ybase;
    point[1].x := point[0].x;
    point[1].y := p.position.y -dy +ybase;
    point[2].x := min(Round(xmax*xunit + xbase), p.position.x +round(err1*xunit) +xbase);
    point[2].y := point[1].y;
    point[3].x := point[2].x;
    point[3].y := point[0].y;
    ConvertToClientCoords(point, clientCoords, 4);
    MRectangle(slice(clientCoords, 4));
    MPolygon(slice(clientCoords, 4));
  end;

  procedure DrawVerticalLine;
  begin
    q := p.des1;
    while q.hidden and Assigned(q.des1) do
      q := q.des1;

    if p.des1.hidden or ((q.attrindex <> p.attrindex) and
                         ((GetNodeAttrib(q).BranchOption = boHalfBranch) or
                          (GetNodeAttrib(q).BranchOption = boBranchOnly))) then
    begin
      q := p.des1;
      while q.hidden and Assigned(q.des1) do
        q := q.des1;
      if p.attrindex <> FAttribIndex then
        ChangeAttrib(FAttribIndex);
      d := GetLineWidth(q);
      point[0].x := p.position.x +xbase;
      point[0].y := q.position.y +ybase - Round(d/2) + FMyNodeAttrib.LineWidth;
      point[1].x := point[0].x;
      point[1].y := p.position.y +ybase + d + FMyNodeAttrib.LineWidth;
      ConvertToClientCoords(point, clientCoords, 2);
      MPolyLine(slice(clientCoords,2));
      if p.attrindex <> FAttribIndex then
        ChangeAttrib(p.attrindex);
    end;
    q := p.des2;
    while q.hidden do q := q.des2;
    if p.des2.hidden or ((q.attrindex <> p.attrindex) and
                         ((GetNodeAttrib(q).BranchOption = boHalfBranch) or
                          (GetNodeAttrib(q).BranchOption = boBranchOnly))) then
    begin
      if p.attrindex <> FAttribIndex then
        ChangeAttrib(FAttribIndex);
      d := GetLineWidth(q);
      point[0].x := p.position.x +xbase;
      point[0].y := p.position.y +ybase - d + FMyNodeAttrib.LineWidth;
      point[1].x := point[0].x;
      point[1].y := q.position.y +ybase + d  - FMyNodeAttrib.LineWidth;
      ConvertToClientCoords(point, clientCoords, 2);
      MPolyLine(slice(clientCoords,2));
      if p.attrindex <> FAttribIndex then
        ChangeAttrib(p.attrindex);
    end;
  end;

begin
  if not NeedsPainting(p) then
    Exit;

  Canvas.Pen := GetBranchPen;
  if p = Root then
  begin
    if (isRooted and ShowRoot) then
    begin
      point[0].x := xbase - LENGTH_OF_ROOT_BRANCH;
      point[0].y := p.position.y +ybase;
      point[1].x := xbase;
      point[1].y := point[0].y;
      ConvertToClientCoords(point, clientCoords, 2);
      MPolyLine(slice(clientCoords,2));
    end;
    DrawVerticalLine;
  end
  else
  begin
    if (p.anc.attrindex <> p.attrindex) then
    begin
      if FMyNodeAttrib.IsImage and FMyNodeAttrib.ShowImage then
        DrawBitmap(p, FMyNodeAttrib);
      if (not p.compressed) and (FMyNodeAttrib.ShowLabel or FMyNodeAttrib.ShowBracket) then
        DrawCaption(p, FMyNodeAttrib);
    end;
    if not p.hidden then
    begin
      if p.OTU or p.compressed then
      begin
        DrawOTUName(p);
        if (not p.OTU) and p.compressed and (not Topoflag) then
          DrawCompressedArea(p, FMyNodeAttrib);
      end
      else
        DrawVerticalLine;

      if (not (p.OTU or p.compressed)) and (GetNodeAttrib(p.des1).BranchOption = boFullBranch) then
        a1 := GetNodeAttrib(p.des1)
      else if GetNodeAttrib(p).BranchOption = boBranchOnly then
        if FAttribIndex < 0 then
          a1 := GroupAttrib[FAttribIndex]
        else
          a1 := AttribList[FAttribIndex]
      else
        a1 := GetNodeAttrib(p);
      if (not (p.OTU or p.compressed)) and (GetNodeAttrib(p.des2).BranchOption = boFullBranch) then
        a2 := GetNodeAttrib(p.des2)
      else if GetNodeAttrib(p).BranchOption = boBranchOnly then
        if FAttribIndex < 0 then
          a2 := GroupAttrib[FAttribIndex]
        else
          a2 := AttribList[FAttribIndex]
      else
        a2 := GetNodeAttrib(p);

      if (p.attrindex <> p.anc.attrindex) and ((GetNodeAttrib(p).BranchOption = boHalfBranch) or
                                               (GetNodeAttrib(p).BranchOption = boBranchOnly)) then
      begin
        if p.compressed then
          d := 0
        else if a1.LineWidth = GetNodeAttrib(p).LineWidth then
          d := a1.LineWidth
        else if a2.LineWidth = GetNodeAttrib(p).LineWidth then
          d := a2.LineWidth
        else if a1.LineWidth > a2.LineWidth then
          d := a1.LineWidth
        else
          d := a2.LineWidth;
        point[0].x := p.anc.position.x +xbase + Round(GetNodeAttrib(p).LineWidth/2);
        point[0].y := p.position.y +ybase;
        if p.position.x = p.anc.position.x then
          point[1].x := point[0].x
        else
          point[1].x := p.position.x +xbase +d -GetNodeAttrib(p).LineWidth;
        point[1].y := point[0].y;
        ConvertToClientCoords(point, clientCoords, 2);
        Canvas.Pen := GetBranchPen;
        MPolyLine(slice(clientCoords,2));

        if (IsLinearized or IsSamplingTimes) and (not Topoflag) and ShowHeightErrBar then
          DrawHeightErrBar;
      end
      else
      begin
        if ((p.attrindex <> FAttribIndex) and (GetNodeAttrib(p).BranchOption = boNoBranch)) or
           ((p.anc.attrindex = p.attrindex) and (GetNodeAttrib(p).BranchOption = boBranchOnly)) then
          ChangeAttrib(FAttribIndex);
        Canvas.Pen := GetBranchPen;
        if p.compressed then
          d := 0
        else if a1.LineWidth = FCurAttrib.LineWidth then
          d := a1.LineWidth*1
        else if a2.LineWidth = FCurAttrib.LineWidth then
          d := a2.LineWidth*1
        else if a1.LineWidth > a2.LineWidth then
          d := a1.LineWidth*1
        else
          d := a2.LineWidth*1;
        point[0].x := p.anc.position.x +xbase;
        if p = p.anc.des1 then
        begin
          if (p.anc = root) and IsRooted and (not ShowRoot) then
            point[0].y := p.anc.position.y +ybase + FCurAttrib.LineWidth
          else
            point[0].y := p.anc.position.y +ybase
        end
        else
          point[0].y := p.anc.position.y +ybase +FCurAttrib.LineWidth;
        point[1].x := point[0].x;
        point[1].y := p.position.y +ybase;
        if p.position.x = p.anc.position.x then
          point[2].x := point[1].x
        else
          point[2].x := p.position.x +xbase +d -FCurAttrib.LineWidth;
        point[2].y := p.position.y +ybase;
        ConvertToClientCoords(point, clientCoords, 3);
        MPolyLine(slice(clientCoords,3));

        if (IsLinearized or IsSamplingTimes) and (not Topoflag) and (not p.outgroup) and ShowHeightErrBar then
          DrawHeightErrBar;
      end;
      if GetNodeAttrib(p) <> FCurAttrib then
        ChangeAttrib(p.attrindex);
    end;
  end;
end;

procedure TTreeCustomControl.DrawBranchCurve(p: TpNode);
var
  x0,x1,x2,y0,y1,y2,i,n : integer;
  r: double;
  xDelta, yDelta: Integer;
  point : array[0..600] of TPoint;
  clientCoords: array[0..600] of TPoint;
  q, p2: TpNode;
begin
  if not NeedsPainting(p) then
    Exit;
  Canvas.Pen := GetBranchPen;
  if p = Root then
  begin
    if (isRooted and ShowRoot) then
    begin
      point[0].x := xbase - LENGTH_OF_ROOT_BRANCH;
      point[0].y := p.position.y +ybase;
      point[1].x := p.position.x +xbase;
      point[1].y := point[0].y;
      ConvertToClientCoords(point, clientCoords, 2);
      MPolyLine(slice(clientCoords,2));
    end;
  end
  else
  begin
    if (p.anc.attrindex <> p.attrindex) then
    begin
      if FMyNodeAttrib.IsImage and FMyNodeAttrib.ShowImage then
         DrawBitmap(p, FMyNodeAttrib);
      if (not p.compressed) and (FMyNodeAttrib.ShowLabel or FMyNodeAttrib.ShowBracket) then
        DrawCaption(p, FMyNodeAttrib);
    end;

    if not p.hidden then
    begin
      if p.OTU or p.compressed then
      begin
        DrawOTUName(p);
      end;
      if (not p.OTU) and p.compressed and (not Topoflag) then
        DrawCompressedArea(p, FMyNodeAttrib);
      if ((p.anc.attrindex <> p.attrindex) and (GetNodeAttrib(p).BranchOption = boNoBranch)) or
         ((p.anc.attrindex = p.attrindex) and (GetNodeAttrib(p).BranchOption = boBranchOnly)) then
        ChangeAttrib(FAttribIndex);
      p2 := p;
      if Topoflag then
      begin
        q := p;
        if p = p.anc.des1 then
        begin
          while not (p2.OTU or p2.compressed) do p2 := p2.des1;
          if p.position.y < p.anc.position.y then
            while (q <> Root) and (q.position.y <= q.anc.position.y) do
            begin
              if q = q.anc.des2 then
              begin
                q := q.anc;
                Break;
              end;
              q := q.anc;
            end
          else
            q := q.anc;
        end
        else
        begin
          while not (p2.OTU or p2.compressed) do p2 := p2.des2;
          if p.position.y > p.anc.position.y then
            while (q <> Root) and (q.position.y >= q.anc.position.y) do
            begin
              if q = q.anc.des1 then
              begin
                q := q.anc;
                Break;
              end;
              q := q.anc;
            end
          else
            q := q.anc;
        end;
      end
      else
        q := p.anc;
      x0 := q.position.x;
      y0 := q.position.y;
      x1 := p2.position.x;
      y1 := p2.position.y;
      xDelta := p.position.x - p.anc.position.x;
      yDelta := abs(p.position.y - p.anc.position.y);
      if (xDelta < 8) or (yDelta < 8) then
      begin
        point[0].x := p.anc.position.x +xbase;
        point[0].y := p.anc.position.y +ybase;
        point[1].x := p.position.x +xbase;
        point[1].y := p.position.y +ybase;
        ConvertToClientCoords(point, clientCoords, 2);
        MPolyLine(slice(clientCoords,2));
      end
      else
      begin
        x2 := round((2*x0 +x1)/3);
        y2 := round((2*y0 +y1)/3);
        r := abs(y1-y0)*sqrt(3)/(x1-x0);
        n := ceil(xDelta/9);
        if n < 3 then
          n := 3
        else if n > 200 then
          n := 200;
        if y0 > y1 then
        begin
          for i := 1 to 3*n-1 do
          begin
            point[i].x := p.anc.position.x +round(xDelta/n/3*i);
            if point[i].x < x2 then
              point[i].y := round(1/3*y0 +2/3*y1 +sqrt(4/9*(y1-y0)*(y1-y0)-r*r*(point[i].x-x0)*(point[i].x-x0)))
            else if point[i].x = x2 then
              point[i].y := y2
            else
              point[i].y := round(4/3*y0 -1/3*y1 -sqrt(16/9*(y1-y0)*(y1-y0) -r*r*(point[i].x-x1)*(point[i].x-x1)));
          end;
        end
        else
        begin
          for i := 1 to 3*n-1 do
          begin
            point[i].x := p.anc.position.x +round(xDelta/n/3*i);
            if point[i].x < x2 then
              point[i].y := round(1/3*y0 +2/3*y1 -sqrt(4/9*(y1-y0)*(y1-y0)-r*r*(point[i].x-x0)*(point[i].x-x0)))
            else if point[i].x = x2 then
              point[i].y := y2
            else
              point[i].y := round(4/3*y0 -1/3*y1 +sqrt(16/9*(y1-y0)*(y1-y0) -r*r*(point[i].x-x1)*(point[i].x-x1)));
          end;
        end;
        point[0].x := p.anc.position.x;//x0;
        point[0].y := p.anc.position.y;//y0;
        point[3*n].x := p.position.x;//x1;
        point[3*n].y := p.position.y;//y1;
        for i := 0 to 3*n do
        begin
          point[i].x := point[i].x +xbase;
          point[i].y := point[i].y +ybase;
        end;
        ConvertToClientCoords(point, clientCoords, 3*n+1);
        MPolyBezier(clientCoords, 3*n+1, False, True);
      end;
      if FCurAttrib <> GetNodeAttrib(p) then
        ChangeAttrib(p.anc.attrindex);
    end;
  end;
end;

procedure TTreeCustomControl.DrawBranchCircle(p: TpNode);
var
  q: TpNode;
  d0,d1,d2, n: integer;
  point : array[0..2] of TPoint;
  clientCoords: array[0..2] of TPoint;
  angleStart, angleLength: Integer;
begin
  if not NeedsPainting(p) then
    Exit;
  Canvas.Pen := GetBranchPen;
  if p = Root then
  begin
    if (isRooted and ShowRoot) then
    begin
      point[0].x := p.position.x - round(cos(p.angle)*100) + xbase;
      point[0].y := p.position.y + round(sin(p.angle)*100) + ybase;
      point[1].x := p.position.x + xbase;
      point[1].y := p.position.y + ybase;
      ConvertToClientCoords(point, clientCoords, 2);
      MPolyLine(slice(clientCoords,2));
    end
  end;
  d1 := 0;
  d2 := 0;
  if not p.hidden then
  begin
    if p.OTU or p.compressed then
    begin
      if HorzTaxonName then
        DrawOTUName(p)
      else
        DrawOTUNameRadiation(p);
      if (not p.OTU) and p.compressed and (not Topoflag) then
        DrawCompressedAreaRadiation(p);
    end
    else
    begin
      q := p.des1;
      while (not (q.OTU or q.compressed)) and q.hidden do
        q := q.des1;

      if ((q.attrindex <> p.attrindex) and
          (GetNodeAttrib(q).BranchOption = boFullBranch)) then
        ChangeAttrib(q.attrindex)
      else if p.attrindex <> FAttribIndex then
        ChangeAttrib(FAttribIndex);

      n := 3;
      point[0].x := Origin.x - Round(p.avglen) + xbase;
      point[0].y := Origin.y - Round(p.avglen) + ybase;
      point[1].x := Origin.x + Round(p.avglen) + xbase;
      point[1].y := Origin.y + Round(p.avglen) + ybase;
      angleStart := Round(p.angle*16*180/PI); { convert radians to 1/16th degrees}
      angleLength := Round(q.angle*16*180/PI - angleStart);
      point[2].x := angleStart;
      point[2].y := angleLength;
      ConvertToClientCoords(point, clientCoords, n);
      MDrawArc(clientCoords[0].x, clientCoords[0].y, clientCoords[1].x, clientCoords[1].y, angleStart, angleLength);

      if FCurAttrib <> GetNodeAttrib(p) then
        ChangeAttrib(p.attrindex);
      q := p.des2;
      while not (q.OTU or q.compressed) and q.hidden do
        q := q.des2;
      if ((q.attrindex <> p.attrindex) and
          (GetNodeAttrib(q).BranchOption = boFullBranch)) then
        ChangeAttrib(q.attrindex)
      else if p.attrindex <> FAttribIndex then
        ChangeAttrib(FAttribIndex);

      n := 3;
      angleStart := Round(p.angle*16*180/PI);
      angleLength := Round(q.angle*16*180/PI - angleStart);
      point[2].x := angleStart;
      point[2].y := angleLength;
      ConvertToClientCoords(point, clientCoords, n);
      MDrawArc(clientCoords[0].x, clientCoords[0].y, clientCoords[1].x, clientCoords[1].y, angleStart, angleLength);

      if  FCurAttrib <> GetNodeAttrib(p) then
        ChangeAttrib(p.attrindex);
      if isStats and ShowStats then
        DrawStatsRadiation(p);
    end;
    if p <> Root then
    begin
      if ((p.attrindex <> FAttribIndex) and (GetNodeAttrib(p).BranchOption = boNoBranch)) or
         ((p.anc.attrindex = p.attrindex) and (GetNodeAttrib(p).BranchOption = boBranchOnly)) then
          ChangeAttrib(FAttribIndex);
      d0 := FCurAttrib.LineWidth*1;
      if d1 <> d0 then
        if d2 = d0 then
          d1 := d2
        else if d1 < d2 then
          d1 := d2;

      //point[0].x := p.position.x -round(cos(p.angle)*(p.avglen-p.anc.avglen-d0)) +xbase;
      //point[0].y := p.position.y +round(sin(p.angle)*(p.avglen-p.anc.avglen-d0)) +ybase;
      //point[1].x := p.position.x -round(cos(p.angle)*(d0-d1)) +xbase;
      //point[1].y := p.position.y +round(sin(p.angle)*(d0-d1)) +ybase;
      //point[0].x := p.position.x -round(cos(p.angle)*(p.avglen-p.anc.avglen-d0)) +xbase - 2*round(cos(p.angle));
      //point[0].y := p.position.y +round(sin(p.angle)*(p.avglen-p.anc.avglen-d0)) +ybase + 2*round(sin(p.angle));
      //point[1].x := p.position.x -round(cos(p.angle)*(d0-d1)) +xbase + 2*round(cos(p.angle));
      //point[1].y := p.position.y +round(sin(p.angle)*(d0-d1)) +ybase - 2*round(sin(p.angle));
      point[0].x := p.position.x -round(cos(p.angle)*(p.avglen-p.anc.avglen-d0)) +xbase - 1*round(cos(p.angle));
      point[0].y := p.position.y +round(sin(p.angle)*(p.avglen-p.anc.avglen-d0)) +ybase + 1*round(sin(p.angle));
      point[1].x := p.position.x -round(cos(p.angle)*(d0-d1)) +xbase + 1*round(cos(p.angle));
      point[1].y := p.position.y +round(sin(p.angle)*(d0-d1)) +ybase - 1*round(sin(p.angle));
      ConvertToClientCoords(point, clientCoords, 2);
      MPolyLine(slice(clientCoords,2));
    end;
    if  FCurAttrib <> GetNodeAttrib(p) then
      ChangeAttrib(p.attrindex);
  end;
end;

procedure TTreeCustomControl.DrawBranchStraight(p: TpNode);
var
  point : array[0..600] of TPoint;
  clientCoords: array[0..600] of TPoint;
  q: TpNode;
begin
  if not NeedsPainting(p) then
    Exit;
  Canvas.Pen := GetBranchPen;
  if p = Root then
  begin
    if (isRooted and ShowRoot) then
    begin
      point[0].x := xbase - LENGTH_OF_ROOT_BRANCH;
      point[1].y := p.position.y +ybase;
      point[1].x := p.position.x +xbase;
      if ShowTopologyOnly then
      begin
        q := p;
        if p.des1.cursize > p.des2.cursize then
          while not q.OTU do q := q.des1
        else
          while not q.OTU do q := q.des2;
        point[0].y := point[1].y -Round(100*(q.position.y-p.position.y)/(q.position.x-p.position.x));
       end
       else
         point[0].y := point[1].y;
      ConvertToClientCoords(point, clientCoords, 2);
      MPolyLine(slice(clientCoords,2));
    end;
  end
  else
  begin
    if (p.anc.attrindex <> p.attrindex) then
    begin
      if FMyNodeAttrib.IsImage and FMyNodeAttrib.ShowImage then
         DrawBitmap(p, FMyNodeAttrib);
      if (not p.compressed) and (FMyNodeAttrib.ShowLabel or FMyNodeAttrib.ShowBracket) then
        DrawCaption(p, FMyNodeAttrib);
    end;
    if not p.hidden then
    begin
      if p.OTU or p.compressed then
        DrawOTUName(p);
      if (not p.OTU) and p.compressed and (not Topoflag) then
        DrawCompressedArea(p, FMyNodeAttrib);
      if ((p.anc.attrindex <> p.attrindex) and (GetNodeAttrib(p).BranchOption = boNoBranch)) or
         ((p.anc.attrindex = p.attrindex) and (GetNodeAttrib(p).BranchOption = boBranchOnly)) then
        ChangeAttrib(FAttribIndex);
      point[0].x := p.anc.position.x +xbase;
      point[0].y := p.anc.position.y +ybase;
      point[1].x := p.position.x +xbase;
      point[1].y := p.position.y +ybase;
      ConvertToClientCoords(point, clientCoords, 2);
      MPolyLine(slice(clientCoords,2));
      if FCurAttrib <> GetNodeAttrib(p) then
        ChangeAttrib(p.anc.attrindex);
    end;
  end;
end;

procedure TTreeCustomControl.DrawBranchRadiation(p: TpNode);
var
  point : array[0..600] of TPoint;
  clientCoords: array[0..600] of TPoint;
begin
  if not NeedsPainting(p) then
    Exit;
  Canvas.Pen := GetBranchPen;
  if p = Root then
  begin
    if (isRooted and ShowRoot) then
    begin
      point[0].x := xbase - LENGTH_OF_ROOT_BRANCH;
      point[0].y := p.position.y +ybase;
      point[1].x := p.position.x +xbase;
      point[1].y := p.position.y +ybase;
      ConvertToClientCoords(point, clientCoords, 2);
      MPolyLine(slice(clientCoords,2));
    end
  end
  else if not p.hidden then
  begin
    if p.OTU or p.compressed then
      if HorzTaxonName then
      begin
        DrawOTUName(p);
      end
      else
        DrawOTUNameRadiation(p);

      if not (p.OTU or p.hidden) and isStats and ShowStats then
        DrawStatsRadiation(p);

    if (not p.OTU) and p.compressed and (not Topoflag) then
      DrawCompressedAreaRadiation(p);

    if ((p.attrindex <> FAttribIndex) and (GetNodeAttrib(p).BranchOption = boNoBranch)) or
       ((p.anc.attrindex = p.attrindex) and (GetNodeAttrib(p).BranchOption = boBranchOnly)) then
      ChangeAttrib(FAttribIndex);
    point[0].x := p.anc.position.x +xbase;
    point[0].y := p.anc.position.y +ybase;
    point[1].x := p.position.x +xbase;
    point[1].y := p.position.y +ybase;
    ConvertToClientCoords(point, clientCoords, 2);
    MPolyLine(slice(clientCoords,2));
    if FCurAttrib <> GetNodeAttrib(p) then
      ChangeAttrib(p.attrindex);
  end;
end;

procedure TTreeCustomControl.DrawMarkerAngle;
var
    size,x,y,d : integer;
    dwPenWidth : Integer;

    procedure DrawMarkerAngleNode(p: TpNode);
    begin
      if not (p.OTU or p.compressed) then
      begin
        DrawMarkerAngleNode(p.des1);
        DrawMarkerAngleNode(p.des2);
      end;

      if GetTaxonMarker(p).Shape = msNone then exit;

      if p.OTU then
        size := Abs(GetNodeAttrib(p).Font.Height)
      else
        size := Abs(GetNodeAttrib(p).CaptionFont.Height);

      d    := GetLineWidth(p)*2;
      if (fsBold in GetNodeAttrib(p).Font.Style) then
          dwPenWidth := size*2 div 9
      else
          dwPenWidth := size div 9;
      UpdatePen(FMarkerPen, psSolid, dwPenWidth, p.Marker.Color, pecRound, pjsMiter);
      UpdateBrush(FFillBrush, bsSolid, p.Marker.Color);
      UpdateBrush(FOpenBrush, bsSolid, clWhite);

      if p.OTU then
      begin
        x := p.position.x +Round(Cos(p.angle)*(size+d)) +xbase;
        y := p.position.y -Round(Sin(p.angle)*(size+d)) +ybase;
      end
      else if (not p.OTU) and p.compressed then
      begin
        x := (p.des1.position.x +p.des2.position.x) div 2 +Round(Cos(p.angle)*(size+d)) +xbase;
        y := (p.des1.position.y +p.des2.position.y) div 2 -Round(Sin(p.angle)*(size+d)) +ybase;
      end
      else
      begin
        x := p.position.x +xbase;
        y := p.position.y +ybase;
      end;
      case p.Marker.Shape of
        msOpenCircle    : DrawCircle(x, y, (size div 4), FMarkerPen, FOpenBrush);
        msFilledCircle  : DrawCircle(x, y, (size div 4), FMarkerPen, FFillBrush);
        msOpenSquare    : DrawSquare(x, y, (size div 2), p.angle, FMarkerPen, FOpenBrush);
        msFilledSquare  : DrawSquare(x, y, (size div 2), p.angle, FMarkerPen, FFillBrush);
        msOpenDiamond   : DrawSquare(x, y, Round(size/Sqrt(6)), p.angle+PI/4, FMarkerPen, FOpenBrush);
        msFilledDiamond : DrawSquare(x, y, Round(size/Sqrt(6)), p.angle+PI/4, FMarkerPen, FFillBrush);
        msOpenUpTriangle  : DrawTriangle(x, y, (size div 2), p.angle-PI/2, FMarkerPen, FOpenBrush);
        msFilledUpTriangle: DrawTriangle(x, y, (size div 2), p.angle-PI/2, FMarkerPen, FFillBrush);
        msOpenDownTriangle  : DrawTriangle(x, y, (size div 2), p.angle+PI/2, FMarkerPen, FOpenBrush);
        msFilledDownTriangle: DrawTriangle(x, y, (size div 2), p.angle+PI/2, FMarkerPen, FFillBrush);
      end;
    end;

begin
  DrawMarkerAngleNode(Root);
end;

procedure TTreeCustomControl.DrawMarker;
var
    size, x, y, dx, dy : integer;
    dwPenWidth : Integer;

    procedure DrawMarkerNode(p: TpNode);
    begin
      if not (p.OTU or p.compressed) then { pre-order traversal}
      begin
        DrawMarkerNode(p.des1);
        DrawMarkerNode(p.des2);
      end;

      if (p.marker.Shape = msNone) or (not NeedsPainting(p)) then { quick exit if nothing to draw}
        Exit;
      if p.OTU or p.compressed then { handle tips which are a simpler case}
      begin
        if GetTaxonMarker(p).Shape = msNone then { GetTaxonMarker gets the appropriate node or group marker}
          exit;
      end
      else if not GetNodeAttrib(p).ShowNodeMarker then {another simple case - non-tips that are not displaying markers}
        exit;

      if IsGeneDups then { then we need to draw it in DrawGeneDupsMarker which doesn't get confused by group info}
      begin
        if p.IsGeneDuplication then
          Exit;
        if p.IsSpeciationEvent then
          Exit;
      end;

      { get the correct font height here}
      if p.OTU and (not p.compressed) then
        size := Abs(GetNodeAttrib(p).Font.Height)
      else
        size := Abs(GetNodeAttrib(p).CaptionFont.Height);

      { set the appropriate pen width}
      if p.OTU and (not p.compressed) then
        if (fsBold in GetNodeAttrib(p).Font.Style) then
          dwPenWidth := size*2 div 9
        else
          dwPenWidth := size div 9
      else
        if (fsBold in GetNodeAttrib(p).CaptionFont.Style) then
          dwPenWidth := size*2 div 9
        else
          dwPenWidth := size div 9;
      if (p.marker.Shape = msOpenDiamond) and (p.IsSpeciationEvent) then { makes the open diamonds easier to see}
        dwPenWidth := dwPenWidth * 2;

      UpdatePen(FMarkerPen, psSolid, dwPenWidth, p.Marker.Color, pecSquare, pjsMiter);
      UpdateBrush(FFillBrush, bsSolid, p.Marker.Color);
      UpdateBrush(FOpenBrush, bsSolid, clwhite);

      dx := size +GetLineWidth(p)*2;
      //if GetNodeAttrib(p).ShowTaxonName or GetNodeAttrib(p).ShowSpeciesName then
      //  dy := -size div 6
      //else
        dy := 0;

      if p.OTU or p.compressed then { handle position for tips}
      begin
        if p.OTU and ShowCharState then
          dx := dx +Canvas.TextWidth(p.charstate) +Abs(CharStateFont.Height)*2;
        if p.OTU or Topoflag then
          x := p.position.x +dx +xbase
        else
          x := p.des1.position.x +dx +xbase;
        if not p.OTU then
          y := (p.des1.position.y+p.des2.position.y) div 2 +dy +ybase
        else
          y := p.position.y +dy +ybase;
      end
      else { handle position for internal nodes}
      begin
        x := p.position.x +xbase;
        y := p.position.y +ybase;
      end;

      case p.Marker.Shape of
        msOpenCircle    : DrawCircle(x, y, (size div 4), FMarkerPen, FOpenBrush);
        msFilledCircle  : DrawCircle(x, y, (size div 4), FMarkerPen, FFillBrush);
        msOpenSquare    : DrawSquare(x, y, (size div 2), 0.0, FMarkerPen, FOpenBrush);
        msFilledSquare  : DrawSquare(x, y, (size div 2), 0.0, FMarkerPen, FFillBrush);
        msOpenDiamond   : DrawSquare(x, y, Round(size/Sqrt(6)), PI/4, FMarkerPen, FOpenBrush);
        msFilledDiamond : DrawSquare(x, y, Round(size/Sqrt(6)), PI/4, FMarkerPen, FFillBrush);
        msOpenUpTriangle  : DrawTriangle(x, y+round(size/6), (size div 2), -PI/2, FMarkerPen, FOpenBrush);
        msFilledUpTriangle: DrawTriangle(x, y+round(size/6), (size div 2), -PI/2, FMarkerPen, FFillBrush);
        msOpenDownTriangle  : DrawTriangle(x, y-dy-round(size/6), (size div 2), PI/2, FMarkerPen, FOpenBrush);
        msFilledDownTriangle: DrawTriangle(x, y-dy-round(size/6), (size div 2), PI/2, FMarkerPen, FFillBrush);
      end;
    end;

begin
  Canvas.Font.Assign(CharStateFont);
  Canvas.Brush.Style := bsClear;
  DrawMarkerNode(Root);
end;

procedure TTreeCustomControl.DrawSquare(x, y, r: integer; angle: double; aPen: TPen; aBrush: TBrush);
var
  point : array[0..3] of TPoint;
  clientCoords: array[0..3] of TPoint;
begin
  point[0].x := Round(x +r*Sqrt(2)*Cos(angle +PI/4));
  point[0].y := Round(y -r*Sqrt(2)*Sin(angle +PI/4));
  point[1].x := Round(x +r*Sqrt(2)*Cos(angle +3*PI/4));
  point[1].y := Round(y -r*Sqrt(2)*Sin(angle +3*PI/4));
  point[2].x := Round(x +r*Sqrt(2)*Cos(angle -3*PI/4));
  point[2].y := Round(y -r*Sqrt(2)*Sin(angle -3*PI/4));
  point[3].x := Round(x +r*Sqrt(2)*Cos(angle -PI/4));
  point[3].y := Round(y -r*Sqrt(2)*Sin(angle -PI/4));
  ConvertToClientCoords(point, clientCoords, 4);
  Canvas.Pen := aPen;
  Canvas.Brush := aBrush;
  MPolygon(slice(clientCoords, 4))
end;

procedure TTreeCustomControl.DrawTriangle(x, y, r: integer; angle: double; aPen: TPen; aBrush: TBrush);
var
  point : array[0..2] of TPoint;
  clientCoords : array[0..2] of TPoint;
begin
  point[0].x := Round(x +r*(1/3*Cos(angle) +4/3*Cos(angle +PI/3)));
  point[0].y := Round(y -4/3*r*Sin(angle +PI/3));
  point[1].x := Round(x +r*(1/3*Cos(angle) +4/3*Cos(angle +PI)));
  point[1].y := Round(y -4/3*r*Sin(angle +PI));
  point[2].x := Round(x +r*(1/3*Cos(angle) +4/3*Cos(angle -PI/3)));
  point[2].y := Round(y -4/3*r*Sin(angle -PI/3));
  Canvas.Pen := aPen;
  Canvas.Brush := aBrush;
  ConvertToClientCoords(point, clientCoords, 3);
  MPolygon(slice(clientCoords, 3));
end;

procedure TTreeCustomControl.DrawCircle(x, y, r: integer; aPen: TPen; aBrush: TBrush);
var
  boxCoords: TRect;
  clientCoords: TRect;
begin
  clientCoords := Rect(0, 0, 0, 0);
  BoxCoords.Left := x-r-4;
  BoxCoords.Top := y-r-4;
  BoxCoords.Right := x+r+4;
  BoxCoords.Bottom := y+r+4;
  Canvas.Pen := aPen;
  Canvas.Brush := aBrush;
  ConvertToClientCoords(boxCoords, clientCoords);
  MDrawCircle(clientCoords);
end;

procedure TTreeCustomControl.DrawGeneDupMarker;
var
  size, x, y, dy : integer;
  dwPenWidth : DWORD;

  procedure DrawGeneDupMarkerNode(p: TpNode);
  begin
    { traverse post-order}
    if not (p.OTU or p.compressed) then
    begin
      DrawGeneDupMarkerNode(p.des1);
      DrawGeneDupMarkerNode(p.des2);
    end;
    { first, handle the cases where we should exit}
    if (not NeedsPainting(p)) or ((p.IsGeneDuplication = False) and (p.IsSpeciationEvent = False)) then
      Exit;
    if p.OTU then
      Exit;
    if p.IsGeneDuplication and (not FShowGeneDupMarkers) then
      Exit;
    if p.IsSpeciationEvent and (not FShowSpeciationMarkers) then
      Exit;

    { Set marker shape and color. Safest to do it here, otherwise it may get fubarred by group attributes}
    if p.IsGeneDuplication then
    begin
      p.marker.shape := msFilledDiamond;
      p.marker.color := GENE_DUP_MARKER_COLOR;
    end
    else if p.IsSpeciationEvent then
    begin
      p.marker.shape := msOpenDiamond;
      p.marker.color := SPECIATION_MARKER_COLOR;
    end
    else
    begin
      p.marker.shape := msNone;
      p.marker.color := clWhite;
    end;

    { set up the drawing tools}
    size := Abs(GetNodeAttrib(p).CaptionFont.Height);
    if (fsBold in GetNodeAttrib(p).CaptionFont.Style) then
      dwPenWidth := size*2 div 9 + 1
    else
      dwPenWidth := size div 9 + 1;
    if (p.marker.Shape = msOpenDiamond) and (p.IsSpeciationEvent) then { makes the open diamonds easier to see}
      dwPenWidth := dwPenWidth * 2;
    UpdatePen(FMarkerPen, psSolid, dwPenWidth, p.Marker.Color, pecRound, pjsMiter);
    Canvas.Pen := FMarkerPen;
    UpdateBrush(FFillBrush, bsSolid, p.Marker.Color);
    UpdateBrush(FFillBrush, bsSolid, clWhite);

    { setup the coordinates}
    dy := 0;
    x := p.position.x +xbase;
    y := p.position.y +ybase;

    {draw the marker}
    case p.Marker.Shape of
      msOpenCircle    : DrawCircle(x, y, (size div 2), FMarkerPen, FOpenBrush);
      msFilledCircle  : DrawCircle(x, y, (size div 2), FMarkerPen, FFillBrush);
      msOpenSquare    : DrawSquare(x, y, (size div 2), 0.0, FMarkerPen, FOpenBrush);
      msFilledSquare  : DrawSquare(x, y, (size div 2), 0.0, FMarkerPen, FFillBrush);
      msOpenDiamond   : DrawSquare(x, y, Round(size/Sqrt(6)), PI/4, FMarkerPen, FOpenBrush);
      msFilledDiamond : DrawSquare(x, y, Round(size/Sqrt(6)), PI/4, FMarkerPen, FFillBrush);
      msOpenUpTriangle  : DrawTriangle(x, y+round(size/6), (size div 2), -PI/2, FMarkerPen, FOpenBrush);
      msFilledUpTriangle: DrawTriangle(x, y+round(size/6), (size div 2), -PI/2, FMarkerPen, FFillBrush);
      msOpenDownTriangle  : DrawTriangle(x, y-dy-round(size/6), (size div 2), PI/2, FMarkerPen, FOpenBrush);
      msFilledDownTriangle: DrawTriangle(x, y-dy-round(size/6), (size div 2), PI/2, FMarkerPen, FFillBrush);
    end;
    { cleanup}
    p.marker.shape := msNone; { otherwise drawmarker might redraw it in the case of switching between multiple trees}
    p.marker.color := clWhite;
  end;

begin
  Canvas.Font.Assign(CharStateFont);
  Canvas.Brush.Style := bsClear;
  DrawGeneDupMarkerNode(Root);
end;

procedure TTreeCustomControl.ChangeAttrib(i: Integer);
var
  penWidth: Integer;
begin
  Assert(AttribList.Count > 0, 'Attrib list not initialized');
  if i < 0 then
    FMyNodeAttrib := GroupAttrib[-(i+1)]
  else
    FMyNodeAttrib := AttribList[i];
  FCurAttrib := FMyNodeAttrib;
  Assert(Assigned(FCurAttrib));
  if FCurAttrib.LineStyle <> psSolid then
    FCurAttrib.LineWidth := 1;
  penWidth := FCurAttrib.LineWidth;
  if (TreeStyle = tsTraditional) then begin
    case BranchStyle of
      bsRectangular, bsCurved:
        UpdatePen(BranchPen, FCurAttrib.LineStyle, penWidth, FCurAttrib.LineColor, pecSquare, pjsMiter);
      bsStraight:
        UpdatePen(BranchPen, FCurAttrib.LineStyle, penWidth, FCurAttrib.LineColor, pecRound, pjsMiter);
    end
  end
  else if TreeStyle = tsCircle then
    UpdatePen(BranchPen, FCurAttrib.LineStyle, penWidth, FCurAttrib.LineColor, pecSquare, pjsMiter)
  else if TreeStyle = tsRadiation then
    UpdatePen(BranchPen, FCurAttrib.LIneStyle, penWidth, FCurAttrib.LineColor, pecRound, pjsMiter);

  UpdatePen(EdgePen, FCurAttrib.LineStyle, penWidth, FCurAttrib.LineColor, pecSquare, pjsMiter);
end;

procedure TTreeCustomControl.CopyImageToClipBoard;
{$IFNDEF MSWINDOWS}
var
  bitmap: TBitmap = nil;
  aRect: TRect;
  sFormats: TStringList = nil;
  formatId: Integer;
{$ENDIF}
begin
  Clipboard.Clear;
  if TEdit(EditBox).Focused then
    TEdit(EditBox).CopyToClipboard
  else
  {$IFDEF MSWINDOWS}
    begin
      OpenClipboard(0);
        EmptyClipboard;
        MakeMetaFile;
        SetClipboardData(CF_ENHMETAFILE, hMetaFile);
        CloseClipboard;
    end;
     {$ELSE}
     begin
       try
         FDrawFullTree := True;
         aRect := Rect(0, 0, MinWidth*4, MinHeight*4);
         bitmap := TBitmap.Create;
         bitmap.PixelFormat := GetPixelFormat(aRect);
         bitmap.SetSize(RectWidth(aRect), RectHeight(aRect));
         DrawTree(bitmap.Canvas);

         {$IFNDEF MSWINDOWS} { on Linux the clipboard was often blank - this seems to fix the issue but not sure how or why or even if this is reliable}
         sFormats := TStringList.Create;
         Clipboard.SupportedFormats(sFormats);
         sFormats.Free;
         formatId := Clipboard.FindPictureFormatID;
         {$ENDIF}
         Clipboard.Assign(bitmap);
       finally
         FDrawFullTree := False;
         if Assigned(bitmap) then
           bitmap.Free;
       end;
     end;
     {$ENDIF}
end;

function TTreeCustomControl.WriteTreeInTableFormat: TStringList;
var
  temp: String;
  i: Integer;
  n: TpNode = nil;

  function LabelString(aNode: TpNode): String;
  begin
    if aNode.OTU then
      Result := aNode.name
    else
      Result := IntToStr(aNode.Index);
  end;

begin
  Result := TStringList.Create;
  temp := 'AncId, Desc1, Desc2';
  if IsBranchLength then
    temp += ', Branch Length 1, Branch Length 2';
  Result.Add(temp);
  for i := NoOfOTUs + 1 to NoOfNodes do
  begin
    n := Node[i];
    temp := Format('%d, %s, %s', [n.index, LabelString(n.des1), LabelString(n.des2)]);
    if IsBranchLength then
      temp += Format(', %.10f, %.10f', [n.des1.branch.length, n.des2.branch.length]);
    Result.Add(temp);
  end;
end;

procedure TTreeCustomControl.Print;
var
  bitmap: TBitmap = nil;
  sourceRect : TRect;
  targetRect: TRect;
  aTop, aLeft: Integer;
  px, py, dx, dy, i, j, nx, ny : integer;
begin
  try
    FDrawFullTree := True;
    sourceRect := Rect(0, 0, MinWidth*4, MinHeight*4);
    bitmap := TBitmap.Create;
    bitmap.PixelFormat := GetPixelFormat(sourceRect);
    bitmap.SetSize(RectWidth(sourceRect), RectHeight(sourceRect));
    aTop := ScrollTop;
    aLeft := ScrollLeft;
    DrawTree(bitmap.Canvas);
    ScrollTo(aLeft, aTop);
    Printer.BeginDoc;
    dx := ScreenInfo.PixelsPerInchX;
    dy := ScreenInfo.PixelsPerInchY;
    px := Printer.XDPI;
    py := Printer.YDPI;
    px := MulDiv(MinWidth*4, px, dx);   //Adjust the # pixels to print out on X axis depending on DPI previously determined
    py := MulDiv(MinHeight*4, py, dy);  //Adjust the # pixels to print out on Y axis depending on DPI previously determined

    nx := px div (Printer.PageWidth-125);  //Determine number of pages horizontally needed to print (on x axis) -1 (starts on page 0)
    ny := py div (Printer.PageHeight-125); //Determine number of pages vertically needed to print (on y axis) -1 (starts on page 0)
    for i := 0 to nx do
    begin
      if i = 0 then
        targetRect.left := 125
      else
        targetRect.left := 125 -i*(Printer.PageWidth-125); //Horizontal Offset Compensating for multiple horizontal pages
      targetRect.right := targetRect.left +px; //The # of pixels per line,  per page is px, so we determine where on the EMF to print for this page horizontally
      for j := 0 to ny do
      begin
          if j = 0 then
            targetRect.top := 125
          else
            targetRect.top := 125 -j*(Printer.PageHeight-125);  //Vertical offset compensating for multiple vertical pages
          targetRect.bottom := targetRect.top +py;
          Printer.Canvas.CopyRect(targetRect, bitmap.Canvas, sourceRect);
          if (i = nx) and (j = ny) then
            break
          else
            Printer.NewPage;
      end;
    end;
  finally
    FDrawFullTree := False;
    Printer.EndDoc;
    if Assigned(bitmap) then
      bitmap.Free;
  end;
end;

procedure TTreeCustomControl.PrintSmall;
var
  targetRect: TRect;
  sourceRect: TRect;
  f,fx,fy: double;
  px, py, dx, dy: integer;
  bitmap: TBitmap = nil;
begin
  dx := ScreenInfo.PixelsPerInchX;
  dy := ScreenInfo.PixelsPerInchY;
  px := Printer.XDPI;
  py := Printer.YDPI;
  px := MulDiv(MinWidth*4, px, dx);
  py := MulDiv(MinHeight*4, py, dy);

  if (Printer.PageWidth-300) < px then
    fx := (Printer.PageWidth-300)/px
  else
    fx := 1.0;
  if (Printer.PageHeight-300) < py then
    fy := (Printer.PageHeight-300)/py
  else
    fy := 1.0;

  if fx < fy then
    f := fx
  else
    f := fy;
  targetRect.left := (Printer.PageWidth -Round(px*f)) DIV 2;
  targetRect.top := (Printer.PageHeight -Round(py*f)) DIV 2;
  targetRect.right := Round(px*f) + targetRect.left;
  targetRect.bottom := Round(py*f) + targetRect.top;
  try
    FDrawFullTree := True;
    sourceRect := Rect(0, 0, MinWidth*4, MinHeight*4);
    bitmap := TBitmap.Create;
    bitmap.PixelFormat := GetPixelFormat(sourceRect);
    bitmap.SetSize(RectWidth(sourceRect), RectHeight(sourceRect));
    DrawTree(bitmap.Canvas);
    Printer.BeginDoc;
    Printer.Canvas.CopyRect(targetRect, bitmap.Canvas, sourceRect);
  finally
    FDrawFullTree := False;
    Printer.EndDoc;
    if Assigned(bitmap) then
      bitmap.Free;
  end;
end;

function TTreeCustomControl.FindMrca(TaxonA: Integer; TaxonB: Integer): Integer;
var
  i, j: Integer;
  TaxonAAncestors, TaxonBAncestors: Array of Integer;
  TempNode: TpNode;
begin
  SetLength(TaxonAAncestors, 0);
  SetLength(TaxonBAncestors, 0);
  Result := -1;

  // find all ancestors of TaxonA.
  TempNode := Node[TaxonA];
  while TempNode <> Root do
  begin
    TempNode := TempNode.anc;
    SetLength(TaxonAAncestors, Length(TaxonAAncestors) + 1);
    TaxonAAncestors[Length(TaxonAAncestors) - 1] := TempNode.index;
  end;

  // find all ancestors of TaxonB
  TempNode := Node[TaxonB];
  while TempNode <> Root do
  begin
    TempNode := TempNode.anc;
    SetLength(TaxonBAncestors, Length(TaxonBAncestors) + 1);
    TaxonBAncestors[Length(TaxonBAncestors) - 1] := TempNode.index;
  end;

  // Look for an intersection, the intersection between taxaAParents and taxaBParents is the most recent common ancestor
  for i := 0 to Length(TaxonAAncestors) - 1 do
  begin
    for j := 0 to Length(TaxonBAncestors) - 1 do
      if TaxonAAncestors[i] = TaxonBAncestors[j] then
      begin
        Result := TaxonAAncestors[i];
        Exit;
      end;
  end;
end;

function TTreeCustomControl.FindMrca(nameA: String; nameB: String): Integer;
var
  nodeA, nodeB: Integer;
begin
  nodeA := FindNodeId(nameA);
  nodeB := FindNodeId(nameB);
  Result := FindMrca(nodeA, nodeB);
end;

function TTreeCustomControl.FindMrca(taxa: TStringList): Integer;
var
  ids: TLongintList = nil;
  aId: Integer = -1;
  aName: String = '';
  i: Integer = -1;
begin
  try
    ids := TLongintList.Create;
    if taxa.Count > 0 then
      for i := 0 to taxa.Count - 1 do
      begin
        aName := taxa[i];
        TrimTaxaName2(aName);
        aId := FindNodeId(aName);
        if aId >= 0 then
          ids.Add(aId)
        else
          raise Exception.Create('failed to find ' + aName + ' in the tree');
      end;
    Result := FindMrca(ids);
  finally
    if Assigned(ids) then
      ids.Free;
  end;
end;

function TTreeCustomControl.FindMrca(taxaIDs: TLongintList): Integer;

  function GetKeysCount(aNode: TpNode; keys: TLongIntList; matchingNodes: Integer; var ancestors: TLongIntList): Integer;
  begin
    if not Assigned(aNode) then
      Exit(0);

    matchingNodes := matchingNodes + GetKeysCount(aNode.des1, keys, matchingNodes, ancestors) + GetKeysCount(aNode.des2, keys, matchingNodes, ancestors);

    if keys.HasValue(aNode.Index) then
      inc(matchingNodes);
    if matchingNodes = keys.Count then
      ancestors.Add(aNode.Index);
    Result := matchingNodes;
  end;

  function LcaOfNodes(aNode: TpNode; keys: TLongIntList): Integer;
  var
    ancestors: TLongIntList = nil;
    matchingNodes: Integer = 0;
  begin
     try
       ancestors := TLongIntList.Create;
       GetKeysCount(aNode, keys, matchingNodes, ancestors);
       if ancestors.Count > 0 then
         Result := ancestors[0]
       else
         Result := -1;
     finally
       if Assigned(ancestors) then
         ancestors.Free;
     end;
  end;

begin
  Result := LcaOfNodes(Root, taxaIDs);
end;

function TTreeCustomControl.FindNodeId(TaxonName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to FNoOfOtus do
  begin
    if SameText(TaxonName, Node[i].name) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TTreeCustomControl.FindTwoExtantTaxa(MrcaId: Integer; var TaxonA: Integer; var TaxonB: Integer): Boolean;
var
  Index: Integer;

  function FindFirstTaxon: Integer;
  begin
    Result := -1;
    Index := MrcaId;
    while Index > FNoOfOtus do
    begin
      Index := Node[Index].des1.index;
    end;
    Result := Index;
  end;

  function FindSecondTaxon: Integer;
  begin
    Result := -1;
    Index := MrcaId;
    while Index > FNoOfOtus do
    begin
      Index := Node[Index].des2.index;
    end;
    Result := Index;
  end;

begin
  Result := False;
  TaxonA := FindFirstTaxon;
  TaxonB := FindSecondTaxon;
  if (TaxonA <> -1) and (TaxonB <> -1) and (TaxonA <= FNoOfOtus) and (TaxonB <= FNoOfOtus)then
    Result := True;
end;

function TTreeCustomControl.FindTwoExtantTaxaNames(MrcaId: Integer; var TaxonA: String; var TaxonB: String): Boolean;
var
  tA: Integer = -1;
  tB: Integer = -1;
begin
  Result := FindTwoExtantTaxa(MrcaId, tA, tB);
  if Result then
  begin
    TaxonA := GetOTUName(tA);
    TaxonB := GetOTUName(tB);
  end;
end;

function TTreeCustomControl.GetNodeLabelForFocusedNode(generateLabelIfNotExist: Boolean): String;
const
  MAX_NAME_LENGTH = 20;
var
  index: Integer = -1;
  taxonA: String = '';
  taxonB: String = '';
begin
  Result := EmptyStr;
  if (not NodeFocused) and (not BranchFocused) then
    Exit;
  if FTreeIndex <> 0 then
    Result := TreeList[FTreeIndex - 1].InternalNodeLabel[FocusedIndex - NoOfOTUs - 1];
  if Result <> EmptyStr then
    Exit;
  if generateLabelIfNotExist then
  begin
    if FindTwoExtantTaxaNames(FocusedIndex, taxonA, taxonB) then
    begin
      if Length(taxonA) > MAX_NAME_LENGTH then
        taxonA := Copy(taxonA, 1, MAX_NAME_LENGTH);
      if Length(taxonB) > MAX_NAME_LENGTH then
        taxonB := Copy(taxonB, 1, MAX_NAME_LENGTH);
      Result := Format('%s-%s', [taxonA, taxonB]);
      Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
    end;
  end;
end;

procedure TTreeCustomControl.FocusOnNode(index: integer);
begin
  if (index = FocusedIndex) and NodeFocused then Exit;
  ClearFocus;
  if (index <= 0) or (index > NoOfNodes) then Exit;
  FFocusedIndex := index;
  FNodeFocused := true;

  MarkedPos.X := Node[index].position.x;
  MarkedPos.Y := Node[index].position.y;

  if @FOnChange <> nil then FOnChange(Self);

  DrawFocus;
end;

procedure TTreeCustomControl.FocusOnOutgroup;
var
  i: Integer;
begin
  for i := FNoOfOTUs downto 1 do
  begin
    if Node[i].outgroup then
    begin
      FocusOnNode(i);
      break;
    end;
  end;
end;

procedure TTreeCustomControl.FocusOnBranch(nodeindex: integer);
begin
  if (nodeindex <= 0) or (nodeindex > NoOfNodes) then Exit;
  if (nodeindex = FocusedIndex) and BranchFocused then Exit;
  if Node[nodeindex] = Root then Exit;

  ClearFocus;

  FFocusedIndex := nodeindex;
  FBranchFocused := true;

  MarkedPos.X := (Node[nodeindex].anc.position.x +Node[nodeindex].position.x) div 2;
  MarkedPos.Y := (Node[nodeindex].anc.position.y +Node[nodeindex].position.y) div 2;

  if @FOnChange <> nil then FOnChange(Self);

  DrawFocus;
end;

procedure TTreeCustomControl.FocusOnTime(nodeindex: integer);
begin
  if (nodeindex = FocusedTimeIndex) and TimeFocused then Exit;
  ClearFocus;

  if (nodeindex <= 0) or (nodeindex > NoOfNodes) then Exit;
  if Node[nodeindex] = Root then Exit;

  FFocusedTimeIndex := nodeindex;
  FFocusedIndex := nodeIndex;
  FTimeFocused := true;
  FBranchFocused := true;

  if @FOnChange <> nil then FOnChange(Self);
end;

procedure TTreeCustomControl.FocusOnName(index: integer);
begin
  if (index <= 0) and (index > NoOfOTUs) then  Exit;
  if FocusName((Node[index].position.x +xbase) div 4, (Node[index].position.y +ybase) div 4) then
    StartUserEditingName;
end;

procedure TTreeCustomControl.ClearFocus;
begin
  if NodeFocused or BranchFocused then
  begin
    DrawFocus;
    Invalidate;     // KT 20181101: This is necessary to erase cursor
  end;

  FNodeFocused := false;
  FBranchFocused := false;
  FTimeFocused := false;
  FFocusedIndex := 0;
  FFocusedTimeIndex := 0;
  FFocusedNameIndex := 0;
end;

function TTreeCustomControl.GetNodeIndexAtCoords(x,y: integer):integer;
var
  i, x0,y0,d : Integer;
  boxX: Integer = -1;
  boxY: Integer = -1;
begin
  Result := 0;
  if NoOfNodes = 0 then Exit;

  ClientToBox(x, y, boxX, boxY);
    d := BranchPen.Width div 2 +7;

    for i := NoOfOTUs+1 to NoOfNodes do
    begin
      if Node[i].hidden then Continue;
      if (TreeStyle = tsCircle) then
      begin
        if (Node[i] <> Root) and (Node[i].avglen = Node[i].anc.avglen) then
          Continue
      end
      else if (Node[i] <> Root) then
        if Node[i].position.x = Node[i].anc.position.x then
          Continue;

      with Node[i]^ do
      begin
        x0 := position.x +xbase;
        y0 := position.y +ybase;

        if (abs(boxY-y0) < MAX_BITMAP_DIMENSION) and (Sqrt((boxX-x0)*(boxX-x0)+(boxY-y0)*(boxY-y0)) < d) then
          begin
              Result := i;
              Break;
          end;
      end;
    end;
end;

function  TTreeCustomControl.FocusNode(x, y : Int64):boolean;
var
  ni : Integer;
begin
    Result := false;
    if NoOfNodes = 0 then Exit;
    if (x <= 0) and (y <= 0) then
    begin
        ClearFocus;
        Exit;
    end;

    ni := GetNodeIndexAtCoords(x,y);
    FocusOnNode(ni);

    if ni > 0 then
     if Assigned(EditBox) and TEdit(EditBox).Visible then
      TEdit(EditBox).Hide;

    Result := NodeFocused;
    if Result then
      Invalidate;
end;

function TTreeCustomControl.GetBranchIndexAtCoords(x,y: integer):integer;
var
  i,x0,x1,x2,x3,y0,y1,y2,y3,dy,d : integer;
  boxX: Integer = -1;
  boxY: Integer = -1;
  q1,q2: TpNode;
  a: double;

    procedure GetCrossPoint;
    var
      c,b: double;
    begin
      c := (y1-y0)/(x1-x0);
      b := (x1*y0-x0*y1)/(x1-x0);
      x0 := round((y +x/c -b)/(c +1/c));
      y0 := round((c*y +x +b/c)/(c +1/c));
    end;
begin
  Result := 0;
  if NoOfNodes = 0 then Exit;

    ClientToBox(x, y, boxX, boxY);
    d := BranchPen.Width div 2 +5;

    for i := 1 to NoOfNodes do
    begin
        if Node[i] = Root then Continue;
        if Node[i].hidden then Continue;
        if TreeStyle = tsCircle then
        begin
          if (not Node[i].OTU) and (Node[i].avglen = Node[i].anc.avglen) then
            Continue;
        end
        else if TreeStyle <> tsRadiation then
          if (not Node[i].OTU) and (Node[i].position.x = Node[i].anc.position.x) then
            Continue;

        with Node[i]^ do
        begin
          if (TreeStyle = tsTraditional) and (BranchStyle = bsRectangular) then
          begin
              x0 := round((anc.position.x +position.x +2*xbase)/2);
              dx := round((position.x -anc.position.x)/2);
              if dx < d then
                dx := d;
              y0 := position.y +ybase;
              dy := d;
          end
          else if (TreeStyle = tsTraditional) and (BranchStyle = bsCurved) then
          begin
              if Topoflag then
              begin
                q1 := Node[i];
                if q1 = q1.anc.des1 then
                  if q1.position.y < q1.anc.position.y then
                    while (q1 <> Root) and (q1.position.y <= q1.anc.position.y) do
                    begin
                      if q1 = q1.anc.des2 then
                      begin
                        q1 := q1.anc;
                        Break;
                      end;
                      q1 := q1.anc;
                    end
                  else
                    q1 := q1.anc
                else
                  if q1.position.y > q1.anc.position.y then
                    while (q1 <> Root) and (q1.position.y >= q1.anc.position.y) do
                    begin
                      if q1 = q1.anc.des1 then
                      begin
                        q1 := q1.anc;
                        Break;
                      end;
                      q1 := q1.anc;
                    end
                  else
                    q1 := q1.anc;
                q2 := Node[i];
                if q2.position.y < q1.position.y then
                  while not (q2.OTU or q2.compressed) do q2 := q2.des1
                else
                  while not (q2.OTU or q2.compressed) do q2 := q2.des2;
              end
              else
              begin
                q1 := Node[i].anc;
                q2 := Node[i];
              end;
              x0 := round(q1.position.x +xbase);
              y0 := round(q1.position.y +ybase);
              x1 := round(q2.position.x +xbase);
              y1 := round(q2.position.y +ybase);
              x2 := round(anc.position.x +xbase);
              y2 := round(anc.position.y +xbase);
              x3 := round(position.x +xbase);
              y3 := round(position.y +xbase);
              if (x3-x2) < 2*d then
              begin
                  x0 := round((x2+x3)/2);
                  dx := d;
                  y0 := round((y2+y3)/2);
                  dy := round(abs(y3-y2)/2);
                  if dy < d then dy := d;
              end
              else if (boxX >= x2) and (boxX <= x3) then
              begin
                  a := abs(y1-y0)*sqrt(3)/(x1-x0);
                  if 2*(boxX-x0) < (x1-boxX) then
                    if y0 > y1 then
                      y0 := round(1/3*y0 +2/3*y1 +sqrt(4/9*(y1-y0)*(y1-y0)-a*a*(boxX-x0)*(boxX-x0)))
                    else
                      y0 := round(1/3*y0 +2/3*y1 -sqrt(4/9*(y1-y0)*(y1-y0)-a*a*(boxX-x0)*(boxX-x0)))
                  else
                    if y0 > y1 then
                      y0 := round(4/3*y0 -1/3*y1 -sqrt(16/9*(y1-y0)*(y1-y0) -a*a*(boxX-x1)*(boxX-x1)))
                    else
                      y0 := round(4/3*y0 -1/3*y1 +sqrt(16/9*(y1-y0)*(y1-y0) -a*a*(boxX-x1)*(boxX-x1)));
                  if abs(y3-y2) div (x3-x2) >= 5 then
                  begin
                    dy := d*abs(y3-y2) div (x3-x2);
                    if y2 < y3 then
                    begin
                      if y0-dy < y2 then
                        dy := y0 -y2;
                      if y0+dy > y3 then
                        dy := y3 -y0;
                    end
                    else
                    begin
                      if y0-dy < y3 then
                        dy := y0-y3;
                      if y0+dy > y2 then
                        dy := y2-y0;
                    end;
                  end
                  else
                    dy := d;
                  x0 := round((x2+x3)/2);
                  dx := round((x3-x2)/2);
                  if dx < d then dx := d;
              end
              else
              begin
                  dy := -d;
                  dx := -d;
              end;
          end
          else
          begin
              if TreeStyle = tsCircle then
              begin
                x0 := round(position.x +xbase);
                y0 := round(position.y +ybase);
                x1 := round(position.x -round(cos(angle)*(avglen-anc.avglen)) +xbase);
                y1 := round(position.y +round(sin(angle)*(avglen-anc.avglen)) +ybase);
              end
              else
              begin
                x0 := round(position.x +xbase);
                y0 := round(position.y +ybase);
                x1 := round(anc.position.x +xbase);
                y1 := round(anc.position.y +ybase);
              end;
              if abs(x0-x1) <= 2*d then
              begin
                dx := d;
                dy := round(abs(y0-y1)/2);
                x0 := round((x0+x1)/2);
                y0 := round((y0+y1)/2);
              end
              else if abs(y0-y1) <= 2*d then
              begin
                dx := round(abs(x0-x1)/2);
                dy := d;
                x0 := round((x0+x1)/2);
                y0 := round((y0+y1)/2);
              end
              else
              begin
                x2 := round((x0+x1)/2);
                y2 := round((y0+y1)/2);
                dx := round(abs(x0-x1)/2) +d;
                dy := round(abs(y0-y1)/2) +d;
                if (boxX >= x2-dx) and (boxX <= x2+dx)
                and (boxY >= y2-dy) and (boxY <= y2+dy) then
                  GetCrossPoint;
                dx := d;
                dy := d;
              end;
          end;

          if (boxY >= y0-dy) and (boxY <= y0+dy) then
            if (boxX >= x0-dx) and (boxX <= x0+dx) then
            begin
                Result := i;
                Break;
            end;
        end;
    end;
end;

function TTreeCustomControl.FocusBranch(x,y : integer):boolean;
var
  bn: integer;
  boxX: Integer = -1;
  boxY: Integer = -1;
begin
    Result := False;
    if NoOfNodes = 0 then Exit;
    if (x <= 0) and (y <= 0) then
    begin
      ClearFocus;
      Exit;
    end;
    bn := GetBranchIndexAtCoords(x, y);

    FocusOnBranch(bn);


    if (bn > 0) then
    begin
      ClientToBox(x, y, boxX, boxY);
      MarkedPos.X := boxX - xbase;
      MarkedPos.Y := boxY - ybase;

      if Assigned(EditBox) and TEdit(EditBox).Visible then
        TEdit(EditBox).Hide;
    end;

    Result := BranchFocused;
    if Result then
      Invalidate;
end;

function TTreeCustomControl.FocusedOnGeneDup: Boolean;
begin
  Result := (NodeFocused and Node[FocusedIndex].IsGeneDuplication);
end;

function TTreeCustomControl.FocusedOnSpeciation: Boolean;
begin
  Result := (NodeFocused and Node[FocusedIndex].IsSpeciationEvent);
end;

function TTreeCustomControl.AvgEvolutionaryRate: Double;
var
  temp: Double;
  i: Integer;
begin
  Result := 0.0;
  if NoOfNodes > 0 then
  begin
    temp := 0.0;
    for i := 1 to NoOfNodes do
      temp := temp + Node[i].rate;
    Result := temp/NoOfNodes;
  end;
end;

procedure TTreeCustomControl.ClickName(Sender: TObject);
begin
  if @OnNameClick <> nil then
    OnNameClick(Sender);
end;

procedure TTreeCustomControl.StartUserEditingName;
var
  boxX: Integer = -1;
  boxY: Integer = -1;
  e: TEdit = nil;
begin
  InitEditBox;
  e := TEdit(EditBox);
  if (FocusedNameIndex = 0) or (not EditNameEnabled) then Exit;
  e.Font.Assign(GetNodeAttrib(Node[FocusedNameIndex]).Font);
  if Node[FocusedNameIndex].hilighted then e.Font.Color := HilightColor;
  Canvas.Font.Assign(GetNodeAttrib(Node[FocusedNameIndex]).Font);
  if Node[FocusedNameIndex].name = '' then
    e.Text := ' '
  else
    e.Text := Node[FocusedNameIndex].name;
  if (not Node[FocusedNameIndex].OTU) and Node[FocusedNameIndex].compressed then
    e.left := (Node[FocusedNameIndex].des1.position.x +xbase) +Canvas.TextWidth(' ')
  else
    e.left := (Node[FocusedNameIndex].position.x +xbase) +Canvas.TextWidth(' ');
  e.left := e.left +GetNodeAttrib(Node[FocusedNameIndex]).LineWidth div 2;
  if GetTaxonMarker(Node[FocusedNameIndex]).Shape <> msNone then
      e.left := e.left +Abs(GetNodeAttrib(Node[FocusedNameIndex]).Font.Height*5);
  Canvas.Font.Assign(CharStateFont);
  if ShowCharState then
      e.left := e.left +Canvas.TextWidth(Node[FocusedNameIndex].charstate) +Abs(CharStateFont.Height);
  if (not Node[FocusedNameIndex].OTU) and Node[FocusedNameIndex].compressed then
    e.Top := ((Node[FocusedNameIndex].des1.position.y +Node[FocusedNameIndex].des2.position.y +2*ybase) div 2)
                    -Abs(e.Font.Height div 2)
  else
    e.Top := ((Node[FocusedNameIndex].position.y +ybase))
                    -Abs(e.Font.Height div 2);
  e.Width := (Node[FocusedNameIndex].namesize.x +Node[FocusedNameIndex].namesize.y);
  if e.Width+e.left > Width then
    e.Width := Width-e.left;
  if BoxToClient(e.Left, e.Top, boxX, boxY) then
  begin
    e.Left := boxX;
    e.Top := boxY;
  end;
  
  e.Show;
  e.SetFocus;
  FOriginalEditText := e.Text;
end;

{ GetDivTimeBoundingBox, GetNodeAtDivTimeCoords,
  GetBLenBoundingBox, GetBranchAtBLenCoords,
  FocusTime are new functions for hit tests
  when user clicks on the tree } 
function TTreeCustomControl.GetDivTimeBoundingBox(aNode: TpNode): TRect;
var
  DivTimeStr: AnsiString;
  Coords: TPoint;
  TextHeight, TextWidth: Integer;
begin
  DivTimeStr := FloatToStrF(aNode.height, DecimalDisplayMode, 8, DivTimeDecimals);
  GetCoordsForDivTime(aNode, DivTimeStr, Coords.X, Coords.Y);
  Canvas.Font.Assign(TimesFont);
  TextWidth := Canvas.TextWidth(DivTimeStr);
  TextHeight := Canvas.TextHeight(DivTimeStr);
  Result := TRect.Create(Coords, TextWidth, TextHeight);
end;

function TTreeCustomControl.GetNodeAtDivTimeCoords(X, Y: Integer):Integer;
var
  i : integer;
  boxX: Integer = -1;
  boxY: Integer = -1;
  BoundRect: TRect;
begin
  Result := 0;
  if NoOfNodes = 0 then Exit;

    ClientToBox(x, y, boxX, boxY);

    for i := 1 to NoOfNodes do
    begin
      if Node[i] = Root then Continue;
      if Node[i].hidden then Continue;
      if Node[i].OTU then Continue;

      BoundRect := GetDivTimeBoundingBox(Node[i]);

      if (boxY > BoundRect.Top) and (boxY < BoundRect.Bottom) then
        if (boxX > BoundRect.Left) and (boxX < BoundRect.Right) then
        begin
              Result := i;
              Break;
        end;
    end;
end;

function TTreeCustomControl.GetBLenBoundingBox(aNode: TpNode): TRect;
var
  BLenString: AnsiString;
  Coords: TPoint;
  TextHeight, TextWidth: Integer;
begin
  BLenString := FloatToStrF(aNode.branch.length, ffFixed, 15, BLenDecimals);
  Coords := GetBestBLenPosition(aNode, BLenString);
  Canvas.Font.Assign(BLensFont);
  TextWidth := Canvas.TextWidth(BLenString);
  TextHeight := Canvas.TextHeight(BLenString);
  Result := TRect.Create(Coords, TextWidth, TextHeight);
end;

function TTreeCustomControl.GetBranchAtBLenCoords(X, Y: Integer):Integer;
var
  i : integer;
  boxX: Integer = -1;
  boxY: Integer = -1;
  BoundRect: TRect;
begin
  Result := 0;
  if NoOfNodes = 0 then Exit;

    ClientToBox(x, y, boxX, boxY);

    for i := 1 to NoOfNodes do
    begin
      if Node[i] = Root then Continue;
      if Node[i].hidden then Continue;

      BoundRect := GetBLenBoundingBox(Node[i]);

      if (boxY > BoundRect.Top) and (boxY < BoundRect.Bottom) then
        if (boxX > BoundRect.Left) and (boxX < BoundRect.Right) then
        begin
              Result := i;
              Break;
        end;
    end;
end;

function TTreeCustomControl.FocusTime(x,y : integer):boolean;
var
  bn: integer = 0;
begin
    Result := False;
    if NoOfNodes = 0 then Exit;
    if (x <= 0) and (y <= 0) then
    begin
      ClearFocus;
      Exit;
    end;

    if ShowBLen then
      bn := GetBranchAtBLenCoords(x, y)
    else if ShowDivergenceTimes then
      bn := GetNodeAtDivTimeCoords(x, y);

    FocusOnTime(bn);

    if (bn > 0) then
    begin
      if Assigned(EditBox) and TEdit(EditBox).Visible then
        TEdit(EditBox).Hide;
    end;

    Result := TimeFocused;
    if Result then
      Invalidate;
end;

{ previous editbox behavior changed tree values as user typed in text.
  new behavior: don't update\validate until the user is done editing } 

procedure TTreeCustomControl.StartUserEditingTime;
var
  boxX: Integer = -1;
  boxY: Integer = -1;
  e: TEdit = nil;
  Coords: TPoint;
  aFormatStr: String = '';
begin
  InitEditBox(True);
  e := TEdit(EditBox);
  if (FocusedTimeIndex = 0) or (not EditTimesEnabled) then Exit;

  if ShowBLen then
  begin
    e.Font.Assign(BLensFont);
    Canvas.Font.Assign(BLensFont);
    aFormatStr := Format('%%.%df', [BLenDecimals]);
    e.Text := Format(aFormatStr, [Node[FocusedTimeIndex].branch.length]);
    Coords := GetBestBLenPosition(Node[FocusedTimeIndex], e.Text);
  end
  else if ShowDivergenceTimes then
  begin
    e.Font.Assign(TimesFont);
    Canvas.Font.Assign(TimesFont);
    aFormatStr := Format('%%.%df', [DivTimeDecimals]);
    e.Text := Format(aFormatStr, [Node[FocusedTimeIndex].height]);
    GetCoordsForDivTime(Node[FocusedTimeIndex], e.Text, Coords.X, Coords.Y);
  end
  else
    aFormatStr := Format('%%.%df', [BLenDecimals]);

  e.Left := Coords.X;
  e.Top := Coords.Y - 5;

  e.Width := Canvas.TextWidth(e.Text + '9.9');

  if e.Width+e.left > Width then
    e.Width := Width-e.left;

  if BoxToClient(e.Left, e.Top, boxX, boxY) then
  begin
    e.Left := boxX;
    e.Top := boxY;
  end;

  e.Color := clWhite;
  e.Font.Color := clBlack;
  e.Show;
  e.SetFocus;
  FOriginalEditText := e.Text;
end;

function TTreeCustomControl.FocusName(x,y : integer):boolean;
var
  x0,x1,y0,y1 : integer;
  boxX: Integer = -1;
  boxY: Integer = -1;

  procedure GetFocusedName(p: TpNode);
  var
    aHeight: Integer = 0;
  begin
    if FocusedNameIndex > 0 then Exit;
    if not GetNodeAttrib(p).ShowTaxonName then exit;
    if p.OTU {or p.compressed} then
    begin
      x0 := (p.position.x +xbase) div 1;
      x1 := ((p.position.x +p.namesize.x +xbase) div 1) +Abs(CharStateFont.Height) div 2;
      if ShowOTUMarker and GetNodeAttrib(p).ShowTaxonMarker and (Marker[p.index].Shape <> msNone) then
      begin
        x0 := x0 -Abs(GetNodeAttrib(p).Font.Height*5) div 1;
        x1 := x1 +Abs(GetNodeAttrib(p).Font.Height*5) div 1;
      end;
      if ShowCharState then
      begin
        x0 := x0 -Canvas.TextWidth(p.charstate) +Abs(CharStateFont.Height) div 2;
        x1 := x1 +Canvas.TextWidth(p.charstate) +Abs(CharStateFont.Height) div 2;
      end;
      aHeight := Canvas.TextHeight(p.name);
      y0 := ((p.position.y +ybase) div 1) - (aHeight div 2);
      y1 := ((p.position.y +ybase) div 1) + (aHeight div 2);

      if (boxY >= y0) and (boxY <= y1) and (boxX >= x0) and (boxX <= x1) then
        FFocusedNameIndex := p.index;
    end
    else if not p.compressed then
    begin
      GetFocusedName(p.des1);
      GetFocusedName(p.des2);
    end;
  end;

begin
    Result := false;
    if not TreeExist then Exit;
    if (not ShowOTUName ) or (TreeStyle = tsCircle) or (TreeStyle = tsRadiation) then Exit;
    ClientToBox(x, y, boxX, boxY);
    ClearFocus;

    if @FOnChange <> nil then FOnChange(Self);

    if (x <= 0) and (y <= 0) then Exit;

    Canvas.Font.Assign(CharStateFont);

    FFocusedNameIndex := 0;
    GetFocusedName(Root);
    if FocusedNameIndex > 0 then
    begin
      ClickName(Self);
      Result := true;
      Invalidate;
    end
    else if Assigned(EditBox) and TEdit(EditBox).Visible then
      TEdit(EditBox).Hide;
end;

procedure TTreeCustomControl.EditBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  with Sender as TEdit do
    { new editbox behavior: do not call ClearFocus right away so that we do not lose track of the selected node.
      allow user to abort editing by hitting Esc }
    if (Key = VK_RETURN) then Hide
    else if (Key = VK_ESCAPE) then begin
      TEdit(Sender).Text := FOriginalEditText;
      Hide;
    end
    else if ((Shift = [ssCtrl]) or (Shift = [ssMeta])) and (Key = VK_Z) then begin
      TEdit(Sender).Text := FOriginalEditText;
      SelectAll;
    end;
end;

{ as text changes, update the position\width of the text box,
  centered on its starting point }
procedure TTreeCustomControl.OnUserEditsTime(Sender: TObject);
var
  e: TEdit = nil;
  OldWidth, OldX: Integer;
begin
  e := TEdit(EditBox);

  OldWidth := e.Width;
  OldX := e.Left;
  e.Width := Canvas.TextWidth(e.Text+'00');
  OldX := OldX - Trunc((e.Width - OldWidth) /2);
  if (OldX > 0) then
    e.Left := OldX;
end;

{ validate and update branch lengths
  and node ages, that will be fleshed out later }
procedure TTreeCustomControl.EndUserEditsTime(Sender: TObject);
var
  e: TEdit = nil;
  UserTime: Double;
begin
  if FocusedTimeIndex < 1 then
    Exit;
  e := TEdit(EditBox);

  if ShowBLen then
  begin
    if TryStrToFloat(e.Text, UserTime) then
      SetBranchLength(FocusedTimeIndex, StrToFloat(e.Text));
  end
  else if ShowDivergenceTimes then
  begin
    if TryStrToFloat(e.Text, UserTime) then
      { MS TODO: add a dedicated function for this when functionality for
        directly editing node ages is fleshed out }
      Node[FocusedTimeIndex].height := StrToFloat(e.Text);
  end;

  EditBoxOnExit(Sender);
end;

{ update width of name box as user enters text }
procedure TTreeCustomControl.OnUserEditsOTUName(Sender: TObject);
var
  e: TEdit = nil;
begin
  e := TEdit(EditBox);

  if e.Width < Canvas.TextWidth(e.Text+'00') then
    if e.Left+Canvas.TextWidth(e.Text+'00') > Width then
      e.Width := Width-e.Left
    else
      e.Width := Canvas.TextWidth(e.Text+'00');
end;

{ new behavior: don't update name value until user is done editing }
procedure TTreeCustomControl.EndUserEditsOTUName(Sender : TObject);
var
  e: TEdit = nil;
begin
  if FocusedNameIndex < 1 then
    Exit;
  e := TEdit(EditBox);
  SetOTUName(FocusedNameIndex, e.Text);

  EditBoxOnExit(Sender);
end;

procedure TTreeCustomControl.EditBoxOnExit(Sender : TObject);
begin
    SetAttrindex;
    TEdit(EditBox).Hide;
    ClearFocus;
    Invalidate;
end;

function TTreeCustomControl.GetHilightOTUName(index: integer):boolean;
begin
  result := false;
  if (index > 0) and (index <= NoOfOTUs) then
    result := Node[index].hilighted;
end;

procedure TTreeCustomControl.SetCustomHilightColorOTUName(index: integer; value: boolean; Color: TColor = clWhite);
begin
  if (index > 0) and (index <= NoOfNodes) then
  begin
    Node[index].hilighted := value;
    if value then
      Node[index].CustomHighlightColor := Color;
  end;
end;

procedure TTreeCustomControl.SetHilightOTUName(index: integer; value: boolean);
begin
  if (index > 0) and (index <= NoOfOTUs) then
    Node[index].hilighted := value;
end;

function TTreeCustomControl.GetScaleDecimals: integer;
begin
  result := Length(FScaleText) -Pos('.', FScaleText);
end;

procedure TTreeCustomControl.SetScaleText(text : AnsiString);
begin
    FScaleText := text;
    Scale := StrToFloat(ScaleText);
end;

function TTreeCustomControl.GetIsScale:boolean;
begin
  Result := (xmax > 0.000000000000001) and (not Topoflag);
end;

function TTreeCustomControl.GetIsTimeScale:boolean;
begin
  Result := IsScale and (abs(TimeScale) >= 0.000000000000001);
end;

procedure TTreeCustomControl.SetTimeText(text : AnsiString);
begin
    FTimeText := text;
    TimeScale := StrToFloat(TimeText);
end;

procedure TTreeCustomControl.AddToSavedLines(p: array of TPoint; size: Integer);
{$IFDEF DARWIN}
var
  i, j, MaxX, MaxY: Integer;
{$ENDIF}
begin
  {$IFDEF DARWIN}
  { Canvas.GetPixel is currently unimplemented in the Cocoa LCL widgetset due
    to Cocoa's vector drawing nature. This is a sufficiently approximate
    workaround to generate the "pixel map" used by HasOverlappingDivTime in
    determining text collisions. }
  MaxX := FmaxTreeWidth - 1;
  MaxY := FmaxTreeHeight - 1;
  for i:=0 to size-1 do
  begin
    if ((i+1) < size) and (p[i].x > 0) and (p[i].y > 0) then
    begin
      if (p[i].x=p[i+1].x) and (p[i].x <= MaxX) then
        begin
          if (p[i].y<p[i+1].y) and (p[i].y < MaxY) then
            for j:=p[i].y to min(p[i+1].y, MaxY) do
              if (j > 0) then
                FSavedLines[p[i].x][j]:=True;
          if (p[i+1].y<p[i].y) and (p[i+1].y < MaxY) then
            for j:=p[i+1].y to min(p[i].y, MaxY) do
              if (j > 0) then
                FSavedLines[p[i].x][j]:=True;
        end;
      if (p[i].y=p[i+1].y) and (p[i].y <= MaxY) then
        begin
          if (p[i].x<p[i+1].x) and (p[i].x < MaxX) then
            for j:=p[i].x to min(p[i+1].x, MaxX) do
              if (j > 0) then
                FSavedLines[j][p[i].y]:=True;
          if (p[i+1].x<p[i].x) and (p[i+1].x < MaxX)then
            for j:=p[i+1].x to min(p[i].x, MaxX) do
              if (j > 0) then
                FSavedLines[j][p[i].y]:=True;
        end;
    end;
  end;
  {$ENDIF}
end;

procedure TTreeCustomControl.CompressCluster(index: integer);

    procedure MarkNode(p : TpNode);
    begin
        p.hidden := true;
        if p.OTU then Exit;
        MarkNode(p.des1);
        MarkNode(p.des2);
    end;

begin
    if index = 0 then Exit;
    if index = Root.index then Exit;
    if Node[index].hidden then Exit;
    if Node[index].compressed then Exit;
    Node[index].compressed := true;
    if Node[index].OTU then Exit;
    MarkNode(Node[index].des1);
    MarkNode(Node[index].des2);
end;

procedure UnMarkNode(p : TpNode);
begin
    p.hidden := false;
    if p.OTU then Exit;
    if p.compressed then Exit;
    UnMarkNode(p.des1);
    UnMarkNode(p.des2);
end;

procedure TTreeCustomControl.ExpandCluster(index: integer);
begin
    if index <= 0 then Exit;
    if Node[index].hidden then Exit;
    if not Node[index].compressed then Exit;
    Node[index].compressed := false;
    if Node[index].OTU then Exit;
    UnMarkNode(Node[index].des1);
    UnMarkNode(Node[index].des2);
end;

procedure TTreeCustomControl.ExpandAllCluster(index: integer);

    procedure ExpandClusters(p : TpNode);
    begin
        p.compressed := false;
        p.autoCompressed := False;
        p.compressedIndex := -1;
        if not p.OTU then
        begin
          ExpandClusters(p.des1);
          ExpandClusters(p.des2);
        end;
    end;

begin
    if index <= 0 then begin
        ExpandClusters(Root);
        UnMarkNode(Root.des1);
        UnMarkNode(Root.des2);
    end
    else if index > NoOfOTUs then begin
        ExpandClusters(Node[index]);
        UnMarkNode(Node[index].des1);
        UnMarkNode(Node[index].des2);
    end;
end;

procedure TTreeCustomControl.GetSessionProperties(var json: TJSONObject);
begin
  json.Add('DivTimeDecimals', DivTimeDecimals);
  json.Add('DataCoverageDecimals', DataCoverageDecimals);
  json.Add('ShowGeneDupMarkers', ShowGeneDupMarkers);
  json.Add('ShowSpeciationMarkers', ShowSpeciationMarkers);
  json.Add('Radius', Radius);
  json.Add('StartAngle', StartAngle);
  json.Add('CenterMargin', CenterMargin);
  json.Add('TreeStyle', TreeStyleString(TreeStyle));
  json.Add('BranchStyle', BranchStyleString(BranchStyle));
  json.Add('FillSubtreeDelta', FillSubtreeDelta);
  json.Add('StatsPosition', BranchInfoPositionString(StatsPosition));
  json.Add('TimesPosition', BranchInfoPositionString(TimesPosition));
  json.Add('BLenPosition', BranchInfoPositionString(BLenPosition));
  json.Add('BLenDecimals', BLenDecimals);
  json.Add('ShowOTUName', ShowOTUName);
  json.Add('ShowSpeciesName', ShowSpeciesName);
  json.Add('ShowOTUMarker', ShowOTUMarker);
  json.Add('ShowStats', ShowStats);
  json.Add('ShowStatsRange', ShowStatsRange);
  json.Add('ShowDataCoverage', ShowDataCoverage);
  json.Add('ShowDivergenceTimes', ShowDivergenceTimes);
  json.Add('ShowNodeIds', ShowNodeIds);
  json.Add('ShowBLen', ShowBLen);
  json.Add('ShowScale', ShowScale);
  json.Add('ShowTimeScale', ShowTimeScale);
  json.Add('ShowSamplingTimeScale', ShowSamplingTimeScale);
  json.Add('ShowCharState', ShowCharState);
  json.Add('ShowCompressedClusterSize', ShowCompressedClusterSize);
  json.Add('ShowHeightErrBar', ShowHeightErrBar);
  json.Add('ShowTopologyOnly', ShowTopologyOnly);
  json.Add('HorzTaxonName', HorzTaxonName);
  json.Add('AlignCaption', AlignCaption);
  json.Add('FontName', Font.Name);
  json.Add('FontSize', Font.Size);
  json.Add('FontBold', fsBold in Font.Style);
  json.Add('FontItalic', fsItalic in Font.Style);
  json.Add('FontUnderline', fsUnderline in Font.Style);
  json.Add('FontStrikeOut', fsStrikeOut in Font.Style);
end;

procedure TTreeCustomControl.SetSessionProperties(const json: TJsonObject);
var
  data: TJsonData = nil;
begin
  data := json.Find('DivTimeDecimals', jtNumber);
  if Assigned(data) then
    FDivTimeDecimals := data.AsInteger;

  data := json.Find('DataCoverageDecimals', jtNumber);
  if Assigned(data) then
    FDataCoverageDecimals := data.AsInteger;

  data := json.Find('ShowGeneDupMarkers', jtBoolean);
  if Assigned(data) then
    FShowGeneDupMarkers := data.AsBoolean;

  data := json.Find('ShowSpeciationMarkers', jtBoolean);
  if Assigned(data) then
    FShowSpeciationMarkers := data.AsBoolean;

  data := json.Find('Radius', jtNumber);
  if Assigned(data) then
    FRadius := data.AsInteger;

  data := json.Find('StartAngle', jtNumber);
  if Assigned(data) then
    FStartAngle := data.AsInteger;

  data := json.Find('CenterMargin', jtNumber);
  if Assigned(data) then
    FCenterMargin := data.AsInteger;

  data := json.Find('TreeStyle', jtString);
  if Assigned(data) then
    TreeStyle := StringToTreeStyle(data.AsString);

  data := json.Find('BranchStyle', jtString);
  if Assigned(data) then
    BranchStyle := StringToBranchStyle(data.AsString);

  data := json.Find('FillSubtreeDelta', jtBoolean);
  if Assigned(data) then
    FillSubtreeDelta := data.AsBoolean;

  data := json.Find('StatsPosition', jtString);
  if Assigned(data) then
    StatsPosition := StringToBranchInfoPosition(data.AsString);

  data := json.Find('TimesPosition', jtString);
  if Assigned(data) then
    TimesPosition := StringToBranchInfoPosition(data.AsString);

  data := json.Find('BLenPosition', jtString);
  if Assigned(data) then
    BLenPosition := StringToBranchInfoPosition(data.AsString);

  data := json.Find('BLenDecimals', jtNumber);
  if Assigned(data) then
    BLenDecimals := data.AsInteger;

  data := json.Find('ShowOTUName', jtBoolean);
  if Assigned(data) then
    ShowOTUName := data.AsBoolean;

  data := json.Find('ShowSpeciesName', jtBoolean);
  if Assigned(data) then
    ShowSpeciesName := data.AsBoolean;

  data := json.Find('ShowOTUMarker', jtBoolean);
  if Assigned(data) then
    ShowOTUMarker := data.AsBoolean;

  data := json.Find('ShowStats', jtBoolean);
  if Assigned(data) then
    ShowStats := data.AsBoolean;

  data := json.Find('ShowStatsRange', jtBoolean);
  if Assigned(data) then
    ShowStatsRange := data.AsBoolean;

  data := json.Find('ShowDataCoverage', jtBoolean);
  if Assigned(data) then
    ShowDataCoverage := data.AsBoolean;

  data := json.Find('ShowDivergenceTimes', jtBoolean);
  if Assigned(data) then
    ShowDivergenceTimes := data.AsBoolean;

  data := json.Find('ShowNodeIds', jtBoolean);
  if Assigned(data) then
    ShowNodeIds := data.AsBoolean;

  data := json.Find('ShowBLen', jtBoolean);
  if Assigned(data) then
    ShowBLen := data.AsBoolean;

  data := json.Find('ShowScale', jtBoolean);
  if Assigned(data) then
    ShowScale := data.AsBoolean;

  data := json.Find('ShowTimeScale', jtBoolean);
  if Assigned(data) then
    ShowTimeScale := data.AsBoolean;

  data := json.Find('ShowSamplingTimeScale', jtBoolean);
  if Assigned(data) then
    ShowSamplingTimeScale := data.AsBoolean;

  data := json.Find('ShowCharState', jtBoolean);
  if Assigned(data) then
    ShowCharState := data.AsBoolean;

  data := json.Find('ShowCompressedClusterSize', jtBoolean);
  if Assigned(data) then
    ShowCompressedClusterSize := data.AsBoolean;

  data := json.Find('ShowHeightErrBar', jtBoolean);
  if Assigned(data) then
    ShowHeightErrBar := data.AsBoolean;

  data := json.Find('ShowTopologyOnly', jtBoolean);
  if Assigned(data) then
    ShowTopologyOnly := data.AsBoolean;

  data := json.Find('HorzTaxonName', jtBoolean);
  if Assigned(data) then
    HorzTaxonName := data.AsBoolean;

  data := json.Find('AlignCaption', jtBoolean);
  if Assigned(data) then
    AlignCaption := data.AsBoolean;

  data := json.Find('FontName', jtString);
  if Assigned(data) then
  begin
    Font.Name := data.AsString;
    FNodeLabelFont.Name := Font.Name;
    FCaptionFont.Name := Font.Name;
    if Assigned(AttribList) and (AttribList.Count > 0) then
    begin
      AttribList[0].Font.Name := Font.Name;
      AttribList[0].CaptionFont.Name := Font.Name;
    end;
  end;

  data := json.Find('FontSize', jtNumber);
  if Assigned(data) then
    Font.Size := data.AsInteger;

  data := json.Find('FontBold', jtBoolean);
  if Assigned(data) then
    if data.AsBoolean then
      Font.Style := Font.Style + [fsBold]
    else
      Font.Style := Font.Style - [fsBold];

  data := json.Find('FontItalic', jtBoolean);
  if Assigned(data) then
    if data.AsBoolean then
      Font.Style := Font.Style + [fsItalic]
    else
      Font.Style := Font.Style - [fsItalic];

  data := json.Find('FontUnderline', jtBoolean);
  if Assigned(data) then
    if data.AsBoolean then
      Font.Style := Font.Style + [fsUnderline]
    else
      Font.Style := Font.Style - [fsUnderline];

  data := json.Find('FontStrikeOut', jtBoolean);
  if Assigned(data) then
    if data.AsBoolean then
      Font.Style := Font.Style + [fsStrikeOut]
    else
      Font.Style := Font.Style - [fsStrikeOut];
end;

procedure TTreeCustomControl.DebugToConsole;
begin
  {$IFDEF DEBUG}
  case FTreeStyle of
    tsTraditional: WriteToDevConsole('traditional');
    tsRadiation: WriteToDevConsole('radiation');
    tsCircle: WriteToDevConsole('circle');
  end;

  WriteToDevConsole(Format('TreeWidth =%d', [TreeWidth]));
  WriteToDevConsole(Format('PPU       =%.2f', [PixelsPerUnit]));
  WriteToDevConsole(Format('HFactor   =%.2f', [HorzFactor]));
  WriteToDevConsole(Format('HRatio    =%.2f', [HorzRatio]));
  WriteToDevConsole(EmptyStr);
  {$ENDIF}
end;

procedure TTreeCustomControl.GetScalingFactors(var aXUnit, aXMax: Double; var aYUnit, aYMax, aGUnit: Integer);
begin
  aXUnit := xunit;
  aXMax := xmax;
  aYUnit := yunit;
  aYMax := ymax;
  aGUnit := gunit;
end;

function TTreeCustomControl.TreeBoxName: String;
begin
  case FTreeBoxType of
    tttOriTree: Result := 'OriTree';
    tttBootTree: Result := 'BootTree';
    tttReltimeTree: Result := 'ReltimeTree';
    tttEditBox: Result := 'EditBox';
    tttCustomTimetreeBox: Result := 'CustomTimeTree';
  end;
end;

procedure TTreeCustomControl.BeginResize;
begin
  AutoSize := False;
  IsResizing := True;
  Self.Cursor := crSizeNWSE;
  Self.DragCursor := crSizeNWSE;
  ShowSelection := false;

  if FNoOfOTUs <= SMALL_TREE_NUM_TAXA then { for small trees, draw everything that is displayed}
    Exit;

  FTreeDisplaySettingsCache.ShowBlens := FShowBLen;
  FTreeDisplaySettingsCache.ShowCharState := FShowCharState;
  FTreeDisplaySettingsCache.ShowStats := FShowStats;
  FTreeDisplaySettingsCache.ShowStatsRange := FShowStatsRange;
  FTreeDisplaySettingsCache.ShowDataCoverage := FShowDataCoverage;
  FTreeDisplaySettingsCache.ShowNodIds := FShowNodeIds;
  FTreeDisplaySettingsCache.ShowNodeLabels := FShowNodeLabels;
  FTreeDisplaySettingsCache.ShowDivTimes := FShowDivergenceTimes;
  FTreeDisplaySettingsCache.ShowGeneDupMarkers := FShowGeneDupMarkers;
  FTreeDisplaySettingsCache.ShowSpeciationMarkers := FShowSpeciationMarkers;
  FTreeDisplaySettingsCache.ShowScale := FShowScale;
  FTreeDisplaySettingsCache.ShowTimeScale := FShowTimeScale;
  FTreeDisplaySettingsCache.ShowSamplingTimeScale := FShowSamplingTimeScale;
  FTreeDisplaySettingsCache.ShowCompressedClusterSize := FShowCompressedClusterSize;
  FTreeDisplaySettingsCache.ShowHeightErrBar := FShowHeightErrBar;

  { for larger trees, only draw the topology and taxa names}
  FShowBlen := False;
  FShowCharState := False;
  FShowStats := False;
  FShowStatsRange := False;
  FShowDataCoverage := False;
  FShowNodeIds := False;
  FShowNodeLabels := False;
  FShowDivergenceTimes := False;
  FShowGeneDupMarkers := False;
  FShowSpeciationMarkers := False;
  FShowScale := False;
  FShowTimeScale := False;
  FShowSamplingTimeScale := False;
  FShowCompressedClusterSize := False;
  FShowHeightErrBar := False;
end;

procedure TTreeCustomControl.EndResize;
begin
  AutoSize := True;
  IsResizing := False;
  Self.Cursor := crDefault;
  Self.DragCursor := crDefault;
  ShowSelection := True;

  if FNoOfOTUs <= SMALL_TREE_NUM_TAXA then
    Exit;

  FShowBLen := FTreeDisplaySettingsCache.ShowBlens;
  FShowCharState := FTreeDisplaySettingsCache.ShowCharState;
  FShowStats := FTreeDisplaySettingsCache.ShowStats;
  FShowStatsRange := FTreeDisplaySettingsCache.ShowStatsRange;
  FShowDataCoverage := FTreeDisplaySettingsCache.ShowDataCoverage;
  FShowNodeIds := FTreeDisplaySettingsCache.ShowNodIds;
  FShowNodeLabels := FTreeDisplaySettingsCache.ShowNodeLabels;
  FShowDivergenceTimes := FTreeDisplaySettingsCache.ShowDivTimes;
  FShowGeneDupMarkers := FTreeDisplaySettingsCache.ShowGeneDupMarkers;
  FShowSpeciationMarkers := FTreeDisplaySettingsCache.ShowSpeciationMarkers;
  FShowScale := FTreeDisplaySettingsCache.ShowScale;
  FShowTimeScale := FTreeDisplaySettingsCache.ShowTimeScale;
  FShowSamplingTimeScale := FTreeDisplaySettingsCache.ShowSamplingTimeScale;
  FShowCompressedClusterSize := FTreeDisplaySettingsCache.ShowCompressedClusterSize;
  FShowHeightErrBar := FTreeDisplaySettingsCache.ShowHeightErrBar;
end;

function TTreeCustomControl.HasImages: Boolean;
var
  i: Integer = -1;
begin
  Result := False;
  if Assigned(AttribList) and (AttribList.Count > 0) then
    for i := 0 to AttribList.Count - 1 do
      if AttribList[i].IsImage then
        Exit(True);

  if Assigned(GroupAttrib) and (GroupAttrib.Count > 0) then
    for i := 0 to GroupAttrib.Count - 1 do
      if GroupAttrib[i].IsImage then
        Exit(True);
end;

function TTreeCustomControl.PixelsPerUnitDisplayValue(var maxVal: Integer; var aCaptionStr: String): Integer;
var
  hRatio: Double = -1;
begin
  hRatio := HorzRatio;
  if CompareValue(hRatio, 0, FP_CUTOFF) > 0 then
    maxVal := Floor(8190*HorzFactor/HorzRatio)
  else
    maxVal := 8190;
  Result := Round(PixelsPerUnit*HorzFactor);
  if HorzFactor >= 1 then
    aCaptionStr := 'Pixels / ' + FloatToStrF(HorzFactor, ffFixed, 15, 0)
  else if HorzFactor > 0 then
    aCaptionStr := 'Pixels / ' + FloatToStrF(HorzFactor, ffFixed, 15, -Floor(Log10(HorzFactor)));
end;

function TTreeCustomControl.PixelsPerUnitInternalValue(displayVal: Integer): Integer;
var
  hFactor: Double = 0;
begin
  hFactor := Power(10, 2 - Floor(Log10(FPixelsPerUnit)));
  if CompareValue(hFactor, 0, FP_CUTOFF) > 0 then
    Result := Round(displayVal/hFactor)
  else
    Result := displayVal;
end;

procedure TTreeCustomControl.SetPixelsPerUnitFromDisplayValue(displayVal: Integer);
var
  aPpu: Integer;
begin
  aPpu := PixelsPerUnitInternalValue(displayVal);
  PixelsPerUnit := aPpu;
end;

function TTreeCustomControl.GetEditBox: TEdit;
begin
  if not Assigned(EditBox) then
    InitEditBox;
  Result := TEdit(EditBox);
end;

procedure TTreeCustomControl.AutoNameInternalNodes;
var
  i: Integer = -1;
  numDigits: Integer = 3;
  aFormatStr: String = '';
begin
  if NoOfOTUs >= 100 then
    numDigits := max(order(NoOfNodes), 4);
  aFormatStr := Format('%%.%dd', [numDigits]);
  for i := NoOfOTUs + 1 to NoOfNodes do
  begin
    if Node[i].name = EmptyStr then
    begin
      if Node[i] = Root then
        Node[i].Name := 'root'
      else
        Node[i].Name := Format(aFormatStr, [Node[i].index]);
      TreeList[0].InternalNodeLabel[i - NoOfOtus - 1] := Node[i].Name;
    end;
  end;
end;

procedure TTreeCustomControl.ClearInternalNodeNames;
var
  i: Integer = -1;
begin
  TreeList.ClearInternalNodeLabels;
  for i := NoOfOTUs + 1 to NoOfNodes do
    Node[i].Name := EmptyStr;
end;

function TTreeCustomControl.SmallestPositiveBranchLength: Double;
var
  i: Integer;
begin
  Result := MaxDouble;
  for i := 1 to NoOfNodes do
    if (CompareValue(Node[i].branch.length, 0, FP_CUTOFF) > 0) and (CompareValue(Node[i].branch.length, Result, FP_CUTOFF) < 0) then
      Result := Node[i].branch.length;
end;

function TTreeCustomControl.IsAutoCompressedClusters: Boolean;
begin
  if (Length(FAutoCompressedSettings) > 0) and (Length(FAutoCompressedSettings[GROUPS_COMPRESSED_INDEX]) > 0) then
  begin
    Result := FAutoCompressedSettings[GROUPS_COMPRESSED_INDEX][FTreeIndex] or
              FAutoCompressedSettings[CLUSTERS_COMPRESSED_INDEX][FTreeIndex] or
              FAutoCompressedSettings[SIMILAR_COMPRESSED_INDEX][FTreeIndex];
  end
  else
    Result := False; { not initialized}
end;

function TTreeCustomControl.NearestTipNodes(ancestralNodeIndex: Integer): TList;
var
  tipNodes: TList = nil;
  minDist: Extended = MaxFloat;
  i: Integer;
  n: TpNode;
  ancestralNode: TpNode;
begin
  Result := TList.Create;
  try
    ancestralNode := Node[ancestralNodeIndex];
    tipNodes := TList.Create;
    GetTipDistancesFromAncestor(ancestralNode, tipNodes);
    if tipNodes.Count > 0 then
      for i := 0 to tipNodes.Count - 1 do
      begin
        n := TpNode(tipNodes[i]);
        if CompareValue(n.tempValue, minDist, FP_CUTOFF) < 0 then
        begin
          minDist := n.tempValue;
          Result.Clear;
          Result.Add(n);
        end
        else if CompareValue(n.tempValue, minDist, FP_CUTOFF) = 0 then
          Result.Add(n);
      end;
  finally
    if Assigned(tipNodes) then
      tipNodes.Free;
  end;
end;

function TTreeCustomControl.FurthestTipNodes(ancestralNodeIndex: Integer): TList;
var
  tipNodes: TList = nil;
  maxDist: Extended = MinFloat;
  i: Integer;
  n: TpNode;
  ancestralNode: TpNode;
begin
  Result := TList.Create;
  try
    ancestralNode := Node[ancestralNodeIndex];
    tipNodes := TList.Create;
    GetTipDistancesFromAncestor(ancestralNode, tipNodes);
    if tipNodes.Count > 0 then
      for i := 0 to tipNodes.Count - 1 do
      begin
        n := TpNode(tipNodes[i]);
        if CompareValue(n.tempValue, maxDist, FP_CUTOFF) > 0 then
        begin
          maxDist := n.tempValue;
          Result.Clear;
          Result.Add(n);
        end
        else if CompareValue(n.tempValue, maxDist, FP_CUTOFF) = 0 then
          Result.Add(n);
      end;
  finally
    if Assigned(tipNodes) then
      tipNodes.Free;
  end;
end;

function TTreeCustomControl.FocusedOnOtuNode: Boolean;
begin
  Result := (NodeFocused and (FFocusedIndex <= FNoOfOTUs));
end;

function TTreeCustomControl.FocusedOnOtuBranch: Boolean;
begin
  Result := (BranchFocused and (FFocusedIndex <= FNoOfOTUs));
end;

function TTreeCustomControl.FocusedOnOutgroup: Boolean;
var
  n: TpNode;
begin
  Result := False;
  if NodeFocused or BranchFocused then
  begin
    n := Node[FFocusedIndex];
    Result := n.outgroup;
  end;
end;

function TTreeCustomControl.DebugStringsToFile(filename: String): Boolean;
var
  aList: TStringList = nil;
begin
  {$IFDEF DEBUG}
  try
    aList := DebugStrings;
    aList.SaveToFile(filename);
    Result := FileExists(filename);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
  {$ENDIF}
end;

function TTreeCustomControl.DebugStrings: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if NoOfNodes > 0 then
    for i := 1 to NoOfNodes do
      Result.Add(DebugString(i));
end;

function TTreeCustomControl.DebugString(nodeIndex: Integer): String;
var
  aNode: TpNode;
begin
  aNode := Node[nodeIndex];
  if aNode.OTU then
    Result := Format('isOtu: %s isOutgroup: %s index: %d  attr: %d  group: %d compIndex: %d isCompressed: %s auto_compressed: %s  anc: %d  d1: %d  d2: %d  %s', [BoolToStr(aNode.OTU, True), BoolToStr(aNode.outgroup, True), aNode.index, aNode.attrindex, aNode.groupindex, aNode.compressedIndex, BoolToStr(aNode.compressed, True), BoolToStr(aNode.autoCompressed, True), aNode.anc.index, -1, -1, aNode.Name])
  else if not Assigned(aNode.anc) then
    Result := Format('isOtu: %s isOutgroup: %s index: %d  attr: %d  group: %d compIndex: %d isCompressed: %s auto_compressed: %s  anc: %d  d1: %d  d2: %d  %s', [BoolToStr(aNode.OTU, True), BoolToStr(aNode.outgroup, True), aNode.index, aNode.attrindex, aNode.groupindex, aNode.compressedIndex, BoolToStr(aNode.compressed, True), BoolToStr(aNode.autoCompressed, True), -1, aNode.des1.index, aNode.des2.index, aNode.Name])
  else
    Result := Format('isOtu: %s isOutgroup: %s index: %d  attr: %d  group: %d compIndex: %d isCompressed: %s auto_compressed: %s  anc: %d  d1: %d  d2: %d  %s', [BoolToStr(aNode.OTU, True), BoolToStr(aNode.outgroup, True), aNode.index, aNode.attrindex, aNode.groupindex, aNode.compressedIndex, BoolToStr(aNode.compressed, True), BoolToStr(aNode.autoCompressed, True),aNode.anc.index, aNode.des1.index, aNode.des2.index, aNode.Name]);
end;

function TTreeCustomControl.DebugPositionString(nodeIndex: Integer): String;
var
  aNode: TpNode = nil;
begin
  aNode := Node[nodeIndex];
  Result := Format('x=%.0n y=%.0n hidden=%s', [aNode.position.X*1.0, aNode.position.y*1.0, BoolToStr(aNode.hidden, True)]);
end;

function TTreeCustomControl.DebugPositionStrings: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if NoOfNodes > 0 then
    for i := 1 to NoOfNodes do
      Result.Add(DebugPositionString(i));
end;

function TTreeCustomControl.SaveToPng(Filename: String): Boolean;
var
  ARect: TRect;
  image: TPortableNetworkGraphic = nil;
  bitmap: TBitmap = nil;
begin
  try
    ARect := Rect(0, 0, MinWidth*4, MinHeight*4);
    FDrawFullTree := True;
    image := TPortableNetworkGraphic.Create;
    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := GetPixelFormat(aRect);
    Bitmap.SetSize(RectWidth(aRect), RectHeight(aRect));
    DrawTree(Bitmap.Canvas);
    image.Assign(bitmap);
    image.SaveToFile(Filename);
  finally
    FDrawFullTree := False;
    Invalidate;
    if Assigned(bitmap) then
      bitmap.Free;
    if Assigned(image) then
      image.Free;
  end;
  Result := FileExists(Filename);
end;

function TTreeCustomControl.SaveToJpeg(Filename: String): Boolean;
var
  ARect: TRect;
  image: TJPEGImage = nil;
  bitmap: TBitmap = nil;
begin
  ARect := Rect(0, 0, MinWidth*4, MinHeight*4);
  try
    FDrawFullTree := True;
    image := TJPEGImage.Create;
    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := GetPixelFormat(aRect);
    Bitmap.SetSize(RectWidth(aRect), RectHeight(aRect));
    DrawTree(Bitmap.Canvas);
    image.Assign(bitmap);
    image.SaveToFile(Filename);
  finally
    FDrawFullTree := False;
    Invalidate;
    if Assigned(bitmap) then
      bitmap.Free;
    if Assigned(image) then
      image.Free;
  end;
  Result := FileExists(Filename);
end;

function TTreeCustomControl.SaveToTiff(Filename: String): Boolean;
var
  ARect: TRect;
  image: TTiffImage = nil;
  bitmap: TBitmap = nil;
begin
  ARect := Rect(0, 0, MinWidth*4, MinHeight*4);
  try
    FDrawFullTree := True;
    image := TTiffImage.Create;
    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := GetPixelFormat(aRect);
    Bitmap.SetSize(RectWidth(aRect), RectHeight(aRect));
    DrawTree(Bitmap.Canvas);
    image.Assign(bitmap);
    image.SaveToFile(Filename);
  finally
    FDrawFullTree := False;
    Invalidate;
    if Assigned(bitmap) then
      bitmap.Free;
    if Assigned(image) then
      image.Free;
  end;
  Result := FileExists(Filename);
end;

function TTreeCustomControl.SaveToBitmap(Filename: String): Boolean;
var
  ARect: TRect;
  Bitmap: TBitmap =  nil;
begin
  try
    FDrawFullTree := True;
    ARect := Rect(0, 0, MinWidth*4, MinHeight*4);
    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := GetPixelFormat(aRect);
    Bitmap.SetSize(RectWidth(aRect), RectHeight(aRect));
    DrawTree(Bitmap.Canvas);
    Bitmap.SaveToFile(Filename);
    Bitmap.Free;
    Result := FileExists(Filename);
  finally
    FDrawFullTree := False;
    Invalidate;
  end;
end;

function TTreeCustomControl.SaveToPdf(Filename: String): Boolean;
const
  MARGIN = 10;
var
  f: TFileStream = nil;
  aRenderer: TPdfTreeRenderer = nil;
begin
  Result := False;
  try
    ScrollTo(0, 0);
    Invalidate;
    FDrawFullTree := True;
    TreeDrawingTarget := tdtPdf;
    while FIsPainting do
      Application.ProcessMessages;
    aRenderer := TPdfTreeRenderer(FRenderer);
    aRenderer.Initialize(MinWidth*4 + 2*MARGIN, MinHeight*4 + 2*MARGIN, Application.Title, ApplicationName);
    try
      DrawTree;
      F := TFileStream.Create(Filename, fmCreate);
      aRenderer.Document.SaveToStream(F);
      Result := FileExists(Filename);
    except
      on E:Exception do
      begin
        ShowMessage('Oh no! An error occurred when generating the PDF file: ' + E.Message);
        Result := False;
      end;
    end;
  finally
    TreeDrawingTarget := tdtCanvas;
    FDrawFullTree := False;
    if Assigned(F) then
      F.Free;
    if Assigned(aRenderer) then
      aRenderer.Finalize;
    Invalidate;
  end;
end;

function TTreeCustomControl.SaveToSvg(Filename: String): Boolean;
begin
  try
    TreeDrawingTarget := tdtSvg;
    FDrawFullTree := True;
    while FIsPainting do
      Application.ProcessMessages;
    TSvgTreeRenderer(FRenderer).Initialize(Filename, MinWidth*4, MinHeight*4);
    ScrollTo(0, 0);
    Invalidate;
    DrawTree;
    Result := FileExists(Filename);
  finally
    FDrawFullTree := False;
    TSvgTreeRenderer(FRenderer).Finalize;
    TreeDrawingTarget := tdtCanvas;
  end;
end;

function TTreeCustomControl.SaveToEmf(Filename: String): Boolean;
begin
  {$IFDEF MSWINDOWS}
  try
    TreeDrawingTarget := tdtEmf;
    while FIsPainting do
      Application.ProcessMessages;
    MakeMetaFile;
    TEmfTreeRenderer(FRenderer).Initialize(Filename, MinWidth*4, MinHeight*4);
    SaveToEnhMetaFile(Filename);
    Result := FileExists(Filename);
  finally
    TEmfTreeRenderer(FRenderer).Finalize;
    TreeDrawingTarget := tdtCanvas;
  end;
  {$ELSE}
  ShowMessage('Unable to save as .emf file');
  {$ENDIF}
end;

procedure TTreeCustomControl.MakeMetaFile;
var dev_mil, dev_pix : TPoint;
    rect : TRect;
    description : array[0..255] of AnsiChar;
    //DpiRatio: Double;
    procedure SetDescription;
    var i, j : integer;
        name, title : AnsiString;
    begin
        name := 'TreeView';
        title := Name;
        i := Length(name);
        StrPCopy(description, name+' '+title+' ');
        j := StrLen(description)-1;
        description[i] := #0;
        description[j] := #0;
    end;

begin
  {$IFDEF MSWINDOWS}
    //DpiRatio := GetDpiRatio;
    dev_mil.x := GetDeviceCaps(Canvas.Handle,HORZSIZE);
    dev_mil.y := GetDeviceCaps(Canvas.Handle,VERTSIZE);
    dev_pix.x := GetDeviceCaps(Canvas.Handle,HORZRES);
    dev_pix.y := GetDeviceCaps(Canvas.Handle,VERTRES);
    rect.top := 0;
    rect.left := 0;
    rect.right := MulDiv(MinWidth, dev_mil.x*500, dev_pix.x);
    rect.bottom := MulDiv(MinHeight, dev_mil.y*500, dev_pix.y);
    if hMetaFile <> 0 then DeleteEnhMetaFile(hMetaFile);
    hMetaFile := CreateEnhMetaFile(Canvas.Handle, nil, @rect, nil);
    SetGraphicsMode(hMetaFile, GM_ADVANCED);  // To change the graphics mode from 16 to 32 bit mode
    SetMapMode(hMetaFile, MM_ANISOTROPIC);

    SetWindowExtEx(hMetaFile, MinWidth, MinHeight, nil);
    SetViewportExtEx(hMetaFile, MinWidth, MinHeight, nil);

    SetBkMode(hMetaFile, TRANSPARENT);
    SetBkColor(hMetaFile, RGB(255,255,255));
    SetROP2(hMetaFile, R2_COPYPEN);
    SetTextAlign(hMetaFile, TA_BOTTOM*4);
    Canvas.Handle:=hMetaFile;
    DrawTree;
    hMetaFile := CloseEnhMetaFile(hMetaFile);
    {$ENDIF}
end;

procedure TTreeCustomControl.SaveToEnhMetaFile(filename : String);
var HMF : THandle;
    buffer : array[0..255] of Char;
begin
    {$IFDEF MSWINDOWS}
    HMF := CopyEnhMetaFile(hMetaFile, StrPCopy(buffer, filename));
    DeleteEnhMetaFile(HMF);
    {$ENDIF}
end;

procedure TTreeCustomControl.ComputeCaptionSize(var a: TNodeAttrib);
var
  size: Integer = 0;
  aMargin: Integer = -1;
begin
  a.CaptionSize := 0;
  aMargin := Round(abs(a.CaptionFont.Height)/2);

  if a.IsCompressed then
  begin
    if a.ShowTaxonMarker and (a.Marker.Shape <> msNone) then
      a.CaptionSize := a.CaptionSize + abs(a.CaptionFont.Height)*2;
  end
  else if a.ShowBracket then
    a.CaptionSize := a.CaptionSize + aMargin + a.BracketLineWidth;

  size := Canvas.TextWidth(a.Caption + ' ');
  if a.ShowLabel then
    if (a.ShowImage and not a.Image.Empty) and
       ((a.GraphicAlign = gaTop) or (a.GraphicAlign = gaBottom)) then
    begin
      if size >= a.Image.Width + aMargin then
        a.CaptionSize := a.CaptionSize + size;
    end
    else
      a.CaptionSize := a.CaptionSize + size;

  if (a.ShowImage and not a.Image.Empty) then
    if a.ShowLabel and ((a.GraphicAlign = gaTop) or (a.GraphicAlign = gaBottom)) then
    begin
      if size < a.Image.Width + aMargin then
        a.CaptionSize := a.CaptionSize + a.Image.Width + aMargin;
    end
    else
      a.CaptionSize := a.CaptionSize + a.Image.Width + aMargin;

  if a.CaptionSize > 0 then
    a.CaptionSize := a.CaptionSize + aMargin;
end;

function TTreeCustomControl.InCompressedClade(n: TpNode): Boolean;
var
  aNode: TpNode = nil;
begin
  Result := False;
  if n.compressed or n.autoCompressed then
    Exit(True);
  aNode := n;
  while Assigned(aNode.anc) do
  begin
    aNode := aNode.anc;
    if aNode.compressed or aNode.autoCompressed then
      Exit(True);
  end;
end;

function TTreeCustomControl.GetCompressedAncestor(descendant: TpNode): TpNode;
var
  aNode: TpNode = nil;
begin
  Result := nil;
  if descendant.compressed or descendant.autoCompressed then
    Exit(descendant);
  aNode := descendant;
  while Assigned(aNode.anc) do
  begin
    aNode := aNode.anc;
    if aNode.compressed or aNode.autoCompressed then
      Exit(aNode);
  end;
end;

function TTreeCustomControl.TreeToJpeg: TJPEGImage;
var
  ARect: TRect;
  bitmap: TBitmap = nil;
begin
  ARect := Rect(0, 0, MinWidth*4, MinHeight*4);
  try
    FDrawFullTree := True;
    Result := TJPEGImage.Create;
    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := GetPixelFormat(aRect);
    Bitmap.SetSize(RectWidth(aRect), RectHeight(aRect));
    DrawTree(Bitmap.Canvas);
    Result.Assign(bitmap);
  finally
    FDrawFullTree := False;
    Invalidate;
    if Assigned(bitmap) then
      bitmap.Free;
  end;
end;

function TTreeCustomControl.ScaleBarHeight: Integer;
begin
  if FScaleBarHeight > 0 then
    Result := FScaleBarHeight
  else
    Result := Abs(ScaleFont.Height)*10 + 16; { an approximation in case the tree has not yet been rendered}

end;

procedure TTreeCustomControl.GetInternalOutgroupNames(var aList: TStringList);
var
  i: Integer;
begin
  aList.Clear;
  for i := 1 to FNoOfOTUs do
    if Node[i].tempOutgroup then
      aList.Add(Node[i].name);
end;

procedure TTreeCustomControl.ClearCalibratedNodeMarkers;
begin
  SetLength(FCalibratedNodes, 0);
  ShowCalibratedNodeMarker := False;
end;

procedure TTreeCustomControl.ResetScrollBounds;
begin
  SetScrollBounds(0, 0, FMinWidth*4, FMinHeight*4);
end;

function TTreeCustomControl.GetDrawingArea: TRect;
begin
  Result.Top := 0;
  Result.Left := 0;
  Result.Right := MinWidth*4;
  Result.Bottom := MinHeight*4;
  if ShowScale then
  begin
    Result.Bottom := Result.Bottom + Abs(ScaleFont.Height)*4;
  end;
end;

function TTreeCustomControl.NumNodesDrawn: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to NoOfNodes do
    if NeedsPainting(Node[i]) then
      inc(Result);
end;

procedure TTreeCustomControl.DoDblClick;
begin
  if FParentFormIsClosing then Exit;
  DblClick;
end;

procedure TTreeCustomControl.DoDragEnd(Target: TObject; X, Y: Integer);
begin
  if FParentFormIsClosing then Exit;
  EndDrag(False);
end;

{ wasn't doing anything as DoDragOver }
procedure TTreeCustomControl.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if FParentFormIsClosing then Exit;
  inherited;
end;

{ TreeEditBox gets dedicated drag-drop code }
procedure TTreeEditBox.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  FoundBranchIndex: integer;
  PPOTU: Integer;
  CursorPos: TPoint;
  SmallMoveRect: TRect;
begin
  if FParentFormIsClosing then Exit;
  Accept := True; { set to true, otherwise our custom cursor will be overridden with crNoDrop}

  CursorPos := TPoint.Create(0, 0);


  if Source is TTreeEditBox then
  begin
    if EditEnabled and BranchFocused then
    begin
      FoundBranchIndex := GetBranchIndexAtCoords(X, Y);
      if (FoundBranchIndex <> 0) and (FoundBranchIndex = TargetBranch) then
      begin
        Cursor := CURSOR_DROP_BRANCH;
        DragCursor := CURSOR_DROP_BRANCH;
      end
      else
      begin
        Cursor := CURSOR_DRAG_BRANCH;
        DragCursor := CURSOR_DRAG_BRANCH;
      end;
      Invalidate;
    end
    else if IsResizing then
    begin
      Accept := True;
      CursorPos := TPoint.Create(0, 0);
      GetCursorPos(CursorPos);
      CursorPos := ScreenToClient(CursorPos);
      SmallMoveRect := rect(FDragLastPoint.X - 4, FDragLastPoint.Y - 4, FDragLastPoint.X + 4, FDragLastPoint.Y + 4);
      if PointInRect(CursorPos, SmallMoveRect, False) or (not IsResizing) then
         Exit;

      PPOTU := FStartPixelsPerOTU - (FDragStartPoint.Y - CursorPos.Y);
      if PPOTU < 1 then
        PPOTU := 1;
      {$IFDEF DARWIN}
      PPOTU := Max(8, PPOTU);
      {$ENDIF}
      FDragLastPoint := CursorPos;
      if PPOTU < PixelsPerOTU then
        PixelsPerOTU := Max(PixelsPerOTU - 4, PPOTU)
      else
        PixelsPerOTU := Min(PixelsPerOTU + 4, PPOTU);
      TreeWidth := FStartTreeWidth - (FDragStartPoint.X - CursorPos.X);
      Refresh;
    end;
  end;

  if Assigned(@OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TTreeCustomControl.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if FParentFormIsClosing then Exit;
  KeyDown(Key, Shift);
end;

procedure TTreeCustomControl.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FParentFormIsClosing then Exit;
  MouseDown(Button, Shift, X, Y);
end;

procedure TTreeCustomControl.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FParentFormIsClosing then Exit;
  MouseMove(Shift, X, Y);
end;

procedure TTreeCustomControl.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FParentFormIsClosing then Exit;
  MouseUp(Button, Shift, X, Y);
end;

function TTreeCustomControl.OutgroupIsDes1: Boolean;

  function ProcessNode(ANode: TpNode): Boolean;
  begin
    if ANode.Otu then
      Result := ANode.outgroup
    else
    begin
      Result := ProcessNode(ANode.des1);
      if not Result then
        Result := ProcessNode(ANode.des2);
    end;
  end;

begin
  Result := ProcessNode(Root.des1);
end;

function TTreeCustomControl.OutgroupIsDes2: Boolean;

  function ProcessNode(ANode: TpNode): Boolean;
  begin
    if ANode.Otu then
      Result := ANode.outgroup
    else
    begin
      Result := ProcessNode(ANode.des1);
      if not Result then
        Result := ProcessNode(ANode.des2);
    end;
  end;

begin
  Result := ProcessNode(Root.des2);
end;

procedure TTreeCustomControl.FocusOnRoot;
begin
  FocusOnNode(Root.Index);
end;

function TTreeCustomControl.IsFocusedOnRoot: Boolean;
begin
  Result := (FocusedIndex = Root.Index);
end;

procedure TTreeCustomControl.SortClusterInOrder;
begin
    SortBranchByOrder(Root);
end;

procedure TTreeCustomControl.SortTreeByYear;
begin
  SortBranchByYear(Root);
end;

procedure TTreeCustomControl.SortTreeByContinent;
begin
  SortBranchByContinent(Root);
end;

procedure TTreeCustomControl.SortTreeByGroup;
begin
  SortBranchByGroup(Root);
end;

procedure TTreeCustomControl.SortClusterForShape;
begin
    SortBranchByFigure(Root);
end;

function TTreeCustomControl.GetCharState(Index : integer):AnsiString;
begin
    if Index <= NoOfNodes then
        Result := Node[Index].charstate;
end;

procedure TTreeCustomControl.SetCharState(Index : integer; const newstate : AnsiString);
begin
    if not TreeExist then Exit;
    if Index <= NoOfNodes then
        Node[Index].charstate := newstate;
end;

procedure TTreeCustomControl.SetDistanceMatrix(value: PDistanceMatrix);
begin
  if value = FDistanceMatrix then Exit;
  FDistanceMatrix := value;
  if (value <> nil) and TreeExist then
     SetClusterHeight;
end;

procedure TTreeCustomControl.SetDivergenceTime(DivergenceTime: TCalibrationTime);
begin
  DoSetDivergenceTime(DivergenceTime.MinTime);
  FShowDivergenceTimes := True;
end;

procedure TTreeCustomControl.SetClusterHeightFromDistances;
var n1, n2 : PArrayOfInt;
    i, j, k : integer;
    h : double;

    procedure SearchOTU(p : TpNode; n : PArrayOfInt);
    begin
        if p.OTU then begin
            Inc(n[0]);
            n[n[0]] := p.Index;
        end
        else begin
            SearchOTU(p.des1, n);
            SearchOTU(p.des2, n);
        end;
    end;

    procedure SetNum(p : TpNode);
    begin
        n1[0] := 0;
        SearchOTU(p.des1, n1);
        n2[0] := 0;
        SearchOTU(p.des2, n2);
    end;

begin
  n1 := nil;
  n2 := nil;
  try
    if not TreeExist then Exit;
    GetMem(n1, SizeOf(Integer)*NoOfOTUs);
    GetMem(n2, SizeOf(Integer)*NoOfOTUs);
    for k := NoOfOTUs+1 to NoOfNodes do begin
        SetNum(Node[k]);
        h := 0.0;
        for i := 1 to n1[0] do
          for j := 1 to n2[0] do
            h := h + DistanceMatrix[n1[i]-1][n2[j]-1];
        h := h/n1[0]/n2[0]/2;
        Node[k].Height := h;
    end;
  finally
    FreeMemAndNil(n1);
    FreeMemAndNil(n2);
  end;
end;

procedure TTreeCustomControl.SetClusterHeightByBranchLengths;

  procedure SetRootHeight(p : TpNode);
  begin
    with p^ do
      if OTU then
        height := 0
      else
      begin
        SetRootHeight(des1);
        SetRootHeight(des2);

        height := max(des1.height+des1.branch.length,
                      des2.height+des2.branch.length);
      end;
  end;

  procedure SetEachHeight(p : TpNode);
  begin
    with p^ do
      begin
        height := anc.height -branch.length;
        if not OTU then
        begin
          SetEachHeight(des1);
          SetEachHeight(des2);
        end;
      end
  end;

begin
  SetRootHeight(Root);
  SetEachHeight(Root.des1);
  SetEachHeight(Root.des2);
end;

procedure TTreeCustomControl.SetClusterHeightUPGMA;

  procedure SetEachHeight(p : TpNode);
  begin
    with p^ do
      if OTU then
      begin
        height := 0.0;
        h0     := 0;
      end
      else
      begin
        SetEachHeight(des1);
        SetEachHeight(des2);
        h0     := ((des1.h0+des1.branch.length)*des1.size +(des2.h0+des2.branch.length)*des2.size)/size;
        height := (des1.h0+des1.branch.length +des2.h0+des2.branch.length)/2;
      end;
    end;

begin
  SetEachHeight(Root);
end;

procedure TTreeCustomControl.SetClusterHeightByLinearizeFunc;
var
  tree: TTreeData;

  procedure SetRootHeight;
  var
    h: double;

    procedure GetNodeHeight(n: TpNode);
    begin
      h := h +tree.BLen[n.index-1];
      if n.OTU then
      begin
        if h > root.height then
          root.height := h;
      end
      else
      begin
        GetNodeHeight(n.des1);
        GetNodeHeight(n.des2);
      end;
      h := h -tree.BLen[n.index-1];
    end;

  begin
    h := 0;
    GetNodeHeight(root.des1);
    GetNodeHeight(root.des2);
  end;

  procedure SetEachClusterHeight(n: TpNode);
  begin
    n.height := n.anc.height -tree.BLen[n.index-1];
    if not n.OTU then
    begin
      SetEachClusterHeight(n.des1);
      SetEachClusterHeight(n.des2);
    end;
  end;

var
  i: integer;
begin
  if (not IsLinearized) and (not IsTimes)  then
  begin
    SetClusterHeightByBranchLengths;
    exit;
  end;

  tree := TTreeData.Create(NoOfOTUs,true,isSE,false);

  for i := 1 to NoOfOTUs-1 do
  begin
    tree.NodeArray[i-1].des1 := Node[NoOfOTUs+i].des1.index-1;
    tree.NodeArray[i-1].des2 := Node[NoOfOTUs+i].des2.index-1;
    tree.DataCoverage[i-1] := Node[NoOfOTUs+i].dataCoverage;
  end;
  for i := 0 to NoOfNodes-2 do
  begin
    tree.BLen[i] := Node[i+1].branch.length;
    if isSE then
      tree.SE[i] := Node[i+1].branch.SE;
  end;
  for i := 1 to NoOfOTUs do
    tree.IsOutgroupMember[i-1] := Node[i].outgroup;

  LinearizeFunc(tree);

  SetRootHeight;
  SetEachClusterHeight(root.des1);
  SetEachClusterHeight(root.des2);

  tree.Free;
end;

procedure TTreeCustomControl.SetClusterHeight;
begin
  if not TreeExist then Exit;
  if IsUPGMA then
    SetClusterHeightUPGMA
  else if assigned(LinearizeFunc) then
    SetClusterHeightByLinearizeFunc
  else
    SetClusterHeightByBranchLengths;
end;

procedure TTreeCustomControl.SetIsLinearized(value: boolean);
begin
  FIsLinearized := value;
end;

procedure TTreeCustomControl.SetIsRooted(const AValue: Boolean);
begin
  FIsRooted := AValue;
end;

procedure TTreeCustomControl.SetGroupIndex;

  procedure SetGroupIndexOnNode(p: TpNode);
  begin
    if p.OTU then exit;

    SetGroupIndexOnNode(p.des1);
    SetGroupIndexOnNode(p.des2);

    if p.des1.groupindex = p.des2.groupindex then
      p.groupindex := p.des1.groupindex
    else
      p.groupindex := -1;
  end;

begin
  SetGroupIndexOnNode(Root);
end;

procedure TTreeCustomControl.SetUseSubtreeAttrib(value: boolean);
begin
  if value = FUseSubtreeAttrib then exit;
  FUseSubtreeAttrib :=value;

  if not value then
    ExpandAllCluster(Root.index);

  SetAttrindex;
end;

procedure TTreeCustomControl.SetUseGroupAttrib(value: boolean);
begin
  if value = FUseGroupAttrib then exit;
  FUseGroupAttrib := value;

  if not value then
    ExpandAllCluster(Root.index);

  SetAttrindex;
end;

procedure TTreeCustomControl.SetAttrIndex;
var
  i: Integer;
  a: TNodeAttrib = nil;

  procedure SetDescend(p: TpNode);
  var
    aAttrib: TNodeAttrib = nil;
  begin
    if p <> root then
    begin
      Assert(Assigned(p.anc), 'unexpected nil ancestor node');
      if p.attrindex = 0 then
        if (p.anc.attrindex <= 0) or (not AttribList[p.anc.attrindex].OverwriteDownstream) then
          if UseGroupAttrib and (p.groupindex <> p.anc.groupindex) then
            p.attrindex := -(p.groupindex + 1); { this will cause GetNodeAttrib to use groupindex instead of attrindex downstream}

      if p.attrindex = 0 then
      begin
        if p.compressedIndex >= 0 then
          CompressCluster(p.index);
        p.attrindex := p.anc.attrindex;
        if p.name = '' then
        begin
          p.namesize.x := 0;
          p.namesize.y := 0;
        end
        else
        begin
          aAttrib := GetNodeAttrib(p);
          Canvas.Font.Assign(aAttrib.Font);
          p.namesize.x := Canvas.TextWidth(p.name+' ');
          if p.PrivateName <> EmptyStr then
          begin
            p.namesize.x := p.namesize.x + (StrLength(p.PrivateName+' ', aAttrib.BoldFont));
          end;
            //p.namesize.x := p.namesize.x + (Canvas.TextWidth(p.PrivateName+' ') * 4);
          p.namesize.y := abs(Canvas.Font.Height);
        end;
        if p.speciesName = '' then
        begin
          p.speciesNameSize.x := 0;
          p.speciesNameSize.y := 0;
        end
        else
        begin
          aAttrib := GetNodeAttrib(p);
          Canvas.Font.Assign(aAttrib.Font);
          p.speciesNameSize.x := Canvas.TextWidth(p.speciesName+' ');
          if p.PrivateName <> EmptyStr then
          begin
            p.speciesNameSize.x := p.speciesNameSize.x + (StrLength(p.PrivateName+' ', aAttrib.BoldFont));
          end;
          p.speciesNameSize.y := abs(Canvas.Font.Height);
        end;
      end
      else if p.attrindex <> p.anc.attrindex then
      begin
        aAttrib := GetNodeAttrib(p);

        if aAttrib.IsCompressed and (not p.hidden) then
          CompressCluster(p.index);

//        aAttrib.CaptionSize := Canvas.TextWidth(aAttrib.Caption)*4;
        if p.OTU then
        begin
          if p.compressed then
          begin
            Canvas.Font.Assign(aAttrib.CaptionFont);
            p.namesize.x := Canvas.TextWidth(aAttrib.Caption+' ');
            p.speciesNameSize.x := Canvas.TextWidth(aAttrib.Caption + ' ');
          end
          else
          begin
            Canvas.Font.Assign(aAttrib.Font);
            p.namesize.x := Canvas.TextWidth(p.name+' ');
            if p.SpeciesName <> EmptyStr then
              p.speciesNameSize.x := Canvas.TextWidth(p.SpeciesName + ' ');
            if p.PrivateName <> EmptyStr then
            begin
              p.namesize.x := p.namesize.x + (StrLength(p.PrivateName+' ', aAttrib.BoldFont));
            end;
          end;
        end
        else
        begin
          p.name := aAttrib.Caption;
          Canvas.Font.Assign(aAttrib.CaptionFont);
          p.namesize.x := Canvas.TextWidth(aAttrib.Caption+' ');
        end;
        p.namesize.y := abs(Canvas.Font.Height);
        p.speciesNameSize.y := abs(Canvas.Font.Height);
        Canvas.Font.Assign(aAttrib.CaptionFont);
        ComputeCaptionSize(aAttrib);
      end;
    end;
    if not p.OTU then
    begin
      SetDescend(p.des1);
      SetDescend(p.des2);
    end;
  end;

begin
  if not Assigned(Root) then  // in case somehow the topology editor got us here without being setup properly
    Exit;
  for i := 1 to NoOfNodes do
  begin
    Node[i].attrindex := BASE_NODE_ATTRUBUTE_INDEX; { index for the root node - these attributes provide defaults and propagate through the tree but can be overridden}
    Node[i].compressedIndex := -1;
  end;

  if not IsGeneDups then
    for i := NoOfOTUs + 1 to NoOfNodes do
      Node[i].marker.Shape := msNone;

  ExpandAllCluster(Root.index);
  if IsAutoCompressedClusters and (CompressedNodeAttributes.Count > 0) then { iterate the list of CompressedNodeAttributes and assign indices for attributes in the list to target nodes}
    for i := 0 to CompressedNodeAttributes.Count - 1 do
    begin
      a := CompressedNodeAttributes[i];
      Node[a.NodeIndex].compressedIndex := i;
    end;

  if UseGroupAttrib then
  begin
    SetGroupIndex; { propagates indices from leaf nodes to internal nodes in the tree where appropriate}
    for i := 1 to NoOfNodes do
    begin
      Assert((Node[i] = Root) or Assigned(Node[i].anc), Format('non-root node #%d is missing its parent', [i]));
      if (Node[i] <> Root) and (not Assigned(Node[i].anc)) then
        continue;
      if (Node[i] <> Root) and (Node[i].groupindex <> Node[i].anc.groupindex) then { this is a node where attributes differ from its parent}
      begin
        if ((i > NoOfOTUs) or (Node[i].marker.Shape = msNone)) and
           (Node[i].groupindex >= 0) and
           (Node[i].groupindex < GroupAttrib.Count) then
          Node[i].marker := GroupAttrib[Node[i].groupindex].Marker;
      end;
    end;
  end;

  if UseSubtreeAttrib then
  begin
    if AttribList.Count > 1 then  { the case where the user has added formatting to subtrees, overriding the default root attributes}
      for i := 1 to AttribList.Count - 1 do
      begin
        Assert(Assigned(AttribList[i]));
        Node[AttribList[i].nodeindex].attrindex := i;
        Node[AttribList[i].nodeindex].marker := AttribList[i].marker;
      end;
  end;

  if UseGroupAttrib or UseSubtreeAttrib then
    SetDescend(Root);

  SetClusterWidth(Root);
end;

function TTreeCustomControl.DebugFontStrings: TStringList;
begin
  Result := TStringList.Create;
  Result.Add(Format('Font           = %s', [Font.Name]));
  Result.Add(Format('FOtuFont       = %s', [FOTU_Font.Name]));
  Result.Add(Format('FNodeLabelFont = %s', [FNodeLabelFont.Name]));
  Result.Add(Format('FCaptionFont   = %s', [FCaptionFont.Name]));
  Result.Add(Format('FPrivateFont   = %s', [FPrivateFont.Name]));
  Result.Add(Format('StatsFont      = %s', [StatsFont.Name]));
  Result.Add(Format('BLensFont      = %s', [BLensFont.Name]));
  Result.Add(Format('TimesFont      = %s', [TimesFont.Name]));
  Result.Add(Format('ScaleFont      = %s', [ScaleFont.Name]));
  Result.Add(Format('CharStateFont  = %s', [CharStateFont.Name]));
end;

function TTreeCustomControl.DebugFontStr: String;
var
  aList: TStringList = nil;
begin
  try
    aList := DebugFontStrings;
    Result := aList.Text
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TTreeCustomControl.ClearAttrib;
var i: integer;
begin
  if AttribList.Count <= 1 then Exit;
  for i := AttribList.Count-1 downto 1 do
  begin
    AttribList[i].Free;
    AttribList.delete(i);
  end;
end;

procedure TTreeCustomControl.InitMem;
var i : integer;
begin
    GetMem(Node,SizeOf(TpNode)*NoOfNodes);
    for i := 1 to NoOfOTUs do
    begin
        New(Node[i]);
        Node[i].index:= i;
        Node[i].minOTU := i;
        Node[i].size := 1;
        Node[i].depth := 0;
        Node[i].height := 0.0;
        Node[i].maxh := 0;
        Node[i].minh := 0;
        Node[i].rate := 0;
        Node[i].width := 0;
        Node[i].anc := nil;
        Node[i].des1 := nil;
        Node[i].des2 := nil;
        Node[i].OTU := true;
        Node[i].compressed := false;
        Node[i].autoCompressed := False;
        Node[i].marker.shape := msNone;
        Node[i].marker.color := clBlack;
        Node[i].outgroup := false;
        Node[i].tempOutgroup := False;
        Node[i].hidden := false;
        Node[i].flag := false;
        Node[i].hilighted := false;
        Node[i].emphasized := false;
        Node[i].CustomHighlightColor := clWhite;
        Node[i].angle := 0.0;
        Node[i].branch.length := 0.0;
        Node[i].branch.SE := 0.0;
        Node[i].branch.maxlen1 := 0.0;
        Node[i].branch.maxlen2 := 0.0;
        Node[i].branch.bootnum := 0;
        Node[i].branch.stats := 0.0;
        Node[i].branch.stat2 := 0;
        Node[i].branch.StatsStdDev := 0;
        Node[i].attrindex := 0;
        Node[i].namesize.x := 0;
        Node[i].namesize.y := 0;
        Node[i].groupindex := -1;
        Node[i].compressedIndex := -1;
        Node[i].capdepth   := 0;
        Node[i].bracket.Left   := 0;
        Node[i].bracket.Right  := 0;
        Node[i].bracket.Top    := 0;
        Node[i].bracket.Bottom := 0;
        Node[i].IsGeneDuplication := False;
        Node[i].IsSpeciationEvent := False;
        Node[i].DataCoverage := 0.0;
        Node[i].h0 := 0.0;
        Node[i].otuInfo := nil;
        Node[i].sortValue := 0;
        Node[i].tempValue := 0.0;
    end;
    for i := NoOfOTUs+1 to NoOfNodes do
    begin
        NEW(Node[i]);
        Node[i].index:= i;
        Node[i].size := 0;
        Node[i].depth := 0;
        Node[i].height := 0.0;
        Node[i].maxh := 0;
        Node[i].minh := 0;
        Node[i].rate := 0;
        Node[i].width := 0;
        Node[i].anc := nil;
        Node[i].des1 := nil;
        Node[i].des2 := nil;
        Node[i].name := '';
        Node[i].SpeciesName := EmptyStr;
        Node[i].PrivateName := '';
        Node[i].OTU := false;
        Node[i].compressed := false;
        Node[i].autoCompressed := False;
        Node[i].marker.shape := msNone;
        Node[i].marker.color := clBlack;
        Node[i].outgroup := false;
        Node[i].tempOutgroup := False;
        Node[i].hidden := false;
        Node[i].flag := false;
        Node[i].hilighted := false;
        Node[i].emphasized := false;
        Node[i].angle := 0.0;
        Node[i].branch.length := 0.0;
        Node[i].branch.SE := 0.0;
        Node[i].branch.maxlen1 := 0.0;
        Node[i].branch.maxlen2 := 0.0;
        Node[i].branch.bootnum := 0;
        Node[i].branch.stats := 0.0;
        Node[i].branch.stat2 := 0;
        Node[i].branch.StatsStdDev := 0;
        Node[i].attrindex := 0;
        Node[i].namesize.x := 0;
        Node[i].namesize.y := 0;
        Node[i].groupindex := -1;
        Node[i].compressedIndex := -1;
        Node[i].capdepth   := 0;
        Node[i].bracket.Left   := 0;
        Node[i].bracket.Right  := 0;
        Node[i].bracket.Top    := 0;
        Node[i].bracket.Bottom := 0;
        Node[i].IsGeneDuplication := False;
        Node[i].IsSpeciationEvent := False;
        Node[i].DataCoverage := 0.0;
        Node[i].h0 := 0.0;
        Node[i].otuInfo := nil;
        Node[i].sortValue := 0;
        Node[i].tempValue := 0.0;
    end;
    Root := nil;
    ClearAttrib;
end;

procedure TTreeCustomControl.ResetMem;
var i : integer;
begin
    if NoOfOTUs = 0 then Exit;

    if Node <> nil then
      for i := 1 to NoOfNodes do
        Dispose(Node[i]);
    FreeMemAndNil(Node,SizeOf(TpNode)*NoOfNodes);
    Node := nil;
    FreeDistMatrix(FDistanceMatrix, NoOfOTUs);
    FDistanceMatrix := nil;
    ClearAttrib;

    FFocusedIndex := 0;
    FFocusedNameIndex := 0;
    FNodeFocused := false;
    FBranchFocused := false;
    FTimeFocused := false;
    FTitle := '';
    FDescription := '';
    FSource := '';
    FDistance := '';
    FGap := '';
    FMethod := '';
    FisValue := false;
    FisValue2 := false;
    FisStats := false;
    FisSE := false;
    FisRooted := false;
    FValueName := '';
    FValue2Name := '';
    FStatsName := '';
    FValue := 0.0;
    FValue2 := 0.0;
    FMaxStats := 0.0;
    FNoOfOTUs := 0;
    FPixelsPerUnit := 0;
    FSBL := 0.0;
    Scale := 0.0;
    FScaleTick := 0.0;
    FScaleText := '';
    TimeScale := 0.0;
    FTimeTick := 0.0;
    FTimeFactor := 0.0;
    FTimeText := '';
    FTreeExist := false;
end;

procedure TTreeCustomControl.Clear;
begin
    if NoOfOTUs > 0 then ResetMem;
end;

function TTreeCustomControl.GetLongestPath:double;
var
  i: integer;
begin
  result := 0.0;
  if ForceLinearized and (Root.height > 0.0000000000001) then
    result := 2*Root.height
  else if isBranchLength then
    for i := 1 to NoOfOTUs do
      if result < Node[i].branch.maxlen1 then
        result := Node[i].branch.maxlen1;
end;

function TTreeCustomControl.GetMinBranchLength:double;
var
  i: integer;
begin
  result := 0.0;
  if not isBranchLength then
    Exit;
  result := LongestPath;
  if (IsLinearized or ForceLinearized) then
    for i := 1 to NoOfNodes do
    begin
      if Node[i] = Root then Continue;
      if result > (Node[i].anc.height-Node[i].height) then
        result := Node[i].anc.height-Node[i].height;
    end
  else
    for i := 1 to NoOfNodes do
    begin
      if Node[i] = Root then Continue;
      if result > Node[i].branch.length then
        result := Node[i].branch.length;
    end;

end;

function TTreeCustomControl.GetMaxBranchLength:double;
var
  i: integer;
begin
  result := 0.0;
  if not isBranchLength then
    Exit;
  for i := 1 to NoOfNodes do begin
    if Node[i] = Root then Continue;
    if result < Node[i].branch.length then
      result := Node[i].branch.length;
  end;
end;

procedure TTreeCustomControl.SetRootAtLast;
var
  d1,d2 : TpNode;
  b : TBranch;
begin
    Assert(Assigned(Node[NoOfNodes].anc), 'missing ancestor of node');
    if  Node[NoOfNodes] = Node[NoOfNodes].anc.des1 then
        Node[NoOfNodes].anc.des1 := Root
    else
        Node[NoOfNodes].anc.des2 := Root;
    Root.anc := Node[NoOfNodes].anc;
    Node[NoOfNodes].anc := nil;

    d1 := Root.des1;
    d2 := Root.des2;

    Root.des1 := Node[NoOfNodes].des1;
    Root.des2 := Node[NoOfNodes].des2;
    Node[NoOfNodes].des1.anc := Root;
    Node[NoOfNodes].des2.anc := Root;

    Node[NoOfNodes].des1 := d1;
    Node[NoOfNodes].des2 := d2;
    d1.anc := Node[NoOfNodes];
    d2.anc := Node[NoOfNodes];

    b := Root.branch;
    Root.branch := Node[NoOfNodes].branch;
    Node[NoOfNodes].branch := b;

    Root := Node[NoOfNodes];
    IsRooted := True;
end;

procedure TTreeCustomControl.InitTree;

    function MinBLenDecimals:integer;
    begin
      result := 0;
      if LongestPath > 0 then
        if floor(log10(LongestPath)) > 0 then
          result := 0
        else
          result := -floor(log10(LongestPath)) +1;
    end;

var
  i: integer;
begin
    for i := NoOfNodes downto NoOfOTUs+1 do
        if Node[i].anc = nil then begin
            Root := Node[i];
            Break;
        end;

    if Root <> Node[NoOfNodes] then
      SetRootAtLast;

    FSBL := 0.0;
    for i := 1 to NoOfNodes-1 do
      FSBL := FSBL +Node[i].branch.length;

    SetStats;
    if isBranchLength then
    begin
      SetMaxLength(Root);
      if Root.des1.branch.maxlen2 > Root.des2.branch.maxlen2 then
          xmax := Root.des1.branch.maxlen2
      else
          xmax := Root.des2.branch.maxlen2;
    end;
    if isBranchLength then
    begin
      if FBLenDecimals < MinBLenDecimals then
        FBLenDecimals := MinBLenDecimals;
    end;
    if isStats then
      InitStatsMargins;

    if isBranchLength then
      Topoflag := FTopologyOnly
    else
      Topoflag := true;

    ShowOTUName := true;
    ShowOTUMarker := true;
    FTreeStyle := tsTraditional;
    FBranchStyle := bsRectangular;
    SetClusterSize(Root);
    SetClusterWidth(Root);
    if not isBranchFixed then
    begin
      if not isRooted then
      begin
        if isOutgroup then
        begin
          for i := 1 to NoOfOTUs do
            Node[i].flag := Node[i].outgroup;
          MoveRoot(SearchCommonAncestor(Root),true);
        end
        else if isBranchLength then
          MoveRoot(Node[SearchMidPoint],true);
        SetClusterSize(Root);
        SetClusterWidth(Root);
      end;
    end;
    SetClusterHeight;

    SetAttrindex;

    SetPosition;
    SetTreeSize;

    InitScale;

    FTreeExist := true;
end;

procedure TTreeCustomControl.InitStatsMargins;
var
  i, j: Integer;
begin
  Assert(yunit > 0);
  FStatsMargin.X := BranchPen.Width + 4;
  i := (yunit - 2*BranchPen.Width - Abs(StatsFont.Height)) div 2;
  j := BranchPen.Width*2 + 2*4;
  FStatsMargin.Y := min(i, j);
end;

procedure TTreeCustomControl.SetEdgePen(AValue: TPen);
begin
  if FEdgePen=AValue then Exit;
  FEdgePen:=AValue;
end;

procedure TTreeCustomControl.SetFillBrush(AValue: TBrush);
begin
  if FFillBrush=AValue then Exit;
  FFillBrush:=AValue;
end;

procedure TTreeCustomControl.SetMarkerPen(AValue: TPen);
begin
  if FMarkerPen=AValue then Exit;
  FMarkerPen:=AValue;
end;

procedure TTreeCustomControl.SetOpenBrush(AValue: TBrush);
begin
  if FOpenBrush=AValue then Exit;
  FOpenBrush:=AValue;
end;

function TTreeCustomControl.GetIsDistanceMatrix:boolean;
begin
  Result := (FDistanceMatrix <> nil);
end;

procedure TTreeCustomControl.SetCalibratedNodeIndices(NodeIndices: ArrayOfInteger);
var
  i: Integer;
begin
  SetLength(FCalibratedNodes, Length(NodeIndices));
  if Length(NodeIndices) > 0 then
  begin
    for i := 0 to Length(NodeIndices) - 1 do
      FCalibratedNodes[i] := NodeIndices[i];
  end;
end;

procedure TTreeCustomControl.SetCenterMargin(value: integer);
begin
  if (value >= 20) and (value <= 80) then
    FCenterMargin := value;
end;

function TTreeCustomControl.GetMetafileHandle: THandle;
begin
  result := hMetaFile;
end;

procedure TTreeCustomControl.GetDescName(nodeindex: integer; names: TStringList);

  procedure GetDescNameRecursive(node: TpNode);
  begin
    if node.OTU then
      names.Add(node.name)
    else
    begin
      GetDescNameRecursive(node.des1);
      GetDescNameRecursive(node.des2);
    end;
  end;


begin
  names.Clear;
  GetDescNameRecursive(Node[nodeindex]);
end;

procedure TTreeCustomControl.BringOtuNameIntoView(aOtuName: String);
var
  index: Integer = -1;
  TreeNameCoords: TRect;
  p: TPoint;
begin
  index := IndexOfName[aOtuName];
  if index = -1 then
    exit;

  TreeNameCoords := NodeCoords(index);
  p.X := TreeNameCoords.Left;
  if XCoordInView(p.X) then
    p.X := ScrollLeft;

  p.Y := TreeNameCoords.Top - Top;
  if YCoordInView(p.Y) then
    p.Y := ScrollTop;

  if not PtInRect(FPaintingArea, p) then
    ScrollTo(p.X, p.Y);
  Invalidate;
end;

{ TTreeBox}

constructor TTreeBox.Create(AOwner : TComponent; aType: TTreeBoxType);
begin
    inherited Create(AOwner, aType);
    FAncStateProc := nil;
    FFileVersion := 16; { this is the version of a session file that is read - default set to current version}
    MyCurrentVersion := 16; { this is the version of this release}
    FNoOfTrees := 0;
    InitDefaultValues;
end;

destructor TTreeBox.Destroy;
begin
    if Self <> nil then
      inherited Destroy;
    if Assigned(TreeList) then
      TreeList.Free;
    if Assigned(FPartitionList) then
      FPartitionList.Free;
    if Assigned(ConsTree) then
      ConsTree.Free;
end;

function TTreeBox.GetSubtreeAttribIndex(const nodeIndex: Integer; var IsGroupAttribute: Boolean): Integer;
var
  attributeIndex: integer;
begin
  Assert(nodeIndex >= 0, Format('bad node index = %d when getting attributes', [nodeIndex]));
  Assert(nodeIndex <= NoOfNodes, Format('bad node index = %d when getting attributes', [nodeIndex]));
  if nodeindex <= 0 then
    attributeIndex := BASE_NODE_ATTRUBUTE_INDEX
  else if nodeindex > NoOfNodes then
    attributeIndex := BASE_NODE_ATTRUBUTE_INDEX
  else if node[nodeindex] = Root then
    attributeIndex := BASE_NODE_ATTRUBUTE_INDEX
  else
    attributeIndex := Node[nodeindex].attrindex;
  if attributeIndex < BASE_NODE_ATTRUBUTE_INDEX then
  begin
    IsGroupAttribute := True;
    Result := -(attributeIndex+1);
  end
  else
  begin
    IsGroupAttribute := False;
    Result := attributeIndex;
  end;
end;

procedure TTreeBox.InitDefaultValues;
begin
  IsGeneDups := False;
  HasGeneDups := False;
  HasSpeciations := False;
  ShowGeneDupMarkers := False;
  ShowSpeciationMarkers := False;
  ForceLinearized := False;
  Radius := 300;
  StartAngle := 0;
  CenterMargin := 20;
  AutoSize := True;
  TreeWidth := 400 ;
  PixelsPerGroupMember := 8;
  PixelsPerOTU := DEFAULT_PPOTU;
  TreeStyle := tsTraditional;
  BranchStyle := bsRectangular;
  FillSubtreeDelta := True;
  StatsPosition := bipAutomatic;
  TimesPosition := bipAutomatic;
  BLenPosition := bipAutomatic;
  BLenDecimals := 0;
  ShowOTUName := True;
  ShowSpeciesName := False;
  ShowSelection := True ;
  ShowOTUMarker := False;
  ShowStats := False;
  ShowDataCoverage := False;
  ShowDivergenceTimes := False;
  ShowNodeIds := False;
  ShowBLen := False;
  ShowScale := True;
  ShowTimeScale := True;
  ShowSamplingTimeScale := False;
  ShowCharState := False;
  ShowHeightErrBar := True;
  ShowRoot := True;
  ShowTopologyOnly := False;
  HorzTaxonName := False;
  AlignCaption := False;

  Color := clWhite;
  EditNameEnabled := True;
  ShowMappedTreeNames := False;
  Font.Charset := DEFAULT_CHARSET;
  Font.Color := clWindowText;
  Font.Size := 12;
  Font.Name := DEFAULT_FONT_NAME;//'MS Sans Serif';
  Font.Style := [];
  ParentColor := False;
  ParentFont := False;

  FreqDecimals := 0;
  TreeIndex := 0;
  isTimes := False;
  ConsensusValue := 50;
  CondenseValue := 50;
  SiteIndex := 0;
  MaxSiteIndex := 0;
  FDivTimeDecimals := 1;
end;

procedure TTreeBox.AssignPartitionList(AList: TPartitionList);
begin
  AList.Assign(FPartitionList);
end;

function TTreeBox.HasPartitions: Boolean;
begin
  Result := ((Assigned(FPartitionList)) and (FPartitionList.NoOfPartitions > 0));
end;

procedure TTreeBox.AssignTreeAttrib(Source: TTreeCustomControl);
var
  OtherTree: TTreeBox;
begin
  inherited;

  if Source is TTreeBox then
  begin
    FConsensusValue  := TTreeBox(Source).FConsensusValue;
    FFreqDecimals := TTreeBox(Source).FFreqDecimals;

    if (TTreeBox(Source).TreeIndex = 0) and (TreeIndex <> 0) then
      CondenseValue := TTreeBox(Source).ConsCondenseValue
    else if (TreeIndex = 0) and ((TTreeBox(Source).TreeIndex <> 0)) then
    begin
      ConsCondenseValue := TTreeBox(Source).FCondenseValue;
      FCondenseValue    := ConsensusValue;
    end;
    OtherTree := TTreeBox(Source);
    FShowDivergenceTimes := OtherTree.ShowDivergenceTimes;
    FTimeFactor := OtherTree.TimeFactor;
    FShowHeightErrBar := OtherTree.ShowHeightErrBar;
    FIsLinearized := OtherTree.IsLinearized;
    FTimeText := OtherTree.TimeText;
  end;
end;

procedure TTreeBox.SetOtuInfo(otuInfo: TList);
var
  i: Integer;
begin
  if NoOfOTUs = 0 then
    Exit;
  if Assigned(otuInfo) and (otuInfo.Count > 0) then
    for i := 0 to otuInfo.Count - 1 do
    begin
      if Assigned(otuInfo[i]) then
        Node[i + 1].otuInfo := otuInfo[i]
      else
        Node[i + 1].otuInfo := nil;
    end;
end;

function TTreeBox.ReplaceTaxaNames(namesList: TStringList): Integer;
var
  lookup: TFPHashList = nil;
  i: Integer;
  index: Integer;
  n: TpNode = nil;
begin
  Result := 0;
  if namesList.Count = 0 then
    Exit;
  try
    lookup := TFPHashList.Create;
    for i := 1 to NoOfOTUs do
      lookup.Add(Node[i].name, Node[i]);
    for i := 0 to namesList.Count - 1 do
    begin
      index := lookup.FindIndexOf(namesList.Names[i]);
      if index >= 0 then
      begin
        n := TpNode(lookup.Items[index]);
        n.name := namesList.ValueFromIndex[i];
        inc(Result);
      end;
    end;
    if Result > 0 then
      for i := 1 to NoOfOTUs do
        TreeList.OTUName[i - 1] := Node[i].name;
  finally
    if Assigned(lookup) then
      lookup.Free;
  end;
end;

function TTreeBox.NumGroups: Integer;
begin
  Result := 0;
  if Assigned(GroupAttrib) then
    Result := GroupAttrib.Count;
end;

procedure TTreeBox.SaveTreeData(index : integer);
var i : integer;
    tree : TTreeData;
begin
    if index = 0 then
        tree := ConsTree
    else
        tree := TreeList[index-1];
    for i := 1 to NoOfOTUs-1 do begin
        tree.NodeArray[i-1].des1 := Node[NoOfOTUs+i].des1.index-1;
        tree.NodeArray[i-1].des2 := Node[NoOfOTUs+i].des2.index-1;
        tree.IsOutgroupMember[i-1] := Node[i].outgroup;
        tree.InternalNodeLabel[i-1] := Node[NoOfOtus+i].name;
    end;
    if tree.isBLen then
        for i := 0 to NoOfNodes-2 do
            tree.BLen[i] := Node[i+1].branch.length;
    if tree.isSE then
        for i := 0 to NoOfNodes-2 do
            tree.SE[i] := Node[i+1].branch.SE;
    for i := 0 to NoOfNodes-2 do
        tree.Stats[i] := Node[i+1].branch.stats;
    if tree.IsStatsStdDev then
      for i := 0 to NoOfNodes - 2 do
        tree.StatsStdDev[i] := Node[i + 1].branch.statsStdDev;
end;

procedure TTreeBox.LoadTreeData(index : integer);
var
  i,j,k : integer;
  tree : TTreeData;
begin
    if index = 0 then
      tree := ConsTree
    else
      tree := TreeList[index-1];
    if (not ShowTopologyOnly) and (@BLenFunc <> nil) and (tree.SBL < 0.000000000001) then
    begin
      if not tree.isBLen then
        tree.isBLen := true;
      BLenFunc(tree);
      if @SEFunc <> nil then
      begin
        if not tree.isSE then
          tree.isSE := true;
        SEFunc(tree);
      end;
      Initialized[index] := false;
    end;

    Topoflag := (not tree.isBLen) or ShowTopologyOnly or IsCondensed;

    FValue := tree.Value;
    FValue2 := tree.Value2;
    for i := 1 to NoOfNodes do begin
        Node[i].anc := nil;
        Node[i].compressed := false;
        Node[i].autoCompressed := False;
        Node[i].hidden := false;
        Node[i].flag := false;
    end;
    for i := 1 to NoOfOTUs-1 do begin
        j := tree.NodeArray[i-1].des1+1;
        k := tree.NodeArray[i-1].des2+1;
        Node[NoOfOTUs+i].des1 := Node[j];
        Node[NoOfOTUs+i].des2 := Node[k];
        Node[j].anc := Node[NoOfOTUs+i];
        Node[k].anc := Node[NoOfOTUs+i];
        if Index <> 0 then
          Node[NoOfOTUs+i].name := tree.InternalNodeLabel[i-1]
        else
          Node[NoOfOTUs+i].name := EmptyStr;
        Node[NoOfOTUs+i].charstate := EmptyStr;
    end;
    for i := NoOfNodes downto NoOfOTUs+1 do
        if Node[i].anc = nil then begin
            Root := Node[i];
            Break;
        end;
    for i  := NoOfNodes downto NoOfOTUs + 1 do
      Node[i].dataCoverage := tree.DataCoverage[i - NoOfOTUs - 1];
    if tree.isBLen then
        for i := 1 to NoOfNodes do begin
            if Node[i] = Root then Continue;
            Node[i].branch.length := tree.BLen[i-1];
        end;
    if tree.isSE then
        for i := 1 to NoOfNodes do begin
            if Node[i] = Root then Continue;
            Node[i].branch.SE := tree.SE[i-1];
        end;
      for i := 1 to NoOfNodes do
      begin
        if Node[i] = Root then Continue;
        Node[i].branch.stats := tree.Stats[i-1];
        if tree.HasMeanBcl then
          Node[i].branch.MeanBcl := tree.MeanBcl[i - 1];
        if tree.IsStatsStdDev then
          Node[i].branch.StatsStdDev := tree.StatsStdDev[i - 1];
      end;

    if IsGeneDups then
      for i := (NoOfOtus + 1) to NoOfNodes do
      begin
        Node[i].IsGeneDuplication := tree.IsGeneDupEvent[i-1];
        Node[i].IsSpeciationEvent := tree.IsSpeciationEvent[i-1];
        Node[i].dataCoverage := tree.DataCoverage[i - NoOfOtus - 1];
        if Node[i].IsGeneDuplication then
        begin
          if not FHasGeneDups then
            FHasGeneDups := True;
        end
        else if Node[i].IsSpeciationEvent then
        begin
          if not FHasSpeciations then
            FHasSpeciations := True;
        end;
      end;
    FTreeExist := true;
end;

procedure TTreeBox.SetOutgroups(outgroup : boolean);
var i : integer;

    procedure MarkOutgroup(n: TpNode);
    begin
        if n.OTU then
            n.flag := true
        else begin
            MarkOutgroup(n.des1);
            MarkOutgroup(n.des2);
        end;
    end;

begin
    if outgroup then
        for i := 1 to NoOfOTUs do
            Node[i].flag := Node[i].outgroup
    else begin
        for i := 1 to NoOfOTUs do
            Node[i].flag := false;
        MarkOutgroup(Root.des2);
    end;
end;

procedure TTreeBox.SetStats;
var
  i : integer;
  rootDescendantsStats: Integer;
  rootMeanBcl: Double = 0;
begin
  if MaxStats > 0.0 then
    for i := 1 to NoOfNodes do
    begin
      if Node[i] = Root then Continue;
      if Node[i].branch.stats < -0.0000000000001 then
        Node[i].branch.stat2 := -1
       else
        Node[i].branch.stat2 := Trunc(Node[i].branch.stats*100/MaxStats+0.000000000001);
    end
  else
    for i := 1 to NoOfNodes do
    begin
      if Node[i] = Root then Continue;
      if Node[i].branch.stats < -0.0000000000001 then
        Node[i].branch.stat2 := -1
      else
        Node[i].branch.stat2 := Trunc(Node[i].branch.stats+0.000000000001);
    end;

  rootDescendantsStats := Max(Root.des1.branch.stat2, Root.des2.branch.stat2);
  Root.des1.branch.stat2 := rootDescendantsStats;
  Root.des2.branch.stat2 := rootDescendantsStats;
  if CompareValue(Root.des1.branch.meanBcl, Root.des2.branch.meanBcl, FP_CUTOFF) > 0 then
    rootMeanBcl := Root.des1.branch.meanBcl
  else
    rootMeanBcl := Root.des2.branch.meanBcl;
  Root.des1.branch.meanBcl := rootMeanBcl;
  Root.des2.branch.meanBcl := rootMeanBcl;
end;

function TTreeBox.GetDataInitialized(index: integer):boolean;
begin
  result := false;
  if FDataInitialized = nil then Exit;
  result := FDataInitialized[index];
end;

procedure TTreeBox.SetTopologyOnly(value : boolean);
begin
  inherited;

  if FDataInitialized = nil then Exit;
  if (not value) and (@BLenFunc <> nil) then
  begin
    SetTreeIndex(TreeIndex);
    if Scale = 0.0 then
      InitScale;
    Refresh;
  end;
end;

procedure TTreeBox.InitTreeData(index : integer);
var
  tree, tmptree : TTreeData;
begin
  if index = 0 then
  begin
    MakeConsensusTree;
    tree := ConsTree;
    if @BLenFunc <> nil then
      tree.isBLen := true;
    if @SEFunc <> nil then
      tree.isSE := true;
  end
  else
  begin
    tree := TreeList[index-1];
    if Source = 'PartitionList' then
    begin
      tmptree := FPartitionList.ExtractTreeData(index-1);
      tree.Assign(tmptree);

      if @BLenFunc <> nil then
        tree.isBLen := true;

      if @ValueFunc <> nil then
        tree.Value := ValueFunc(tree)
      else if FPartitionList.ValueName <> '' then
        tree.Value := FPartitionList.Value[index-1];
      if @Value2Func <> nil then
        tree.Value2 := Value2Func(tree)
      else if FPartitionList.Value2Name <> '' then
        tree.Value2 := FPartitionList.Value2[index-1];
      tree.Freq := FPartitionList.Freq[index-1];
    end
    else
    begin
      if @StatsFunc <> nil then
        StatsFunc(tree);

      if @ValueFunc <> nil then
          tree.Value := ValueFunc(tree);
      if @Value2Func <> nil then
          tree.Value2 := Value2Func(tree);
    end;
  end;
  FDataInitialized[index] := true;
end;

procedure TTreeBox.SetTreeIndex(index : integer);
var p : TpNode;
begin
    if (index < 0) or (index > NoOfTrees) then Exit;
    if (index = 0) and not isConsensus then Exit;

    if NodeFocused and BranchFocused then
      ClearFocus;

    SaveTreeData(TreeIndex);

    if not FDataInitialized[index] then
      InitTreeData(index);

    if (TreeIndex = 0) and (index <> 0) then
    begin
      IsCondensed      := ConsCondensed;
      ShowTopologyOnly := ConsTopoOnly;
      FIsRooted        := ConsRooted;
      FCondenseValue   := ConsCondenseValue;
    end
    else if (TreeIndex <> 0) and (index = 0) then
    begin
      ConsCondensed := IsCondensed;
      ConsTopoOnly  := ShowTopologyOnly;
      ConsRooted    := FIsRooted;
      FIsCondensed  := true;
      FTopologyOnly := true;
      FIsRooted     := false;
      ConsCondenseValue := FCondenseValue;
      FCondenseValue    := ConsensusValue;
    end;

    LoadTreeData(index);
    FTreeIndex := index;

    if not Initialized[index] then
      if isOutgroup then
        SetOutgroups(true)
      else
        SetOutgroups(false);

    SetStats;
    if isBranchLength then SetMaxLength(Root);
    SetClusterSize(Root);
    SetClusterWidth(Root);
    SetAttribListToAttributesForActiveTree;
    SetCompressedAttribList;
    SetAttrindex;
    if not Initialized[index] then
    begin
      if isOutgroup and (not isRooted) then
      begin
        p := SearchCommonAncestor(Root);
        if p.anc <> Root then
        begin
          MoveRoot(p, true);
          SetClusterSize(Root);
          SetClusterWidth(Root);
//          SortBranchByFigure(Root);
        end;
      end
      else if index = 0 then
      begin
        if not isRooted then
        begin
          if isBranchLength then
              MoveRoot(Node[SearchMidPoint], true)
          else
              MoveRoot(SearchCommonAncestor(Root), false);
          SetClusterSize(Root);
          SetClusterWidth(Root);
        end;
//        SortBranchByFigure(Node^[NoOfNodes]);
      end
      else if not isBranchFixed then
      begin
        if (not isRooted) and isBranchLength then
        begin
          MoveRoot(Node[SearchMidPoint], true);
          SetClusterSize(Root);
          SetClusterWidth(Root);
        end;
//        SortBranchByFigure(Node^[NoOfNodes]);
      end;
      SetAttribListToAttributesForActiveTree;
      SetCompressedAttribList;
      SetAttrindex;
    end;
    SetClusterHeight;

    Initialized[index] :=  FDataInitialized[index];
    if @AncStateProc <> nil then SetAncState;
end;

function TTreeBox.GetStatsName:AnsiString;
begin
    if TreeIndex = 0 then
        Result := CONSENSUS_VALUE
    else
        Result := FStatsName;
end;

function TTreeBox.GetCondenseValue: integer;
begin
    if TreeIndex = 0 then
        Result := ConsCondenseValue
    else
        Result := FCondenseValue;
end;

procedure TTreeBox.SetCondenseValue(value : integer);
begin
    if value < 0 then
        value := 0
    else if value > 100 then
        value := 100;
    if TreeIndex = 0 then
        ConsCondenseValue := value
    else
        FCondenseValue := value;
end;

procedure TTreeBox.SetConsensusValue(value : integer);
begin
    if value < 0 then
        FConsensusValue :=  0
    else if value > 100 then
        FConsensusValue :=  100
    else
        FConsensusValue :=  value;
    if TreeIndex = 0 then
        FCondenseValue := FConsensusValue;
end;

procedure TTreeBox.MakeConsensusTree;
var MTree : TPartitionList;
    tree : TTreeData;
    i : integer;
begin
    if not isConsensus then Exit;
    if Source = 'PartitionList' then
      MTree := FPartitionList
    else
    begin
      MTree := TPartitionList.Create(NoOfOTUs, NoOfTrees, false);
      MTree.MaxNoOfTrees := 0;
      for i := 0 to NoOfTrees-1 do
      begin
        tree := TreeList[i];
        MTree.AddTreeData(tree, false);
      end
    end;

    MTree.GetConsensus(ConsTree);

    if @ValueFunc <> nil then
    begin
      ConsTree.Value := ValueFunc(ConsTree);
      ConsTreeIsValue := true;
    end
    else if isValue then
    begin
      ConsTree.Value := 0.0;
      ConsTreeIsValue := false;
    end;
    if @Value2Func <> nil then
    begin
      ConsTree.Value2 := Value2Func(ConsTree);
      ConsTreeIsValue2 := true;
    end
    else if isValue2 then
    begin
      ConsTree.Value2 := 0.0;
      ConsTreeIsValue2 := false;
    end;

    i := MTree.Compare(ConsTree.NodeArray, nil);
    if i < 0 then
      ConsTree.Freq := 0.0
    else
      ConsTree.Freq := MTree.Freq[i];

    for i := 0 to NoOfOTUs-3 do
      ConsTree.Stats[i+NoOfOTUs] := ConsTree.Stats[i];
    for i := 0 to NoOfOTUs-1 do
      ConsTree.Stats[i] := 0.0;

    if not (Source = 'PartitionList') then
      MTree.Free;
end;

procedure TTreeBox.SetOutgroupCluster(AColor: TColor; AIndex: Integer);
var
  theNode: TpNode;

  procedure ProcessNode(ANode: TpNode);
  begin
    ANode.outgroup := True;
    if ANode.OTU then
    begin
      ANode.emphasized := True;
    end
    else
    begin
      ProcessNode(ANode.des1);
      ProcessNode(ANode.des2);
    end;
  end;

begin
  { need to reset it in case the user sets the outgroup multiple times}
  ClearOutgroups;
  theNode := Node[AIndex];
  ProcessNode(theNode);
  if OutgroupIsDes1 then
    HighlightOutgroupCluster(Root, True, AColor)
  else
    HighlightOutgroupCluster(Root, False, AColor);
end;

procedure TTreeBox.InitTree;
begin
    if (NoOfTrees = 0) and (Source = 'PartitionList') then
    begin
      FTreeIndex := 0;
      ConsCondensed := IsCondensed;
      ConsTopoOnly  := ShowTopologyOnly;
      ConsRooted    := FIsRooted;
      FIsCondensed  := true;
      FTopologyOnly := true;
      FIsRooted     := false;
      ConsCondenseValue := FCondenseValue;
      FCondenseValue    := ConsensusValue;
    end
    else
      FTreeIndex := 1;
    CompressedNodeAttributes := CompressedAttributesArray[TreeIndex];
    InitTreeData(TreeIndex);
    LoadTreeData(TreeIndex);
    Initialized[TreeIndex] := FDataInitialized[TreeIndex];
    FConsensusValue := 50;
    ConsCondenseValue := FCondenseValue;
    inherited InitTree;
end;

function TTreeBox.GetSBL(index:integer):double;
var
  tree : TTreeData;
begin
    Result := 0.0;
    if (index < 0) or (index > NoOfTrees) or (not Assigned(FDataInitialized)) then
      Exit;
    if not FDataInitialized[index] then
      InitTreeData(index);
    if index = 0 then
      tree := ConsTree
    else
      tree := TreeList[index-1];
    if tree = nil then
      Result := 0
    else
      Result := tree.SBL;
end;

procedure TTreeBox.SetValue(index: integer; value: double);
var
  tree : TTreeData;
begin
  if (index < 0) or (index > NoOfTrees) then Exit;
  if not isConsensus and (index = 0) then Exit;
  if index = 0 then
      tree := ConsTree
  else
      tree := TreeList[index-1];
  tree.Value := value;
end;

procedure TTreeBox.SetValue2(index: integer; value: double);
var
  tree : TTreeData;
begin
  if (index < 0) or (index > NoOfTrees) then Exit;
  if not isConsensus and (index = 0) then Exit;
  if index = 0 then
      tree := ConsTree
  else
      tree := TreeList[index-1];
  tree.Value2 := value;
end;

function TTreeBox.GetFirstRepresentativeInCluster(p: TpNode): String;
var
  n: TpNode;
begin
  if p.OTU then
    Result := p.name
  else
  begin
    n := p;
    while not n.OTU do
      n := n.des1;
    Result := n.Name;
  end;
end;

procedure TTreeBox.DrawBranchLength;
begin
  if not (SBL[TreeIndex] > FP_CUTOFF) then Exit;
  inherited DrawBranchLength;
end;

procedure TTreeBox.GetSessionProperties(var json: TJsonObject);
begin
  inherited GetSessionProperties(json);
  json.Add('FreqDecimals', FreqDecimals);
end;

procedure TTreeBox.SetSessionProperties(const json: TJsonObject);
var
  data: TJsonData = nil;
begin
  inherited SetSessionProperties(json);
  data := json.Find('FreqDecimals', jtNumber);
  if Assigned(data) then
    FreqDecimals := data.AsInteger;
end;

procedure TTreeBox.SetTaxonGroupName(aTaxon: String; aGroupName: String);
var
  aList: TStringList = nil;
  i: Integer = -1;
  index: Integer = -1;
begin
  try
    aList := TStringList.Create;
    for i := 1 to FNoOfOTUs do
    begin
      if Node[i].groupindex >= 0 then
        aList.Add(Format('%s=%s', [Node[i].name, GroupAttrib[Node[i].groupindex].Name]));
    end;
    index := aList.IndexOfName(aTaxon);
    if index >= 0 then
      aList[index] := Format('%s=%s', [aTaxon, aGroupname])
    else
      aList.Add(Format('%s=%s', [aTaxon, aGroupname]));
    SetGroupInfo(aList);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TTreeBox.SetTaxaGroupName(aGroupName: String; aNodeIndex: Integer);
var
  aList: TStringList = nil;
  i: Integer = -1;
  index: Integer = -1;
  n: TpNode = nil;

  procedure SetNewGroupNameForCluster(aNode: TpNode);
  begin
    if not aNode.OTU then
    begin
      SetNewGroupNameForCluster(aNode.des1);
      SetNewGroupNameForCluster(aNode.des2);
    end
    else
    begin
      if aList.IndexOfName(aNode.name) > 0 then
        aList[aList.IndexOfName(aNode.name)] := Format('%s=%s', [aNode.name, aGroupName])
      else
        aList.Add(Format('%s=%s', [aNode.name, aGroupName]));
    end;
  end;

begin
  try
    { first, build the key=value list of current group assignments to taxa}
    aList := TStringList.Create;
    for i := 1 to FNoOfOTUs do
    begin
      if Node[i].groupindex >= 0 then
        aList.Add(Format('%s=%s', [Node[i].name, GroupAttrib[Node[i].groupindex].Name]));
    end;

    n := Node[aNodeIndex];
    SetNewGroupNameForCluster(n);
    SetGroupInfo(aList);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

procedure TTreeBox.ExpandAutoCompressedClusters(DoRefresh: Boolean);
var
  i: Integer;
begin
  for i := 1 to NoOfNodes do
  begin
    if Node[i].autoCompressed then
      Node[i].compressed := False;
    Node[i].autoCompressed := False;
    Node[i].compressedIndex := -1;
  end;
  CompressedNodeAttributes.DeleteAll;
  FAutoCompressedSettings[GROUPS_COMPRESSED_INDEX][TreeIndex] := False;
  FAutoCompressedSettings[CLUSTERS_COMPRESSED_INDEX][TreeIndex] := False;
  FAutoCompressedSettings[SIMILAR_COMPRESSED_INDEX][TreeIndex] := False;
  if DoRefresh then
  begin
    SetAttrIndex;
    Refresh;
  end;
end;

procedure TTreeBox.AutoCompressCluster(p: TpNode);
var
  attrIndex: Integer = -1;
  a: TNodeAttrib = nil;
  isGroupAttrib: Boolean = False;

  procedure HideNodes(n: TpNode);
  begin
    if Assigned(n.des1) then
    begin
      n.des1.hidden := True;
      HideNodes(n.des1);
    end;
    if Assigned(n.des2) then
    begin
      n.des2.hidden := True;
      HideNodes(n.des2);
    end;
  end;

begin
  a := TNodeAttrib.Create;
  attrIndex := GetSubtreeAttribIndex(p.index, isGroupAttrib);
  if isGroupAttrib then
  begin
    a.Assign(GroupAttrib[attrIndex]);
    a.AutoCompressed := True;
    if a.Caption = EmptyStr then
      a.Caption := a.Name;
    CompressedNodeAttributes.Add(a);
    p.compressedIndex := (CompressedNodeAttributes.Count - 1);
  end
  else
  begin
    a.Assign(AttribList[attrIndex]);
    a.autoCompressed := True;
    CompressedNodeAttributes.Add(a);
    p.compressedIndex := (CompressedNodeAttributes.Count - 1);
    if (a.Name <> EmptyStr) and (a.Caption = EmptyStr) then
      a.Caption := a.Name
    else if a.Caption = EmptyStr then
    begin
      a.Caption := Format('%s...', [GetFirstRepresentativeInCluster(p)]);
    end;
  end;
  a.NodeIndex := p.index;
  ComputeCaptionSize(a);
  HideNodes(p);
end;

procedure TTreeBox.CompressClustersBySize(clusterSize: Int64);

  procedure DoCompress(p: TpNode);
  begin
    if Assigned(p.anc) and (p.size <= clusterSize) { and (p.anc.size > clusterSize)} then
    begin
      AutoCompressCluster(p);
    end
    else if p.size > clusterSize then
    begin
      if Assigned(p.des1) and (not p.des1.OTU) then
        DoCompress(p.des1);
      if Assigned(p.des2) and (not p.des2.OTU) then
        DoCompress(p.des2);
    end;
  end;

begin
  ExpandAutoCompressedClusters(False);
  if Assigned(Root) then
  begin
    DoCompress(Root.des1);
    DoCompress(Root.des2);
    SetClusterWidth(Root);
    FAutoCompressedSettings[CLUSTERS_COMPRESSED_INDEX][TreeIndex] := True;
    SetAttrIndex;
    Refresh;
  end;
end;

procedure TTreeBox.CompressGroups;

  procedure DoCompress(p: TpNode);
  begin
    if (p.groupindex >= 0) and (not p.otu) then
      AutoCompressCluster(p)
    else
    begin
      if Assigned(p.des1) and (not p.des1.OTU) then
        DoCompress(p.des1);
      if Assigned(p.des2) and (not p.des2.OTU) then
        DoCompress(p.des2)
    end;
  end;

begin
  if Assigned(GroupAttrib) and (GroupAttrib.Count > 0) then
  begin
    ExpandAutoCompressedClusters(False);
    DoCompress(Root);
    SetClusterWidth(Root);
    FAutoCompressedSettings[GROUPS_COMPRESSED_INDEX][TreeIndex] := True;
    SetAttrIndex;
    Refresh;
  end
  else
    ExpandAutoCompressedClusters(True);
end;

procedure TTreeBox.CompressSimilarSequences(maxDiff: Extended);

  function BothDescendantsBranchesCompressed(p: TpNode): Boolean;
  begin
    Result := False;
    if (not Assigned(p.des1)) or (not Assigned(p.des2)) then
      Exit;
    Result := (CompareValue(p.des1.branch.length, maxDiff, FP_CUTOFF) < 0);
    if Result then
      Result := (CompareValue(p.des2.branch.length, maxDiff, FP_CUTOFF) < 0);
  end;

  procedure DoCompressClusters(p: TpNode);
  begin
    if p.autoCompressed then
    begin
      AutoCompressCluster(p);
    end
    else
    begin
      if Assigned(p.des1) then
        DoCompressClusters(p.des1);
      if Assigned(p.des2) then
        DoCompressClusters(p.des2);
    end;
  end;

  procedure DoMarkCompressedNodes(p: TpNode);
  begin
    if Assigned(p.des1) then
      DoMarkCompressedNodes(p.des1);
    if Assigned(p.des2) then
      DoMarkCompressedNodes(p.des2);

    if p.OTU then
    begin
      if (not p.autoCompressed) and (CompareValue(p.branch.length, maxDiff, FP_CUTOFF) < 0) then
        p.autoCompressed := True;
    end
    else if p.des1.autoCompressed and p.des2.autoCompressed and BothDescendantsBranchesCompressed(p) then
      p.autoCompressed := True;
  end;

  procedure CompressOnlyRootOfClusters;
  var
    i: Integer;
  begin
    for i := 1 to NoOfNodes do
      if (Node[i].compressedIndex < 0) and Node[i].autoCompressed then
        Node[i].autoCompressed := False;
  end;

begin
  ExpandAutoCompressedClusters(False);
  if Assigned(Root) then
  begin
    DoMarkCompressedNodes(Root);
    DoCompressClusters(Root);
    CompressOnlyRootOfClusters;
    SetClusterWidth(Root);
    FAutoCompressedSettings[SIMILAR_COMPRESSED_INDEX][TreeIndex] := True;
    SetAttrIndex;
    Refresh;
  end;
end;

procedure TTreeBox.UpdateCalibratedNodes(aCalibrations: TCalibrations; showUnselected: Boolean = False);
var
  i: Integer;
  calib: TCalibrationTime;
begin
  SetLength(FCalibratedNodes, 0);
  if Assigned(aCalibrations) and (aCalibrations.Count > 0) then
    for i := 0 to aCalibrations.Count - 1 do
    begin
      calib := aCalibrations.GetCalibration(i);
      if (not calib.IsSelected) and (not showUnselected) then
        continue;
      SetLength(FCalibratedNodes, Length(FCalibratedNodes) + 1);
      if calib.IsSamplingTime then
        FCalibratedNodes[High(FCalibratedNodes)] := TreeList.OtuNameList.IndexOf(calib.NodeA) + 1
      else
      begin
        if (Trim(calib.NodeA) <> EmptyStr) and (Trim(calib.NodeB) <> EmptyStr) then
          FCalibratedNodes[High(FCalibratedNodes)] := FindMrca(calib.NodeA, calib.NodeB)
        else
          FCalibratedNodes[High(FCalibratedNodes)] := calib.TreeViewFormIndex;
      end;
    end;
end;

function TTreeBox.GetAncStatesExportHeader: TAncStatesExportHeader;
var
  i: Integer = -1;
  Child1: Integer = -1;
  Child2: Integer = -1;
  TaxonName: String;
begin
  Result := TAncStatesExportHeader.Create(NoOfNodes);
  if NoOfNodes > 0 then
  begin
    for i := 1 to NoOfNodes do
    begin
      Result[i - 1].NodeId := Node[i].index;
      if i > NoOfOtus then
      begin
        GetIntCoords(i, Child1, Child2);
        TaxonName := IntToStr(Child1);
        Result[i - 1].Child1 := taxonName;
        TaxonName := IntToStr(child2);
        Result[i - 1].Child2 := TaxonName;
        Result[i - 1].NodeLabel := '-';
      end
      else
      begin
        Result[i - 1].Child1 := '-';
        Result[i - 1].Child2 := '-';
        Result[i - 1].NodeLabel := OTUName[i];
      end;
    end;
  end;
end;

procedure TTreeBox.GetInternalNodeLabels(var NodeLabels: TStringList);
var
  i: Integer;
begin
  NodeLabels.Clear;
  for i := 0 to TreeList.InternalNodeLbls[0].Count - 1 do
    NodeLabels.Add(TreeList[0].InternalNodeLabel[i]);
end;

function TTreeBox.GetValue(index:integer):double;
var tree : TTreeData;
begin
    Result := 0.0;
    if (index < 0) or (index > NoOfTrees) then Exit;
    if not isConsensus and (index = 0) then Exit;
    if not FDataInitialized[index] then InitTreeData(index);
    if index = 0 then
        tree := ConsTree
    else
        tree := TreeList[index-1];
    Result := tree.Value;
end;

function TTreeBox.GetValue2(index:integer):double;
var tree : TTreeData;
begin
    Result := 0.0;
    if (index < 0) or (index > NoOfTrees) then Exit;
    if not isConsensus and (index = 0) then Exit;
    if not FDataInitialized[index] then InitTreeData(index);
    if index = 0 then
        tree := ConsTree
    else
        tree := TreeList[index-1];
    Result := tree.Value2;
end;

function TTreeBox.HasSpeciesNames: Boolean;
begin
  if Assigned(TreeList) then
    Result := TreeList.HasSpeciesNames
  else
    Result := False;
end;

procedure TTreeBox.HighlightOutgroupCluster(ANode: TpNode; IsDes1: Boolean; AColor: TColor);
var
  Attrib: TNodeAttrib = nil;
  AFont: TFont = nil;
begin
  try
    ClearTemporaryAttributes;
    if ANode <> nil then
    begin
      Attrib := TNodeAttrib.Create;
      GetGroupAttrib(Attrib, 'outgroup');
      Attrib.IsTemporaryAttribute := True;
      Attrib.LineColor := AColor;
      Attrib.CaptionFont.Color := AColor;
      Attrib.BracketColor := AColor;
      Attrib.OverwriteDownstream := True;
      Attrib.ManualCompressed := True;
      AFont := TFont.Create;
      AFont.Assign(FOTU_Font);
      AFont.Color := AColor;
      Attrib.Font := AFont;
      if IsDes1 then
      begin
        Attrib.NodeIndex := ANode.des1.index;
        ClearSubtreeAttrib(ANode.des2.index, True);
        SetGroupAttrib(Attrib, 'outgroup');
      end
      else
      begin
        Attrib.NodeIndex := ANode.des2.index;
        ClearSubtreeAttrib(ANode.des1.index, True);
        SetGroupAttrib(Attrib, 'outgroup');
      end;
    end;
    HighlightIngroupRootBranch(AColor);
    if Assigned(GroupAttrib) and (GroupAttrib.Count > 0) and Assigned(GroupAttrib[0]) then
      GroupAttrib[0].IsTemporaryAttribute := True;
  finally
    if Assigned(Attrib) then
      Attrib.Free;
    if Assigned(AFont) then { because Attrib is making its own copy}
      AFont.Free;
  end;
end;

function TTreeBox.GetFrequency(index:integer):double;
var tree : TTreeData;
begin
    Result := 0.0;
    if (index < 0) or (index > NoOfTrees) then Exit;
    if not isConsensus and (index = 0) then Exit;
    if not FDataInitialized[index] then InitTreeData(index);
    if index = 0 then
      tree := ConsTree
    else
      tree := TreeList[index-1];
    Result := tree.Freq;
end;

function TTreeBox.GetTotalFrequency:double;
var tree : TTreeData;
    i : integer;
begin
    Result := 0.0;
    if Source = 'PartitionList' then
        Result := FPartitionList.TotalFrequency
    else
        if NoOfTrees > 0 then
            for i := 0 to NoOfTrees-1 do begin
                tree := TreeList[i];
                Result := Result +tree.Freq;
            end;
end;

procedure TTreeBox.InitMem;
var
  i : integer;
begin
    inherited;
    GetMem(AncState, SizeOf(PString)*NoOfNodes);
    for i := 1 to NoOfNodes do
        AncState[i-1] := Addr(Node[i].charstate);
    GetMem(FDataInitialized, SizeOf(Boolean)*(NoOfTrees+1));
    GetMem(Initialized, SizeOf(Boolean)*(NoOfTrees+1));
    GetMem(AttribListArray, SizeOf(TNodeAttribList)*(NoOfTrees+1));
    SetLength(FAutoCompressedSettings, 3, NoOfTrees+1);
    SetLength(CompressedAttributesArray, NoOfTrees + 1);
    for i := 0 to NoOfTrees do
    begin
      FDataInitialized[i] := false;
      Initialized[i] := false;
      AttribListArray[i] := nil;
      CompressedAttributesArray[i] := TNodeAttribList.Create;
      FAutoCompressedSettings[GROUPS_COMPRESSED_INDEX][i] := False;
      FAutoCompressedSettings[CLUSTERS_COMPRESSED_INDEX][i] := False;
      FAutoCompressedSettings[SIMILAR_COMPRESSED_INDEX][i] := False;
    end;
    ConsTreeIsValue := false;
end;

function TTreeBox.GetHasInternalNodeLabels: Boolean;
begin
  if TreeIndex = 0 then
    Result := False
  else
    Result := TreeList[TreeIndex - 1].HasInternalNodeLbls;
end;

function TTreeBox.GetClustersAreAutoCompressed: Boolean;
begin
  if (Length(FAutoCompressedSettings) > CLUSTERS_COMPRESSED_INDEX) and (Length(FAutoCompressedSettings[CLUSTERS_COMPRESSED_INDEX]) > TreeIndex) then
    Result := FAutoCompressedSettings[CLUSTERS_COMPRESSED_INDEX][TreeIndex]
  else
    Result := False;
end;

function TTreeBox.GetGroupsAreAutoCompressed: Boolean;
begin
  if (Length(FAutoCompressedSettings) > GROUPS_COMPRESSED_INDEX) and (Length(FAutoCompressedSettings[GROUPS_COMPRESSED_INDEX]) > TreeIndex) then
    Result := FAutoCompressedSettings[GROUPS_COMPRESSED_INDEX][TreeIndex]
  else
    Result := False;
end;

function TTreeBox.GetHasNonZeroBranchLength: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to NoOfNodes do
    if CompareValue(Node[i].branch.length, 0.0, FP_CUTOFF) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function TTreeBox.GetSimilarSeqsAreAutoCompressed: Boolean;
begin
  if (Length(FAutoCompressedSettings) > SIMILAR_COMPRESSED_INDEX) and (Length(FAutoCompressedSettings[SIMILAR_COMPRESSED_INDEX]) > TreeIndex) then
    Result := FAutoCompressedSettings[SIMILAR_COMPRESSED_INDEX][TreeIndex]
  else
    Result := False;
end;

procedure TTreeBox.ResetMem;
var
  i: integer;
begin
    if ConsTree <> nil then
      ConsTree.Free;
    ConsTree := nil;
    if Assigned(TreeList) then
      FreeAndNil(TreeList);
    if Assigned(FPartitionList) then
      FreeAndNil(FPartitionList);


    if FDistanceMatrix <> nil then
    begin
      FreeDistMatrix(FDistanceMatrix, NoOfOTUs);
      FDistanceMatrix := nil;
    end;

    FreeMemAndNil(AncState);
    FreeMemAndNil(FDataInitialized);
    FreeMemAndNil(Initialized);

    if AttribListArray <> nil then
    begin
      if AttribList.Count > 1 then
        for i := AttribList.Count-1 downto 1 do
          AttribList.Delete(i);
      for i := 0 to NoOfTrees do
        if AttribListArray[i] <> nil then
        begin
          while AttribListArray[i].Count > 0 do
          begin
            AttribListArray[i][0].Free;
            AttribListArray[i].Delete(0);
          end;
          FreeAndNil(AttribListArray[i]);
        end;
      FreeMemAndNil(AttribListArray);
    end;
    if Length(CompressedAttributesArray) > 0 then
    begin
      for i := Low(CompressedAttributesArray) to High(CompressedAttributesArray) do
      begin
        CompressedAttributesArray[i].DeleteAll;
        CompressedAttributesArray[i].Free;
      end;
      SetLength(CompressedAttributesArray, 0);
    end;
    SetLength(FAutoCompressedSettings, 0);
    FNoOfTrees := 0;
    FSiteIndex := 0;
    FMaxSiteIndex := 0;

    inherited ResetMem;
end;

procedure TTreeBox.SetOTUName(index: integer; name: AnsiString);
begin
  TreeList.OTUName[index-1] := name;
  Node[index].name := name;
  Node[index].width := StrLength(name, OTU_Font);
end;

procedure TTreeBox.ResetBaseNodeAttribFonts;
var
  a: TNodeAttrib = nil;
begin
  a := AttribList[0];
  a.Font.Assign(FOTU_Font);
  a.CaptionFont.Assign(FCaptionFont);
end;

procedure TTreeBox.EndUserEditsOTUName(Sender : TObject);
begin
  if FocusedNameIndex < 1 then
    Exit;
  FNamesAreChanged := True;
  inherited;
end;

function TTreeBox.SetTreeList(aTreeList: TTreeList; optimize: boolean):boolean;
var
  i : integer;
begin
  Result := False;
  if aTreeList.NoOfTrees = 0 then
    Exit;

  if NoOfTrees > 0 then
    ResetMem;
  FSource := 'TreeList';
  FisSE := aTreeList.isSE;
  FisStats := (aTreeList.isStats and (FTreeBoxType <> tttReltimeTree));
  FisRooted := aTreeList.isRooted;
  FisBranchFixed := not optimize;
  if aTreeList.ValueName = '' then
      FisValue := False
  else
  begin
      FisValue := True;
      FValueName := aTreeList.ValueName;
  end;
  if aTreeList.Value2Name = '' then
      FisValue2 := False
  else
  begin
      FisValue2 := True;
      FValue2Name := aTreeList.Value2Name;
  end;
  FFreqName := aTreeList.FreqName;
  if FFreqName <> '' then
  begin
    FFreqDecimals := 0;
    for i := 0 to aTreeList.NoOfTrees-1 do
      if Frac(aTreeList[i].Freq) <> 0.0 then
      begin
        FFreqDecimals := 8;
        Break;
      end;
  end;
  FStatsName := aTreeList.StatsName;
  FMaxStats := aTreeList.MaxStats;
  FNoOfTrees := aTreeList.NoOfTrees;
  FNoOfOTUs := aTreeList.NoOfOTUs;

  TreeList := aTreeList;
  if @BLenFunc <> nil then
  begin
    TreeList.isBLen := true;
    FTopologyOnly := true;
  end;
  if @SEFunc <> nil then
    TreeList.isSE := true;
  if @StatsFunc <> nil then
    TreeList.isStats := true;

  if (aTreeList.NoOfTrees > 1) and (aTreeList.TotalFrequency > 0.0) then
    ConsTree := TTreeData.Create(NoOfOTUs, false, false, true);

  try
    InitMem;

    for i := 1 to NoOfOTUs do
    begin
      Node[i].name := aTreeList.OTUname[i-1];
      Node[i].oriname := Node[i].name;
      Node[i].SpeciesName := aTreeList.SpeciesName[i - 1];
      Node[i].PrivateName := '';
      Node[i].outgroup := aTreeList.Items[0].IsOutgroupMember[i - 1];
    end;

    for i := NoOfOTUs + 1 to NoOfNodes do
    begin
      if aTreeList[0].InternalNodeLabel[i - NoOfOTUs - 1] <> EmptyStr then
        Node[i].name := aTreeList[0].InternalNodeLabel[i - NoOfOTUs - 1];
    end;

    if FisValue then
      if @ValueFunc = nil then
      begin
        FValueDecimals := 0;
        for i := 0 to NoOfTrees-1 do
          if Frac(TreeList[i].Value) <> 0.0 then
          begin
            FValueDecimals := 8;
            Break;
          end;
      end
      else
        FValueDecimals := 8;

    if FisValue2 then
      if @Value2Func = nil then
      begin
        FValue2Decimals := 0;
        for i := 0 to NoOfTrees-1 do
          if Frac(TreeList[i].Value2) <> 0.0 then begin
            FValue2Decimals := 8;
            Break;
          end;
      end
      else
        FValue2Decimals := 8;

    if aTreeList.DistanceMatrix <> nil then
        FDistanceMatrix := aTreeList.DistanceMatrix;

    InitTree;
    Result := True;
  except
    ResetMem;
    Result := False;
  end;
end;

procedure TTreeBox.SetTreeListRooted(const AValue: Boolean);
begin
  if TreeList.isRooted = AValue then
    Exit;
  TreeList.IsRooted := AValue;
end;

procedure TTreeBox.SetPartitionList(Partitions: TPartitionList);
var tree : TTreeData;
    i : integer;
begin
    Assert(FTreeBoxType <> tttReltimeTree, 'partition list should not be set for a reltime tree');
    if (NoOfTrees > 0) or (Source = 'PartitionList') then
        ResetMem;

    FPartitionList := Partitions;
    FSource := 'PartitionList';
    FisRooted := FPartitionList.isRooted;
    FisSE := false;
    FisStats := true;
    FStatsName := FPartitionList.StatsName;
    FMaxStats := FPartitionList.MaxStats;

    if FPartitionList.ValueName = '' then
        FisValue := false
    else begin
        FisValue := true;
        FValueName := FPartitionList.ValueName;
        FValueDecimals := 0;
        for i := 0 to FPartitionList.NoOfTrees-1 do
            if Frac(FPartitionList.Value[i]) <> 0.0 then begin
                FValueDecimals := 8;
                Break;
            end;
    end;
    if FPartitionList.Value2Name = '' then
        FisValue2 := false
    else begin
        FisValue2 := true;
        FValue2Name := FPartitionList.Value2Name;
        FValue2Decimals := 0;
        for i := 0 to FPartitionList.NoOfTrees-1 do
            if Frac(FPartitionList.Value2[i]) <> 0.0 then begin
                FValue2Decimals := 8;
                Break;
            end;
    end;
    FFreqName := FPartitionList.FreqName;
    FFreqDecimals := 0;
    for i := 0 to FPartitionList.NoOfTrees-1 do
        if Frac(FPartitionList.Freq[i]) <> 0.0 then begin
            FFreqDecimals := 8;
            Break;
        end;

    FDistanceMatrix := FPartitionList.DistanceMatrix;
    FisBranchFixed := false;

    FNoOfTrees := FPartitionList.NoOfTrees;
    FNoOfOTUs := FPartitionList.NoOfOTUs;
    try
      TreeList := TTreeList.Create;
      TreeList.Information := FPartitionList.Information;

      TreeList.DistanceMatrix := FPartitionList.DistanceMatrix;
      TreeList.FreqName := FPartitionList.FreqName;
      TreeList.ValueName := FPartitionList.ValueName;
      TreeList.Value2Name := FPartitionList.Value2Name;
      TreeList.StatsName := FPartitionList.StatsName;
      TreeList.MaxStats := FPartitionList.MaxStats;
      for i := 0 to NoOfOTUs-1 do
        TreeList.OTUName[i] := FPartitionList.OTUName[i];

      if NoOfTrees > 0 then
        for i := 1 to NoOfTrees do begin
          tree := TTreeData.Create(NoOfOTUs, FPartitionList.isBLen, (@SEFunc <> nil), true);
          TreeList.Add(tree);
        end;

      if (FPartitionList.NoOfPartitions >= (NoOfOTUs-3)) and (FPartitionList.TotalFrequency > 0.0) then
          ConsTree := TTreeData.Create(NoOfOTUs, false, false, true);

      InitMem;

      for i := 1 to NoOfOTUs do begin
          Node[i].name := FPartitionList.OTUname[i-1];
          Node[i].oriname := Node[i].name;
          Node[i].PrivateName := ''; // [dp Sept 23 08] was Node[i].PrivateName := Node[i].name;
      end;

      InitTree;
    except
      ResetMem;
    end;
end;

function TTreeBox.GetPartitionList: TPartitionList;
begin
  Result := FPartitionList;
end;

function TTreeBox.GetIsValue:boolean;
begin
  if @FValueFunc <> nil then
    Result := true
  else if TreeIndex = 0 then
    Result := ConsTreeIsValue
  else
    Result := FisValue;
end;

function TTreeBox.GetIsValue2:boolean;
begin
  if @FValue2Func <> nil then
    Result := true
  else if TreeIndex = 0 then
    Result := ConsTreeIsValue2
  else
    Result := FisValue2;
end;

function TTreeBox.GetIsFreq:boolean;
begin
  if (NoOfTrees <= 1) or (FreqName = '') then
      Result := false
  else if TreeList.TotalFrequency = 0.0 then
      Result := false
  else
      Result := true;
end;

procedure TTreeBox.SetValueFunc(f: TBranchInfoFunc);
begin
    if @f = @FValueFunc then Exit;
    FValueFunc := f;
end;

procedure TTreeBox.UpdateDataCoverage(AData: TTreeData);
var
  i: Integer;
begin
  if Assigned(TreeList[TreeIndex - 1]) then
    TreeList[TreeIndex - 1].AssignDataCoverage(AData);
    for i  := NoOfNodes downto NoOfOTUs + 1 do
      Node[i].dataCoverage := AData.DataCoverage[i - NoOfOTUs - 1];
end;

procedure TTreeBox.SetValue2Func(f: TBranchInfoFunc);
begin
    if @f = @FValue2Func then Exit;
    FValue2Func := f;
end;

function TTreeBox.GetIsConsensus:boolean;
begin
  if ConsTree = nil then
    Result := false
  else
    Result := true;
end;

function TTreeBox.GetIsAncState:boolean;
begin
    Result := (@FAncStateProc <> nil);
end;

procedure TTreeBox.SetAncStateProc(proc: TAncStateProc);
begin
    if @proc = @FAncStateProc then Exit;
    if @proc = nil then ShowCharState := false;
    FAncStateProc := proc;
    SaveTreeData(TreeIndex);
    SetAncState;
end;

function TTreeBox.GetAncState(proc: TAncStateProc; NodeIndex: integer; Site: Integer=-1):AnsiString;
var tree: TTreeData;
begin
    if Site = -1 then
      Site:= FSiteIndex;
    if TreeIndex = 0 then
        tree := ConsTree
    else
        tree := TreeList[TreeIndex-1];
    if NoOfOTUs > 0 then
        proc(AncState, Site, tree);
    Result := AncState[NodeIndex-1]^;
end;

procedure TTreeBox.SetAncState;
var tree: TTreeData;
begin
  if not Assigned(AncStateProc) then
    Exit;
  if TreeIndex = 0 then
      tree := ConsTree
  else
      tree := TreeList[TreeIndex-1];
  if NoOfOTUs > 0 then
      AncStateProc(AncState, FSiteIndex, tree);
end;

procedure TTreeBox.AssignTreeList(ATreeList: TTreeList);
begin
  ATreeList.Assign(TreeList);
end;

function TTreeBox.AncStateProb(AncState: AnsiChar; NodeIndex: integer):double;
var
  tree: TTreeData;
  str: TStringList;
begin
  if @FAncProbFunc = nil then
  begin
      Result := 0.0;
      Exit;
  end;
  if TreeIndex = 0 then
      tree := ConsTree
  else
      tree := TreeList[TreeIndex-1];
  str := TStringList.Create;
  AncProbFunc(NodeIndex, FSiteIndex, tree, str);
  Result := StrToFloat(str.Values[AncState]);
  str.Free;
end;

function TTreeBox.GetShowCharState: boolean;
begin
  if @AncStateProc = nil then
    result := false
  else
    result := inherited GetShowCharState;
end;

procedure TTreeBox.SetSiteIndex(value: integer);
begin
    if value = SiteIndex then Exit;
    FSiteIndex := value;
    if @AncStateProc = nil then Exit;
    SetAncState;
end;

function TTreeBox.SetSpeciesNames(AList: TStringList): Boolean;
var
  i: Integer;
begin
  Result := True;
  { first loop makes sure that they are ordered correctly in TreeList}
  for i := 0 to AList.Count - 1 do
    TreeList.SetSpeciesNameForOtu(AList.ValueFromIndex[i], AList.Names[i]);

  { now that they are ordered correctly, we can just grab them one by one}
  for i := 1 to NoOfOTUs do
      Node[i].SpeciesName := TreeList.SpeciesName[i - 1];
end;

procedure TTreeBox.SetMaxSiteIndex(value: integer);
begin
    if value = FMaxSiteIndex then Exit;
    FMaxSiteIndex := value;
    if FMaxSiteIndex < FSiteIndex then
        FSiteIndex := FMaxSiteIndex;
end;

procedure TTreeBox.GetSubtree(subtree: TTreeBox);
var tree, oritree: TTreeData;
    TL : TTreeList;
    h, k: PArrayOfInt;
    n, i, j : integer;

    procedure GetNodeInfo(node: TpNode);
    begin
      if node.OTU then begin
        TL.OTUName[i] := node.Name;
        if isBranchLength then
          tree.BLenArray[i] := oritree.BLen[node.index-1];
        if isSE then
          tree.SEArray[i] := oritree.SE[node.index-1];
          tree.StatsArray[i] := oritree.Stats[node.index-1];
        h[node.index] := i;
        Inc(i);
      end
      else begin
        GetNodeInfo(node.des1);
        GetNodeInfo(node.des2);
        tree.NodeArray[j].des1 := h[node.des1.index];
        tree.NodeArray[j].des2 := h[node.des2.index];
        if isBranchLength then
          tree.BLenArray[n+j] := oritree.BLen[node.index-1];
        if isSE then
          tree.SEArray[n+j] := oritree.SE[node.index-1];
          tree.StatsArray[n+j] := oritree.Stats[node.index-1];
        h[node.index] := n+j;
        Inc(j);
      end;
    end;

    procedure SetSubtreeNodeAttrib(p: TpNode);
    var a: TNodeAttrib;
    begin
      if (p.attrindex <> p.anc.attrindex) and (p.attrindex > 0) then
      begin
        a := TNodeAttrib.Create;
        a.Assign(AttribList[p.attrindex]);
        subtree.SetSubtreeAttrib(a, h[p.index]+1);
        a.Free;
      end;

      if p.OTU then
        subtree.Node[h[p.index]+1].marker := p.marker
      else
      begin
        SetSubtreeNodeAttrib(p.des1);
        SetSubtreeNodeAttrib(p.des2);
      end;
    end;

    procedure SetSubtreeGroupInfo;
    var
      sl: TStringList;
      i,j: integer;
    begin
      sl := TStringList.Create;
      for i := 1 to NoOfOTUs do
      begin
        if Node[i].groupindex > -1 then
          for j := 1 to subtree.NoOfOTUs do
            if subtree.OTUName[j] = OTUName[i] then
            begin
              sl.Add(OTUName[i]+'='+GroupAttrib[Node[i].groupindex].Name);
              break;
            end;
      end;
      if sl.Count > 0 then
      begin
        subtree.SetGroupInfo(sl);
        for i := 0 to subtree.GroupAttrib.Count-1 do
          for j := 0 to GroupAttrib.Count-1 do
            if subtree.GroupAttrib[i].Name = GroupAttrib[j].Name then
            begin
              subtree.GroupAttrib[i].Assign(GroupAttrib[j]);
              break;
            end;
      end;
      sl.Free;
    end;

begin
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then exit;
    GetMem(h, SizeOf(Integer)*(NoOfNodes+1));
    for i := 0 to NoOfNodes do
      h[i] := NoOfNodes;

    TL := TTreeList.Create;

    TL.isRooted := true;
    TL.ValueName := '';
    TL.Value2Name := '';
    TL.FreqName := '';
    TL.StatsName := StatsName;
    TL.MaxStats := MaxStats;

    SaveTreeData(TreeIndex);
    if TreeIndex = 0 then
      oritree := ConsTree
    else
      oritree := TreeList[TreeIndex-1];
    tree := TTreeData.Create(Node[FocusedIndex].size, isBranchLength, isSE, true);

    i := 0;
    j := 0;
    n := tree.NoOfOTUs;
    GetNodeInfo(Node[FocusedIndex].des1);
    GetNodeInfo(Node[FocusedIndex].des2);
    tree.NodeArray[j].des1 := h[Node[FocusedIndex].des1.index];
    tree.NodeArray[j].des2 := h[Node[FocusedIndex].des2.index];
    TL.Add(tree);

    if isDistanceMatrix then begin
      TL.DistanceMatrix := NewDistMatrix(n, true);
      GetMem(k, SizeOf(Integer)*n);
      for i := 1 to NoOfOTUs do
        if h[i] < n then
          k[h[i]] := i-1;
      for i := 1 to n-1 do begin
        for j := 0 to i-1 do begin
          TL.DistanceMatrix[i][j] := DistanceMatrix[k[i]][k[j]];
          TL.DistanceMatrix[j][i] := TL.DistanceMatrix[i][j];
        end;
      end;
      FreeMemAndNil(k);
    end
    else
      TL.DistanceMatrix := nil;

    subtree.SetTreeList(TL, false);
    subtree.SortClusterInOrder;

    SetSubtreeGroupInfo;

    subtree.AssignTreeAttrib(Self);
    if Node[FocusedIndex].Attrindex >= 0 then
      subtree.AttribList[0].Assign(AttribList[Node[FocusedIndex].Attrindex]);

    SetSubtreeNodeAttrib(Node[FocusedIndex].des1);
    SetSubtreeNodeAttrib(Node[FocusedIndex].des2);

    subtree.SetAttrIndex;

    FreeMemAndNil(h);
end;

function TTreeBox.GetSubtreeTreeList: TTreeList;
var
  addNodeLabels: Boolean = False;
  newTree: TTreeData = nil;
  currentTree: TTreeData = nil;
  TL : TTreeList = nil;
  h: PArrayOfInt;
  n: Integer = 0;
  i: Integer = 0;
  j: integer = 0;

    procedure GetNodeInfo(node: TpNode);
    begin
      if node.OTU then
      begin
        TL.OTUName[i] := node.Name;
        if isBranchLength then
          newTree.BLenArray[i] := currentTree.BLen[node.index-1];
        if isSE then
          newTree.SEArray[i] := currentTree.SE[node.index-1];
        newTree.StatsArray[i] := currentTree.Stats[node.index-1];
        h[node.index] := i;
        Inc(i);
      end
      else
      begin
        GetNodeInfo(node.des1);
        GetNodeInfo(node.des2);
        newTree.NodeArray[j].des1 := h[node.des1.index];
        newTree.NodeArray[j].des2 := h[node.des2.index];
        if isBranchLength then
          newTree.BLenArray[n+j] := currentTree.BLen[node.index-1];
        if isSE then
          newTree.SEArray[n+j] := currentTree.SE[node.index-1];
        if addNodeLabels and (node.name <> EmptyStr) then
          TL[0].InternalNodeLabel[j] := node.name;
        newTree.StatsArray[n+j] := currentTree.Stats[node.index-1];
        h[node.index] := n+j;
        Inc(j);
      end;
    end;

begin
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      exit;
    try
      addNodeLabels := HasInternalNodeLabels;
      GetMem(h, SizeOf(Integer)*(NoOfNodes + 1));
      for i := 0 to NoOfNodes do
        h[i] := NoOfNodes;

      TL := TTreeList.Create;
      TL.isRooted := True;
      TL.ValueName := EmptyStr;
      TL.Value2Name := EmptyStr;
      TL.FreqName := EmptyStr;
      TL.StatsName := StatsName;
      TL.MaxStats := MaxStats;

      SaveTreeData(TreeIndex);
      if TreeIndex = 0 then
        currentTree := ConsTree
      else
        currentTree := TreeList[TreeIndex - 1];
      newTree := TTreeData.Create(Node[FocusedIndex].size, isBranchLength, isSE, true);
      i := 0;
      n := newTree.NoOfOTUs;
      TL.Add(newTree);
      GetNodeInfo(Node[FocusedIndex].des1);
      GetNodeInfo(Node[FocusedIndex].des2);
      newTree.NodeArray[j].des1 := h[Node[FocusedIndex].des1.index];
      newTree.NodeArray[j].des2 := h[Node[FocusedIndex].des2.index];
      TL.DistanceMatrix := nil;
      Result := TL;
    finally
      FreeMemAndNil(h);
    end;
end;

function TTreeBox.GetIsBranchLength:boolean;
var
  tree : TTreeData;
begin
    Result := False;
    if (FTreeIndex < 0) or (FTreeIndex > NoOfTrees) or (not Assigned(FDataInitialized)) then
      Exit;
    if not FDataInitialized[FTreeIndex] then
      InitTreeData(FTreeIndex);
    if FTreeIndex = 0 then
      tree := ConsTree
    else
      tree := TreeList[FTreeIndex-1];
    if tree = nil then
      Result := False
    else
      Result := tree.isBLen;
    //Result := (SBL[TreeIndex] > 0.00000000000001)
end;

function TTreeBox.GetIsSE:boolean;
begin
  if @SEFunc <> nil then
    Result := true
  else if TreeIndex = 0 then
    if isConsensus then
      Result := ConsTree.isSE
    else
      Result := false
  else
    Result := FisSE;
end;

function TTreeBox.GetIsStats:boolean;
begin
  if @StatsFunc <> nil then
    Result := true
  else if TreeIndex = 0 then
    Result := true
  else
    Result := FisStats;
end;

function TTreeBox.GetIsTimes: Boolean;
begin
  Result := FisTimes;
end;

function TTreeBox.GetMaxStats:double;
begin
  if isConsensus and (TreeIndex = 0) then
    if Source = 'PartitionList' then
      Result := FPartitionList.TotalFrequency
    else if TreeList.NoOfTrees > 0 then
      Result := TreeList.TotalFrequency
    else
      Result := FMaxStats
  else
    Result := FMaxStats;
end;

function TTreeBox.ReadFromFile(var data: File; SessionFileVersion: integer):boolean;
var
  buffer : array[0..4095] of AnsiChar;
  color: TColor = clBlack;
  penstyle: TPenStyle = psClear;
  brushstyle: TBrushStyle = bsClear;
  fontstyle: TFontStyles = [];
  m: Integer = -1;
  n: Integer = -1;
  i: Integer = -1;
  j: Integer = -1;
  k: Integer = -1;
  r: double = 0;
  b: boolean = False;
  a: TNodeAttrib;
  blen: boolean = False;
  DivTime: Double = 0;
  penStyleSize: Integer = -1;
begin
  for i := Low(buffer) to High(buffer) do
    buffer[i] := #0;
  if SessionFileVersion >= 703 then
    penStyleSize := SizeOf(TPenStyle)
  else
    penStyleSize := 1;
  if NoOfTrees > 0 then ResetMem;

  BlockRead(data, FNoOfTrees, 4);
  BlockRead(data, FNoOfOTUs, 4);

  TreeList := TTreeList.Create;
  TreeList.ReadFromFile(data, SessionFileVersion);
  FSource := 'MTS';
  blen := TreeList.isBLen;
  FisSE := TreeList.isSE;
  FisStats := TreeList.isStats;
  FisRooted := TreeList.isRooted;
  FisBranchFixed := false;
  FStatsName := TreeList.StatsName;
  FMaxStats := TreeList.MaxStats;
  FFreqName := TreeList.FreqName;
  if TreeList.ValueName = '' then
    FisValue := false
  else begin
    FisValue := true;
    FValueName := TreeList.ValueName;
  end;
  if TreeList.Value2Name = '' then
    FisValue2 := false
  else begin
    FisValue2 := true;
    FValue2Name := TreeList.Value2Name;
  end;

  try
    InitMem;

    for i := 1 to NoOfOTUs do begin
      Node[i].name := TreeList.OTUname[i-1];
      Node[i].oriname := Node[i].name;
      Node[i].PrivateName := '';
    end;

    BlockRead(data, i, 4);
    if i > 0 then begin
      ConsTree := TTreeData.Create(NoOfOTUs, false, false, true);
      if i and 2 = 2 then
        ConsTree.isBLen := true
      else
        ConsTree.isBLen := false;
      if i and 4 = 4 then
        ConsTree.isSE := true
      else
        ConsTree.isSE := false;
      ConsTree.ReadFromFile(data, SessionFileVersion);
      BlockRead(data, ConsTreeIsValue, 1);
      BlockRead(data, ConsTreeIsValue2, 1);

      if TreeList.NoOfTrees = 0 then
      begin
        blen := ConsTree.isBLen;
        FisSE := ConsTree.isSE;
        FisStats := ConsTree.isStats;
      end;
    end;

    BlockRead(data, FTreeIndex, 4);
    BlockRead(data, Initialized^, NoOfTrees+1);
    BlockRead(data, FDataInitialized^, NoOfTrees+1);

    BlockRead(data, FFreqDecimals, 4);
    BlockRead(data, FValueDecimals, 4);
    BlockRead(data, FValue2Decimals, 4);
    BlockRead(data, FBLenDecimals, 4);

    BlockRead(data, FCondenseValue, 4);
    BlockRead(data, FConsensusValue, 4);

    BlockRead(data, FPixelsPerUnit, 8);
    BlockRead(data, FTreeWidth, 4);
    BlockRead(data, MinTreeWidth, 4);

    BlockRead(data, yunit, 4);
    BlockRead(data, gunit, 4);

    BlockRead(data, FRadius, 4);
    BlockRead(data, FStartAngle, 4);
    BlockRead(data, FCenterMargin, 4);

    BlockRead(data, FStatsCutoff, 4);
    BlockRead(data, FStatsPosition, SizeOf(TBranchInfoPosition));
    BlockRead(data, FStatsMargin, SizeOf(TPoint));

    BlockRead(data, r, 8);  //BlockRead(data, FLongestPath, 8);
    BlockRead(data, r, 8);  //BlockRead(data, FMinBranchLength, 8);
    BlockRead(data, r, 8);  //BlockRead(data, FMaxBranchLength, 8);
    BlockRead(data, FBLenCutoff, 8);
    BlockRead(data, FBLenPosition, SizeOf(TBranchInfoPosition));

    BlockRead(data, Scale, 8);
    BlockRead(data, FScaleTick, 8);
    BlockRead(data, TimeScale, 8);
    BlockRead(data, FTimeTick, 8);
    BlockRead(data, FTimeFactor, 8);
    if FileVersion >= 14 then
      BlockRead(data, FLatestTime, 8);

    BlockRead(data, i, 4);
    if i > 0 then begin
      BlockRead(data, buffer, i);
      buffer[i] := #0;
      FScaleText := StrPas(buffer);
    end
    else
      FScaleText := '';
    BlockRead(data, i, 4);
    if i > 0 then begin
      BlockRead(data, buffer, i);
      buffer[i] := #0;
      FScaleUnit := StrPas(buffer);
    end
    else
      FScaleUnit := '';
    BlockRead(data, i, 4);
    if i > 0 then begin
      BlockRead(data, buffer, i);
      buffer[i] := #0;
      FTimeText := StrPas(buffer);
    end
    else
      FTimeText := '';
    BlockRead(data, i, 4);
    if i > 0 then begin
      BlockRead(data, buffer, i);
      buffer[i] := #0;
      FTimeUnit := StrPas(buffer);
    end
    else
      FTimeUnit := '';

    if TreeIndex = 0 then
    begin
      ConsCondensed     := IsCondensed;
      ConsTopoOnly      := ShowTopologyOnly;
      ConsRooted        := FIsRooted;
      FCondenseValue    := ConsensusValue;
    end;

    BlockRead(data, FTreeStyle, SizeOf(TTreeStyle));
    BlockRead(data, FBranchStyle, SizeOf(TBranchStyle));
    BlockRead(data, FIsCondensed, SizeOf(boolean));
    BlockRead(data, FIsLinearized, SizeOf(boolean));
    BlockRead(data, FTopologyOnly, SizeOf(boolean));
    BlockRead(data, FHorzTaxonName, SizeOf(boolean));
    BlockRead(data, FAlignCaption, SizeOf(boolean));
    BlockRead(data, b, 1);
    AttribList[0].ShowTaxonName := b;
    BlockRead(data, b, 1);
    AttribList[0].ShowTaxonMarker := b;
    BlockRead(data, FShowBLen, 1);
    BlockRead(data, FShowStats, 1);
    BlockRead(data, FShowScale, 1);
    BlockRead(data, FShowTimeScale, 1);
    BlockRead(data, FShowSamplingTimeScale, 1);
    BlockRead(data, ConsCondenseValue, 4);

    BlockRead(data, n, 4);
    BlockRead(data, i, 4);
    BlockRead(data, i, 4);
    BlockRead(data, i, 4);
    BlockRead(data, i, 4);
    BlockRead(data, i, 4);

    BlockRead(data, FFileVersion, 4); // Version
    if FileVersion > 0 then
      BlockRead(data, n, 4)
    else
      BlockRead(data, i, 4);
    if FileVersion > 2 then
      BlockRead(data, FShowRoot, 1)
    else
      BlockRead(data, b, 1); //IncludeBranch


    BlockRead(data, b, 1); // ShowCaption

// Default Set
    BlockRead(data, color, SizeOf(TColor));
    FBranchPen.Color := color;
    BlockRead(data, i, 4);
    FBranchPen.Width := i;
    BlockRead(data, penstyle, penStyleSize);
    FBranchPen.Style := ValidPenStyle(penstyle);

    BlockRead(data, color, SizeOf(TColor));
    FBranchPen.Color := color;
    BlockRead(data, i, 4);
    FBranchPen.Width := i;
    BlockRead(data, penstyle, penStyleSize);
    FBranchPen.Style := ValidPenStyle(penstyle);
    BlockRead(data, i, 4);
    if i > 0 then begin
      BlockRead(data, buffer, i);
      buffer[i] := #0;
      FOTU_Font.Name := StrPas(buffer);
    end;
    BlockRead(data, color, SizeOf(TColor));
    FOTU_Font.Color := color;
    BlockRead(data, i, 4);
    FOTU_Font.Size := i;
    a := AttribList[0];
    a.Font.Name := FOTU_Font.Name;
    a.Font.Color := FOTU_Font.Color;
    a.Font.Size := FOTU_Font.Size;
    a.FLinePen.Width := FBranchPen.Width;
    a.FLinePen.Color := FBranchPen.Color;
    a.FLinePen.Style := ValidPenStyle(penstyle);

    if SessionFileVersion >= 703 then
      BlockRead(data, fontstyle, SizeOf(TFontStyle))
    else
      BlockRead(data, fontstyle, 1); // in Delphi, the size was 1
    FOTU_Font.Style := ValidFontStyles(fontstyle);

    BlockRead(data, color, SizeOf(TColor));
    FBrush.Color := color;
    if SessionFileVersion >= 703 then
      BlockRead(data, brushstyle, SizeOf(TBrushStyle))
    else
      BlockRead(data, brushstyle, 1); // size was 1 in Delphi
    FBrush.Style := ValidBrushStyle(brushstyle);

    if n > 1 then
      for i := 1 to n-1 do
      begin
        BlockRead(data, j, 4);
        if AttribListArray[j] = nil then
          AttribListArray[j] := TNodeAttribList.Create;
        a := TNodeAttrib.Create;
        a.ReadFromFile(data, FileVersion, SessionFileVersion);
        AttribListArray[j].Add(a);
      end;

    if SessionFileVersion >= 1007 then
    begin
      BlockRead(data, j, SizeOf(j));
      if j > 0 then
      begin
        if Length(CompressedAttributesArray) < j then
          SetLength(CompressedAttributesArray, j);
        for i := 0 to j - 1 do
        begin
          if not Assigned(CompressedAttributesArray[i]) then
            CompressedAttributesArray[i] := TNodeAttribList.Create;
          BlockRead(data, k, SizeOf(k));
          if k > 0 then
            for m := 0 to k - 1 do
            begin
              a := TNodeAttrib.Create;
              a.ReadFromFile(data, FileVersion, SessionFileVersion);
              CompressedAttributesArray[i].Add(a);
            end;
         end;
      end;

      BlockRead(data, k, SizeOf(k));
      BlockRead(data, m, SizeOf(m));
      SetLength(FAutoCompressedSettings, k, m);
      if k > 0 then
        for j := 0 to k - 1 do
          if m > 0 then
            for i := 0 to m - 1 do
            begin
              BlockRead(data, b, SizeOf(b));
              FAutoCompressedSettings[j][i] := b;
            end;
    end;

    //  from version 6
    if FileVersion >= 6 then
    begin
      for i := 1 to NoOfOTUs do
      begin
        BlockRead(data, n, 4);
        Node[i].groupindex := n;
      end;
      BlockRead(data, n, 4);
      if n > 0 then
        for i := 0 to n-1 do
        begin
          a := TNodeAttrib.Create;
          a.ReadFromFile(data, FileVersion, SessionFileVersion);
          GroupAttrib.Add(a);
        end;
    end;

    if FileVersion >= 8 then
    begin
      BlockRead(data, FUseSubtreeAttrib, 1);
      BlockRead(data, FUseGroupAttrib, 1);
    end;

    BlockRead(data, color, SizeOf(TColor));
    FScalePen.Color := color;
    BlockRead(data, i, 4);
    FScalePen.Width := i;
    BlockRead(data, penstyle, penStyleSize);
    FScalePen.Style := ValidPenStyle(penstyle);

    BlockRead(data, i, 4);
    if i > 0 then begin
      BlockRead(data, buffer, i);
      buffer[i] := #0;
      FStatsFont.Name := StrPas(buffer);
    end;
    BlockRead(data, color, SizeOf(TColor));
    FStatsFont.Color := color;
    BlockRead(data, i, 4);
    FStatsFont.Size := i;
    if SessionFileVersion >= 703 then
      BlockRead(data, fontstyle, SizeOf(TFontStyle))
    else
      BlockRead(data, fontstyle, 1); // in Delphi, the size was 1
    FStatsFont.Style := ValidFontStyles(fontstyle);

    BlockRead(data, i, 4);
    if i > 0 then begin
      BlockRead(data, buffer, i);
      buffer[i] := #0;
      FScaleFont.Name := StrPas(buffer);
    end;
    BlockRead(data, color, SizeOf(TColor));
    FScaleFont.Color := color;
    BlockRead(data, i, 4);
    FScaleFont.Size := i;
    if SessionFileVersion >= 703 then
      BlockRead(data, fontstyle, SizeOf(TFontStyle))
    else
      BlockRead(data, fontstyle, 1); // in Delphi, the size was 1
    FScaleFont.Style := ValidFontStyles(fontstyle);

    BlockRead(data, i, 4);
    if i > 0 then
    begin
      if not (i < Length(buffer)) then
        raise Exception.Create('corrupt session file');
      BlockRead(data, buffer, i);
      buffer[i] := #0;
      FCharStateFont.Name := StrPas(buffer);
    end;
    BlockRead(data, color, SizeOf(TColor));
    FCharStateFont.Color := color;
    BlockRead(data, i, 4);
    FCharStateFont.Size := i;
    if SessionFileVersion >= 703 then
      BlockRead(data, fontstyle, SizeOf(TFontStyle))
    else
      BlockRead(data, fontstyle, 1); // in Delphi, the size was 1
    FCharStateFont.Style := ValidFontStyles(fontstyle);

    if FileVersion >= 2 then
      BlockRead(data, FForceLinearized, SizeOf(boolean));

    LoadTreeData(TreeIndex);

    SetStats;
    if blen then
    begin
      SetMaxLength(Root);
      FSBL := 0.0;
      for i := 1 to NoOfNodes-1 do
        FSBL := FSBL +Node[i].branch.length;
    end;
    SetClusterSize(Root);
    SetClusterWidth(Root);
    SetClusterHeight;
    SetAttribListToAttributesForActiveTree;
    SetCompressedAttribList;
    SetAttrindex;
    if blen then
      if (IsLinearized or ForceLinearized) then
        xmax := Root.Height
      else
        xmax := Max(Root.des1.branch.maxlen2, Root.des2.branch.maxlen2);

    if (PixelsPerUnit > 0.0) and
       (Ceil(xmax*PixelsPerUnit*2) < TreeWidth) then begin    // To convert from version 100
      FTreeWidth := Ceil(FTreeWidth/4);
      MinTreeWidth := Ceil(MinTreeWidth/4);
    end;

    if FileVersion >= 13 then
    begin
      { divergence time data}
      BlockRead(data, FisTimes, 1);
      BlockRead(data, DivTime, SizeOf(DivTime));
      if DivTime > 0 then // otherwise, the user never applied a calibration
        DoSetDivergenceTime(DivTime);
      BlockRead(data, i, SizeOf(Integer)); // number of calibrated nodes
      if i > 0 then
      begin
        SetLength(FCalibratedNodes, i);
        for k := 0 to i - 1 do
        begin
          BlockRead(data, FCalibratedNodes[k], SizeOf(Integer));
        end;
      end;

      BlockRead(data, b, 1);
      if b then
        BlockRead(data, FTimeFactor, SizeOf(Double));
      BlockRead(data, SessionIsReltime, SizeOf(Boolean));
      { divergence times display formatting}
      BlockRead(data, FShowDivergenceTimes, 1);
      BlockRead(data, FTimesPosition, SizeOf(TBranchInfoPosition));
      BlockRead(data, FTimesMargin, SizeOf(TPoint));
      BlockRead(data, FDivTimeDecimals, 4);
      BlockRead(data, i, 4);
      if i > 0 then begin
        BlockRead(data, buffer, i);
        buffer[i] := #0;
        FTimesFont.Name := StrPas(buffer);
      end;
      BlockRead(data, color, SizeOf(TColor));
      FTimesFont.Color := color;
      BlockRead(data, i, 4);
      FTimesFont.Size := i;
      if SessionFileVersion >= 703 then
        BlockRead(data, fontstyle, SizeOf(TFontStyle))
      else
        BlockRead(data, fontstyle, 1); // in Delphi, the size was 1
      FTimesFont.Style := ValidFontStyles(fontstyle);
      if SessionFileVersion >= 1003 then
        BlockRead(data, FIsSamplingTimes, SizeOf(FIsSamplingTimes));

      { branch length font}
      BlockRead(data, i, 4);
      if i > 0 then begin
        BlockRead(data, buffer, i);
        buffer[i] := #0;
        FBLensFont.Name := StrPas(buffer);
      end;
      BlockRead(data, color, SizeOf(TColor));
      FBLensFont.Color := color;
      BlockRead(data, i, 4);
      FBLensFont.Size := i;
      if SessionFileVersion >= 703 then
        BlockRead(data, fontstyle, SizeOf(TFontStyle))
      else
        BlockRead(data, fontstyle, 1); // in Delphi, the size was 1
      FBLensFont.Style := ValidFontStyles(fontstyle);
      BlockRead(data, FShowHeightErrBar, SizeOf(Boolean));
    end;

    if FileVersion >= 15 then
    begin
      BlockRead(data, b, SizeOf(Boolean));
      IsGeneDups := b;
      BlockRead(data, FShowGeneDupMarkers, SizeOf(Boolean));
      BlockRead(data, FShowSpeciationMarkers, SizeOf(Boolean));
      BlockRead(data, FHasGeneDups, SizeOf(Boolean));
      BlockRead(data, FHasSpeciations, SizeOf(Boolean));
    end;

    if FileVersion >= 15 then
    begin
      for i := 1 to NoOfOtus do
      begin
        if TreeList.HasSpeciesNames then
          Node[i].SpeciesName := TreeList.SpeciesName[i-1];
      end;

      if FTreeIndex > 0 then
      begin
        for i := (NoOfOtus + 1) to NoOfNodes do
        begin
          Node[i].IsGeneDuplication := TreeList[FTreeIndex-1].IsGeneDupEvent[i-1];
          Node[i].IsSpeciationEvent := TreeList[FTreeIndex-1].IsSpeciationEvent[i-1];
          Node[i].DataCoverage := TreeList[FTreeIndex-1].DataCoverage[i - NoOfOtus - 1];
          if Node[i].IsGeneDuplication then
          begin
            if not FHasGeneDups then
              FHasGeneDups := True;
          end
          else if Node[i].IsSpeciationEvent then
          begin
            if not FHasSpeciations then
              FHasSpeciations := True;
          end;
        end;
      end;
    end;

    Result := true;
  except
    on E:Exception do
    begin
      ResetMem;
      raise Exception.Create(E.Message);
    end;
  end;
end;

procedure TTreeBox.WriteToFile(var data: File);
var
  buffer : array[0..4095] of AnsiChar;
  color: TColor;
  penstyle: TPenStyle;
  brushstyle: TBrushStyle;
  fontstyle: TFontStyles;
  n: integer;
  i,j : integer;
  r: double;
  b: boolean;
  tree: TTreeData;
  DivTime: Double;
begin
  if TreeList = nil then Exit;
  SaveTreeData(TreeIndex);
  if Source = 'PartitionList' then
    for i := 1 to TreeList.NoOfTrees do
      if not FDataInitialized[i] then
      begin
        tree := FPartitionList.ExtractTreeData(i-1);
        TreeList[i-1].Assign(tree);
        tree.Free;
        FDataInitialized[i] := true;
      end;

  BlockWrite(data, FNoOfTrees, 4);
  BlockWrite(data, FNoOfOTUs, 4);

  TreeList.WriteToFile(data);
  if isConsensus then begin
    if not FDataInitialized[0] then
    begin
      MakeConsensusTree;
      FDataInitialized[0] := true;
    end;
    i := 1;
    if ConsTree.isBLen then i := i or 2;
    if ConsTree.isSE then i := i or 4;
    BlockWrite(data, i, 4);
    ConsTree.WriteToFile(data);
    BlockWrite(data, ConsTreeIsValue, 1);
    BlockWrite(data, ConsTreeIsValue2, 1);
  end
  else begin
    i := 0;
    BlockWrite(data, i, 4);
  end;

  BlockWrite(data, FTreeIndex, 4);
  BlockWrite(data, Initialized^, NoOfTrees+1);
  BlockWrite(data, FDataInitialized^, NoOfTrees+1);

  BlockWrite(data, FFreqDecimals, 4);
  BlockWrite(data, FValueDecimals, 4);
  BlockWrite(data, FValue2Decimals, 4);
  BlockWrite(data, FBLenDecimals, 4);

  BlockWrite(data, FCondenseValue, 4);
  BlockWrite(data, FConsensusValue, 4);

  BlockWrite(data, FPixelsPerUnit, 8);
  BlockWrite(data, FTreeWidth, 4);
  BlockWrite(data, MinTreeWidth, 4);

  BlockWrite(data, yunit, 4);
  BlockWrite(data, gunit, 4);

  BlockWrite(data, FRadius, 4);
  BlockWrite(data, FStartAngle, 4);
  BlockWrite(data, FCenterMargin, 4);

  BlockWrite(data, FStatsCutoff, 4);
  BlockWrite(data, FStatsPosition, SizeOf(TBranchInfoPosition));
  BlockWrite(data, FStatsMargin, SizeOf(TPoint));

  r := 0.0;
  BlockWrite(data, r, 8);  //  BlockWrite(data, FLongestPath, 8);
  BlockWrite(data, r, 8);  //  BlockWrite(data, FMinBranchLength, 8);
  BlockWrite(data, r, 8);  //  BlockWrite(data, FMaxBranchLength, 8);
  BlockWrite(data, FBLenCutoff, 8);
  BlockWrite(data, FBLenPosition, SizeOf(TBranchInfoPosition));

  BlockWrite(data, Scale, 8);
  BlockWrite(data, FScaleTick, 8);
  BlockWrite(data, TimeScale, 8);
  BlockWrite(data, FTimeTick, 8);
  BlockWrite(data, FTimeFactor, 8);
  BlockWrite(data, FLatestTime, 8);

  i := Length(FScaleText);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FScaleText);
    BlockWrite(data, buffer, i);
  end;
  i := Length(FScaleUnit);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FScaleUnit);
    BlockWrite(data, buffer, i);
  end;
  i := Length(FTimeText);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FTimeText);
    BlockWrite(data, buffer, i);
  end;
  i := Length(FTimeUnit);
  BlockWrite(data, i, 4);
  if i > 0 then begin
    StrPCopy(buffer, FTimeUnit);
    BlockWrite(data, buffer, i);
  end;

  BlockWrite(data, FTreeStyle, SizeOf(TTreeStyle));
  BlockWrite(data, FBranchStyle, SizeOf(TBranchStyle));
  BlockWrite(data, FIsCondensed, SizeOf(boolean));
  BlockWrite(data, FIsLinearized, SizeOf(boolean));
  BlockWrite(data, FTopologyOnly, SizeOf(boolean));
  BlockWrite(data, FHorzTaxonName, SizeOf(boolean));
  BlockWrite(data, FAlignCaption, SizeOf(boolean));
  b := AttribList[0].ShowTaxonName;
  BlockWrite(data, b, 1);
  b := AttribList[0].ShowTaxonMarker;
  BlockWrite(data, b, 1);
  BlockWrite(data, FShowBLen, 1);
  BlockWrite(data, FShowStats, 1);
  BlockWrite(data, FShowScale, 1);
  BlockWrite(data, FShowTimeScale, 1);
  BlockWrite(data, FShowSamplingTimeScale, 1);
  BlockWrite(data, ConsCondenseValue, 4);

  j := 1;
  for i := 0 to NoOfTrees do
    if AttribListArray[i] <> nil then
      Inc(j, TNodeAttribList(AttribListArray[i]).Count);
  BlockWrite(data, j, 4);   // Number of NodeAttrib;

// Default Set
  i := 0; // Depth
  BlockWrite(data, i, 4);
  i := 0; // Bracket left
  BlockWrite(data, i, 4);
  i := 0; // Bracket top
  BlockWrite(data, i, 4);
  i := 0; // Bracket right
  BlockWrite(data, i, 4);
  i := 0; // Bracket bottom
  BlockWrite(data, i, 4);

  i := MyCurrentVersion; // Version
  BlockWrite(data, i, 4); // Version
  BlockWrite(data, j, 4); // Number of NodeAttrib

//  b := false; //IncludeBranch
  BlockWrite(data, FShowRoot, 1);   // From ver 3
  b := false; // ShowCaption
  BlockWrite(data, b, 1);
//LinePen
  color := FBranchPen.Color;
  BlockWrite(data, color, SizeOf(TColor));
  i := FBranchPen.Width;
  BlockWrite(data, i, 4);
  penstyle := FBranchPen.Style;
  BlockWrite(data, penstyle, SizeOf(TPenStyle));
//Bracketpen
  color := FBranchPen.Color;
  BlockWrite(data, color, SizeOf(TColor));
  i := FBranchPen.Width;
  BlockWrite(data, i, 4);
  penstyle := FBranchPen.Style;
  BlockWrite(data, penstyle, SizeOf(TPenStyle));
//Font
  i := Length(FOTU_Font.Name);
  BlockWrite(data, i, 4);
  StrPCopy(buffer, FOTU_Font.Name);
  BlockWrite(data, buffer, i);
  color := FOTU_Font.Color;
  BlockWrite(data, color, SizeOf(TColor));
  i := FOTU_Font.Size;
  BlockWrite(data, i, 4);
  fontstyle := FOTU_Font.Style;
  BlockWrite(data, fontstyle, SizeOf(TFontStyle));
//Brush
  color := FBrush.Color;
  BlockWrite(data, color, SizeOf(TColor));
  brushstyle := FBrush.Style;
  BlockWrite(data, brushstyle, SizeOf(TBrushStyle));

  for i := 0 to NoOfTrees do
  begin
    if AttribListArray[i] <> nil then
      for j := 0 to AttribListArray[i].Count-1 do
      begin
        BlockWrite(data, i, 4);            // TreeIndex
        AttribListArray[i][j].WriteToFile(data);
      end;
  end;

  j := Length(CompressedAttributesArray);
  BlockWrite(data, j, SizeOf(j));
  for i := Low(CompressedAttributesArray) to High(CompressedAttributesArray) do
  begin
    if Assigned(CompressedAttributesArray[i]) then
    begin
      j := CompressedAttributesArray[i].Count;
      BlockWrite(data, j, SizeOf(j));
      if CompressedAttributesArray[i].Count > 0 then
        for j := 0 to CompressedAttributesArray[i].Count - 1 do
          CompressedAttributesArray[i][j].WriteToFile(data);
    end
    else
    begin
      j := 0;
      BlockWrite(data, j, SizeOf(Integer));
    end;
  end;

  j := SIMILAR_COMPRESSED_INDEX + 1;
  BlockWrite(data, j, SizeOf(j));
  j := Length(FAutoCompressedSettings[GROUPS_COMPRESSED_INDEX]);
  BlockWrite(data, j, SizeOf(j));
  for j := GROUPS_COMPRESSED_INDEX to SIMILAR_COMPRESSED_INDEX do
    for i := Low(FAutoCompressedSettings[j]) to High(FAutoCompressedSettings[j]) do
    begin
      b := FAutoCompressedSettings[j][i];
      BlockWrite(data, b, SizeOf(b));
    end;

//  from version 6
  for i := 1 to NoOfOTUs do
  begin
    n := Node[i].groupindex;
     BlockWrite(data, n, 4);
  end;
  n := GroupAttrib.Count;
  BlockWrite(data, n, 4);
  if n > 0 then
    for i := 0 to GroupAttrib.Count-1 do
//      WriteDrawAttrib(GroupAttrib[i]);
      GroupAttrib[i].WriteToFile(data);
//
// from version 8
  BlockWrite(data, FUseSubtreeAttrib, 1);
  BlockWrite(data, FUseGroupAttrib, 1);
//


  color := FScalePen.Color;
  BlockWrite(data, color, SizeOf(TColor));
  i := FScalePen.Width;
  BlockWrite(data, i, 4);
  penstyle := FScalePen.Style;
  BlockWrite(data, penstyle, SizeOf(TPenStyle));

  i := Length(FStatsFont.Name);
  BlockWrite(data, i, 4);
  StrPCopy(buffer, FStatsFont.Name);
  BlockWrite(data, buffer, i);
  color := FStatsFont.Color;
  BlockWrite(data, color, SizeOf(TColor));
  i := FStatsFont.Size;
  BlockWrite(data, i, 4);
  fontstyle := FStatsFont.Style;
  BlockWrite(data, fontstyle, SizeOf(TFontStyle));

  i := Length(FScaleFont.Name);
  BlockWrite(data, i, 4);
  StrPCopy(buffer, FScaleFont.Name);
  BlockWrite(data, buffer, i);
  color := FScaleFont.Color;
  BlockWrite(data, color, SizeOf(TColor));
  i := FScaleFont.Size;
  BlockWrite(data, i, 4);
  fontstyle := FScaleFont.Style;
  BlockWrite(data, fontstyle, SizeOf(TFontStyle));

  i := Length(FCharStateFont.Name);
  BlockWrite(data, i, 4);
  StrPCopy(buffer, FCharStateFont.Name);
  BlockWrite(data, buffer, i);
  color := FCharStateFont.Color;
  BlockWrite(data, color, SizeOf(TColor));
  i := FCharStateFont.Size;
  BlockWrite(data, i, 4);
  fontstyle := FCharStateFont.Style;
  BlockWrite(data, fontstyle, SizeOf(TFontStyle));

  BlockWrite(data, FForceLinearized, SizeOf(boolean));

  { For MEGA6, we have added support for calibration times as well as fonts for branch lengths}

  { active divergence time data}
  BlockWrite(data, FisTimes, 1);
  DivTime := GetDivergenceTime;
  BlockWrite(data, DivTime, SizeOf(DivTime));
  if Length(FCalibratedNodes) > 0 then
  begin
    j := Length(FCalibratedNodes);
    BlockWrite(data, j, SizeOf(Integer));
    for i := 0 to Length(FCalibratedNodes) - 1 do
      BlockWrite(data, FCalibratedNodes[i], SizeOf(Integer));
  end
  else
  begin
    j := 0;
    BlockWrite(data, j, SizeOf(Integer));
  end;

  if FTimeFactor > 0 then
    b := True
  else
    b := False;
  BlockWrite(data, b, 1);

  if b then
    BlockWrite(data, FTimeFactor, SizeOf(Double));
  BlockWrite(data, SessionIsReltime, SizeOf(Boolean));
  { divergence times display formatting}
  BlockWrite(data, FShowDivergenceTimes, 1);
  BlockWrite(data, FTimesPosition, SizeOf(TBranchInfoPosition));
  BlockWrite(data, FTimesMargin, SizeOf(TPoint));
  BlockWrite(data, DivTimeDecimals, 4);
  i := Length(FTimesFont.Name);
  BlockWrite(data, i, 4);
  StrPCopy(buffer, FTimesFont.Name);
  BlockWrite(data, buffer, i);
  color := FTimesFont.Color;
  BlockWrite(data, color, SizeOf(TColor));
  i := FTimesFont.Size;
  BlockWrite(data, i, 4);
  fontstyle := FTimesFont.Style;
  BlockWrite(data, fontstyle, SizeOf(TFontStyle));
  BlockWrite(data, FIsSamplingTimes, SizeOf(FIsSamplingTimes));

  { branch length font}
  i := Length(FBLensFont.Name);
  BlockWrite(data, i, 4);
  StrPCopy(buffer, FBLensFont.Name);
  BlockWrite(data, buffer, i);
  color := FBLensFont.Color;
  BlockWrite(data, color, SizeOf(TColor));
  i := FBLensFont.Size;
  BlockWrite(data, i, 4);
  fontstyle := FBLensFont.Style;
  BlockWrite(data, fontstyle, SizeOf(TFontStyle));
  b := FShowHeightErrBar;
  BlockWrite(data, b, SizeOf(Boolean));

  { gene duplication stuff}
  b := IsGeneDups;
  BlockWrite(data, b, SizeOf(Boolean));
  BlockWrite(data, FShowGeneDupMarkers, SizeOf(Boolean));
  BlockWrite(data, FShowSpeciationMarkers, SizeOf(Boolean));
  BlockWrite(data, FHasGeneDups, SizeOf(Boolean));
  BlockWrite(data, FHasSpeciations, SizeOf(Boolean));
end;

function  TTreeBox.ImportFromNewickStandard(filename : AnsiString; Unroot: Boolean=False):boolean;
var
  trees: TTreeList;
begin
  trees := TTreeList.Create;
  try
    result := trees.ImportFromNewickFile(filename, nil);
    if Unroot and trees.isRooted then
      trees.isRooted := False;

    if result then
    begin
      SetTreeList(trees, false);
      trees := nil;
    end;

    FSource := 'File: '+filename;
  finally
    if trees <> nil then
      trees.Free;
  end;
end;

function  TTreeBox.ImportFromNewickString(NewickTree : AnsiString):boolean;
var
  trees: TTreeList;
begin
  trees := TTreeList.Create;
  try
    result := trees.ImportFromNewick(NewickTree, nil);
    if result then
    begin
      SetTreeList(trees, false);
      trees := nil;
    end;

    FSource := 'Tree: '+NewickTree;
  finally
    if trees <> nil then
      trees.Free;
  end;
end;

function TTreeBox.ImportFromNewickString(NewickTree: AnsiString; NamesList: TStringList): boolean;
var
  trees: TTreeList;
begin
  trees := TTreeList.Create;
  try
    result := trees.ImportFromNewick(NewickTree, NamesList);
    if result then
    begin
      SetTreeList(trees, false);
      trees := nil;
    end;
    FSource := 'Tree: '+NewickTree;
  finally
    if trees <> nil then
      trees.Free;
  end;
end;

function TTreeBox.ImportFromMEGA:boolean;
var data: TextFile;
    buffer: AnsiString;
    tree: TTreeData;
    i,j,k: integer;
    b: boolean;
begin
    Result := false;
    if not FileExists('C:\MEGA\TEMPXXYY.MGA\TEMPTREE.MGA') then Exit;

    AssignFile(data,'C:\MEGA\TEMPXXYY.MGA\TEMPTREE.MGA');
    Reset(data);
    FSource := 'MEGA';

    Readln(data, buffer);
    case buffer[1] of
      'U': FMethod := 'UPGMA';
      'N': FMethod := 'Neighbor Joining';
      'P': FMethod := 'Parsimony';
    end;
    if Method = 'UPGMA' then
        FisRooted := true
    else
        FisRooted := false;
    if buffer[2] = 'B' then
        b := true
    else
        b := false;
    if (buffer[3] = 'B') or (buffer[3] = 'E') then begin
        FisStats := true;
        FMaxStats := 100;
        if buffer[3] = 'B' then begin
            FTestMethod := 'Bootstrap Test';
            FStatsName := '%BP Value';
        end
        else if buffer[3] = 'E' then begin
            FTestMethod := 'Standard Error Test';
            FStatsName := '%CP Value';
        end;
    end
    else
        FisStats := false;
    FTitle := '';
    FDescription := '';
    FisSE := false;
    FisBranchFixed := false;
    FisValue := false;
    FNoOfTrees := 1;

    Readln(data, buffer);
    buffer := Trim(buffer);
    FNoOfOTUs := StrToInt(buffer);
    TreeList := TTreeList.Create;
    for i := 1 to NoOfTrees do begin
        tree := TTreeData.Create(NoOfOTUs, b, FisSE, true);
        TreeList.Add(tree);
    end;

    InitMem;

    try
      for i := 1 to NoOfOTUs do begin
          Read(data, j);
          Readln(data, Node[i].name);
          Node[i].name := Trim(Node[i].name);
          Node[i].oriname := Node[i].name;
          Node[i].PrivateName := ''; // [dp Sept 23 08] was Node[i].PrivateName := Node[i].name;
          TreeList.OTUName[i-1] := Node[i].name;
      end;

      for i := 0 to NoOfOTUs-2 do begin
          Read(data, j);
          Read(data, j);
          Read(data, k);
          tree.NodeArray[i].des1 := j-1;
          tree.NodeArray[i].des2 := k-1;
          if b then
          begin
              Read(data, tree.BLenArray[j-1]);
              Read(data, tree.BLenArray[k-1]);
          end;
          if FisStats then
          begin
              Read(data, tree.StatsArray[j-1]);
              Read(data, tree.StatsArray[k-1]);
          end;
          if not Eoln(data) then Readln(data, buffer);
      end;
      TreeList.StatsName := FStatsName;
      TreeList.MaxStats := FMaxStats;

      InitTree;
      Result := true;
    finally
      if not Result then ResetMem;
      CloseFile(data);
    end;
end;

function TTreeBox.GetInformation:TStringList;
begin
    Result := TreeList.Information;
end;

procedure TTreeBox.ExportTree(var f: TextFile; treeindex: integer);
var tree: TTreeData;

  procedure ExportNode(nodeindex: integer);
  begin
    if tree[nodeindex].des1 < NoOfOTUs then
      Write(f, IntToStr(tree[nodeindex].des1+1))
    else begin
      Write(f, '(');
      ExportNode(tree[nodeindex].des1-NoOfOTUs);
      Write(f, ')');
    end;
    Write(f, ',');
    if tree[nodeindex].des2 < NoOfOTUs then
      Write(f, IntToStr(tree[nodeindex].des2+1))
    else begin
      Write(f, '(');
      ExportNode(tree[nodeindex].des2-NoOfOTUs);
      Write(f, ')');
    end;
  end;

var root, i, j: integer;
    flag: boolean;
begin
  root := -1;
  if treeindex = 0 then
    tree := ConsTree
  else
    tree := TreeList[treeindex-1];
  for i := NoOfNodes-1 downto NoOfOTUs do begin
    flag := true;
    for j := NoOfOTUs-2 downto 0 do begin
      if tree[j].des1 = i then begin
        flag := false;
        Break;
      end;
      if tree[j].des2 = i then begin
        flag := false;
        Break;
      end;
    end;
    if flag then begin
      root := i-NoOfOTUs;
      Break;
    end;
  end;
  Write(f, '(');
  if isRooted then
    ExportNode(root)
  else if tree[root].des1 < NoOfOTUs then begin
    Write(f, IntToStr(tree[root].des1+1)+',');
    ExportNode(tree[root].des2-NoOfOTUs);
  end
  else if tree[root].des2 < NoOfOTUs then begin
    ExportNode(tree[root].des1-NoOfOTUs);
    Write(f, ','+IntToStr(tree[root].des2+1));
  end
  else begin
    ExportNode(tree[root].des1-NoOfOTUs);
    Write(f, ',(');
    ExportNode(tree[root].des2-NoOfOTUs);
    Write(f, ')');
  end;
  Writeln(f, ');');
end;

function TTreeBox.FinalizeCalibration(var ACalibration: TCalibrationTime): Boolean;
var
  TaxonA: Integer;
begin
  Result := False;
  if not ACalibration.IsSamplingTime then // then we have an internal node
  begin
    Result := TreeList.FinalizeCalibration(ACalibration);
  end
  else
  begin
    if ACalibration.NodeName = EmptyStr then
      ACalibration.NodeName := Node[ACalibration.TreeViewFormNodeId].name;
    TaxonA := FindNodeId(ACalibration.NodeName);
    if TaxonA < 0 then
    begin
      ACalibration.IsValid := False;
      ACalibration.IsSelected := False;
    end
    else
      ACalibration.SetNodeIdFromTreeViewFormId(TaxonA);
    ACalibration.NodeA := ACalibration.NodeName;
    ACalibration.NodeB := ACalibration.NodeName;
  end;
  Result := True;
end;

procedure TTreeBox.ExportCurrentTreeToFile(filename : AnsiString);
var f : TextFile;
    i : integer;
begin
  AssignFile(f, filename);
  ReWrite(f);
  Writeln(f, '#mega');
  Writeln(f, '!Title: '+Information.Values['Title']+';');
  Writeln(f, '!Format: datatype=tree ntaxa='+IntToStr(NoOfOTUs)+';');
  Writeln(f);
  for i := 1 to NoOfOTUs do
    Writeln(f, '['+IntToStr(i)+'] #'+OtuNameToMegaStr(OTUName[i]));
  Writeln(f);

  if treeindex = 0 then
    Write(f, '#MEGAConsensusTree=')
  else
    Write(f, '#MEGATree=');
  ExportTree(f, TreeIndex);

  CloseFile(f);
end;

procedure TTreeBox.ExportAllTreesToFile(filename : AnsiString);
var f : TextFile;
    i : integer;
begin
  AssignFile(f, filename);
  ReWrite(f);
  Writeln(f, '#mega');
  Writeln(f, '!Title: '+Information.Values['Title']+';');
  Writeln(f, '!Format: datatype=tree ntaxa='+IntToStr(NoOfOTUs)+';');
  Writeln(f);
  for i := 1 to NoOfOTUs do
    Writeln(f, '['+IntToStr(i)+'] #'+OtuNameToMegaStr(OTUName[i]));
  Writeln(f);

  for i := 1 to NoOfTrees do begin
    if treeindex = 0 then
      Write(f, '#MEGAConsensusTree=')
    else
      Write(f, '#MEGATree'+IntToStr(i)+'=');
    ExportTree(f, i);
  end;

  CloseFile(f);
end;

function TTreeBox.GetNewickTree(Options: TNewickExportOptions): String;
begin
  SaveTreeData(TreeIndex);
  if TreeIndex = 0 then
    Result := GetBootstrapNewickString(Options)
  else if isCondensed then
    Result := TreeList.OutputNewickTree(TreeIndex-1, Options, CondenseValue/100*MaxStats)
  else
    Result := TreeList.OutputNewickTree(TreeIndex-1, Options, 0);
end;

function TTreeBox.GetSubtreeNewick(useQuotesForLabels: Boolean): String;
var
  aList: TTreeList = nil;
  options: TNewickExportOptions;
begin
  if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
    Result := GetNewickTree(useQuotesForLabels)
  else
  begin
    try
      aList := GetSubtreeTreeList;
      options.BootstrapVals := aList.isStats;
      options.BranchLengths := aList.isBLen;
      options.DivergenceTimes := (FTreeBoxType = tttReltimeTree) and (CompareValue(FTimeFactor, 1, FP_CUTOFF) > 0);
      options.Reltimes := (TreeBoxType = tttReltimeTree) and (not options.DivergenceTimes);
      options.GeneDuplications := HasGeneDups;
      options.SpeciationEvents := HasSpeciations;
      options.NodeLabels := alist[0].HasInternalNodeLbls;
      options.UseQuotesForLabels := useQuotesForLabels;
      Result := aList.OutputNewickTree(0, options, 0.0);
    finally
      if Assigned(aList) then
        aList.Free;
    end;
  end;
end;

function TTreeBox.GetBootstrapNewickString(Options: TNewickExportOptions): String;
var
  aTreeList: TTreeList;
  aTreeData: TTreeData;
  i: Integer;
begin
  try
    aTreeList := TTreeList.Create;
    aTreeList.MaxStats := MaxStats;
    for i := 0 to NoOfOTUs-1 do
      aTreeList.OTUName[i] := OTUName[i+1];
    aTreedata := TTreeData.Create(NoOfOTUs, False, False, ShowStats);
    aTreedata.Assign(ConsTree);
    aTreeList.Add(aTreedata);
    if IsCondensed then
      Result := aTreeList.OutputNewickTree(0, Options, ConsensusValue/100*MaxStats)
    else
      Result := aTreeList.OutputNewickTree(0, Options, 0);
  finally
    if Assigned(aTreeList) then
    begin
      aTreeList.Delete(0);
      aTreeList.Free;
    end;
  end;
end;

function TTreeBox.GetAllNewickTrees(Options: TNewickExportOptions): TStringList;
begin
  SaveTreeData(TreeIndex);
  if isCondensed then
    Result := TreeList.OutputAllNewickTrees(Options, CondenseValue/100*MaxStats)
  else
    Result := TreeList.OutputAllNewickTrees(Options, 0);
end;

function TTreeBox.GetOTUNamesList: TStringlist;
begin
  Result := TStringList.Create;
  Result.AddStrings(TreeList.OTUNameList);
end;

procedure TTreeBox.GetOTUNames(var aList: TStringList);
begin
  aList.AddStrings(TreeList.OTUNameList);
end;

function TTreeBox.GetOutgroupInfo: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 1 to NoOfOTUs do
  begin
    if Node[i].groupindex >= 0 then
    begin
      Result.Add(Node[i].name + '=' + GroupAttrib[Node[i].groupindex].Name);
    end;
  end;
end;

procedure TTreeBox.GetOutgroupTaxa(var AList: TStringList);
var
  i: Integer;
begin
  for i := 1 to NoOfOtus do
    if Node[i].outgroup then
      AList.Add(Node[i].name);
end;

procedure TTreeBox.ExportCurrentTreeToNewickFile(filename : AnsiString);
var
  tempTreeList: TTreeList;
  treedata: TTreeData;
  i: integer;
begin
  SaveTreeData(TreeIndex);

  if TreeIndex = 0 then
  begin
    tempTreeList := TTreeList.Create;
    tempTreeList.MaxStats := MaxStats;
    for i := 0 to NoOfOTUs-1 do
      tempTreeList.OTUName[i] := OTUName[i+1];
    treedata := TTreeData.Create(NoOfOTUs, (not ShowTopologyOnly), false, ShowStats);
    treedata.Assign(ConsTree);
    tempTreeList.Add(treedata);
    if IsCondensed then
      tempTreeList.ExportATreeToNewickFile(0, filename, (not ShowTopologyOnly), ShowStats, ConsensusValue/100*MaxStats)
    else
      tempTreeList.ExportATreeToNewickFile(0, filename, (not ShowTopologyOnly), ShowStats, 0);
    tempTreeList.Delete(0);
    tempTreeList.Free;
  end
  else if ShowTopologyOnly and isCondensed then
    TreeList.ExportATreeToNewickFile(TreeIndex-1, filename, (not ShowTopologyOnly), ShowStats, CondenseValue/100*MaxStats)
  else
    TreeList.ExportATreeToNewickFile(TreeIndex-1, filename, (not ShowTopologyOnly), ShowStats, 0);
end;

function TTreeBox.GetCurrentTree: TTreeData;
var
  treedata: TTreeData;
begin
  SaveTreeData(TreeIndex);

  if TreeIndex = 0 then
  begin
    treedata := TTreeData.Create(NoOfOTUs, (not ShowTopologyOnly), false, ShowStats);
    treedata.Assign(ConsTree);
    result := treedata;
  end
  else
    result := TreeList[TreeIndex-1];
end;

function TTreeBox.GetRootedNewickTreeNoBCL: String;
var
  options: TNewickExportOptions;
  aList: TTreeList = nil;
begin
  try
    SaveTreeData(TreeIndex);
    options.BootstrapVals := False; // NoBCL means no Bootstrap Confidence Limits
    options.BranchLengths := TreeList.isBLen;
    options.DivergenceTimes := (FTreeBoxType = tttReltimeTree) and (CompareValue(FTimeFactor, 1, FP_CUTOFF) > 0);
    options.Reltimes := (TreeBoxType = tttReltimeTree) and (not options.DivergenceTimes);
    options.GeneDuplications := HasGeneDups;
    options.SpeciationEvents := HasSpeciations;
    options.NodeLabels := TreeList[TreeIndex - 1].HasInternalNodeLbls;
    options.UseQuotesForLabels := True;
    aList := TTreeList.Create;
    aList.Assign(TreeList);
    aList.isRooted := True;
    if ShowTopologyOnly and isCondensed then
      result := aList.OutputNewickTree(TreeIndex-1, options, CondenseValue/100*MaxStats)
    else
      result := aList.OutputNewickTree(TreeIndex-1, options, 0);
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TTreeBox.GetNewickTree(useQuotesForLabels: Boolean): String;
var
  options: TNewickExportOptions;
begin
  SaveTreeData(TreeIndex);
  options.BootstrapVals := TreeList.isStats;
  options.BranchLengths := TreeList.isBLen;
  options.DivergenceTimes := (FTreeBoxType = tttReltimeTree) and (CompareValue(FTimeFactor, 1, FP_CUTOFF) > 0);
  options.Reltimes := (TreeBoxType = tttReltimeTree) and (not options.DivergenceTimes);
  options.GeneDuplications := HasGeneDups;
  options.SpeciationEvents := HasSpeciations;
  options.NodeLabels := TreeList[TreeIndex - 1].HasInternalNodeLbls;
  options.UseQuotesForLabels := useQuotesForLabels;
  if ShowTopologyOnly and isCondensed then
    result := TreeList.OutputNewickTree(TreeIndex-1, options, CondenseValue/100*MaxStats)
  else
    result := TreeList.OutputNewickTree(TreeIndex-1, options, 0);
end;

procedure TTreeBox.ExportAllTreesToNewickFile(filename : AnsiString);
begin
  SaveTreeData(TreeIndex);
  if ShowTopologyOnly and isCondensed then
    TreeList.ExportToNewickFile(filename, (not ShowTopologyOnly), ShowStats, CondenseValue/100*MaxStats)
  else
    TreeList.ExportToNewickFile(filename, (not ShowTopologyOnly), ShowStats, 0);
end;

procedure TTreeBox.SetTaxonMarkerOfSubtree(nodeindex: integer; marker: TNodeMarker);
var
  flag: boolean;

  procedure SetMarkerOnTaxon(p: TpNode);
  begin
    if (p <> Root) and (p.attrindex > 0) then
    begin
      AttribList[p.attrindex].ShowTaxonMarker := flag;
      AttribListArray[TreeIndex][p.attrindex-1].ShowTaxonMarker := flag;
    end;
    if p.OTU then
      p.marker := marker
    else
    begin
      SetMarkerOnTaxon(p.des1);
      SetMarkerOnTaxon(p.des2);
    end;
  end;

begin
  if (nodeindex <= 0) or (nodeindex > NoOfNodes) then
    exit;

  flag := marker.Shape <> msNone;
  if Node[nodeindex].attrindex >= 0 then
    flag := AttribList[Node[nodeindex].attrindex].ShowTaxonMarker;

  SetMarkerOnTaxon(Node[nodeindex]);
end;

{ Populates the provided TNodeAttrib for the given node
  and returns true when the given node is NOT using group attributes and the node uses different attributes than its ancestor}
function TTreeBox.GetSubtreeAttrib(var NodeAttrib: TNodeAttrib; const nodeindex: integer): boolean;
var
  attributeIndex: integer = -1;
begin
  Assert(nodeIndex >= 0, Format('bad node index = %d when getting attributes', [nodeIndex]));
  Assert(nodeIndex <= NoOfNodes, Format('bad node index = %d when getting attributes', [nodeIndex]));
  if nodeindex <= 0 then
    attributeIndex := BASE_NODE_ATTRUBUTE_INDEX
  else if nodeindex > NoOfNodes then
    attributeIndex := BASE_NODE_ATTRUBUTE_INDEX
  else if node[nodeindex] = Root then
    attributeIndex := BASE_NODE_ATTRUBUTE_INDEX
  else
    attributeIndex := Node[nodeindex].attrindex;

  if attributeIndex < 0 then
    NodeAttrib.Assign(GroupAttrib[-(attributeIndex+1)])
  else
    NodeAttrib.Assign(AttribList[attributeIndex]);
  if Node[nodeindex] = Root then
    result := true
  else
    result := (Node[nodeindex].attrindex >= 0) and (Node[nodeindex].attrindex <> Node[nodeindex].anc.attrindex);
end;

function TTreeBox.NodeIsAttributeHolder(nodeIndex: Integer): Boolean;
begin
  Assert(nodeIndex >= 0, Format('bad node index = %d when getting attributes', [nodeIndex]));
  Assert(nodeIndex <= NoOfNodes, Format('bad node index = %d when getting attributes', [nodeIndex]));
 if (Node[nodeindex].attrindex < 0) or (Node[nodeindex].attrindex = Node[nodeindex].anc.attrindex) then
   Result := False
 else
   Result := True;
end;

procedure TTreeBox.SetSubtreeAttrib(NodeAttrib: TNodeAttrib; nodeindex: integer);

  procedure PropogateNamesAndMarkerSettings(p: TpNode);
  begin
    Assert(Assigned(p), 'setting attributes on nil node');
    Assert(Assigned(NodeAttrib), 'accessing nil TNodeAttrib');
    Assert(p.attrindex < AttribList.Count, Format('attribute list index out of bounds. Got %d but list count is %d.', [p.attrindex, AttribList.Count]));

    if (p.attrindex > 0) and (p.attrindex <> p.anc.attrindex) then { then p is the attribute holder}
    begin
      AttribList[p.attrindex].ShowTaxonName   := NodeAttrib.ShowTaxonName;
      AttribList[p.attrindex].ShowSpeciesName := NodeAttrib.ShowSpeciesName;
      AttribList[p.attrindex].ShowTaxonMarker := NodeAttrib.ShowTaxonMarker;
      AttribListArray[TreeIndex][p.attrindex-1].ShowTaxonName   := NodeAttrib.ShowTaxonName;
      AttribListArray[TreeIndex][p.attrindex-1].ShowSpeciesName   := NodeAttrib.ShowSpeciesName;
      AttribListArray[TreeIndex][p.attrindex-1].ShowTaxonMarker := NodeAttrib.ShowTaxonMarker;
      AttribListArray[TreeIndex][p.attrindex-1].OverwriteDownstream := NodeAttrib.OverwriteDownstream;
    end;

    if p.OTU then exit;

    PropogateNamesAndMarkerSettings(p.des1);
    PropogateNamesAndMarkerSettings(p.des2);
  end;

var
  attributeIndex: integer;
begin
  if (nodeindex = BASE_NODE_ATTRUBUTE_INDEX) or (Node[nodeindex] = Root) then
  begin
    AttribList[BASE_NODE_ATTRUBUTE_INDEX].Assign(NodeAttrib);
    Exit;
  end;

  if not NodeIsAttributeHolder(nodeIndex) then
  begin
    if AttribListArray[TreeIndex] = nil then
      AttribListArray[TreeIndex] := TNodeAttribList.Create;
    AttribListArray[TreeIndex].Add(TNodeAttrib.Create);
    attributeIndex := AttribListArray[TreeIndex].Count - 1;
  end
  else
    attributeIndex := Node[nodeindex].attrindex - 1; { why is this minus 1?}
  NodeAttrib.NodeIndex := nodeindex;
  AttribListArray[TreeIndex][attributeIndex].Assign(NodeAttrib);

  if not Node[nodeindex].OTU then
  begin
    PropogateNamesAndMarkerSettings(Node[nodeindex].des1);
    PropogateNamesAndMarkerSettings(Node[nodeindex].des2);
  end;

  SetAttribListToAttributesForActiveTree;
  SetAttrindex;
end;

procedure TTreeBox.ClearSubtreeAttrib(nodeindex: integer; recursive: boolean);

  procedure ClearDescendant(node: TpNode);
  begin
    if not node.OTU then
    begin
      ClearDescendant(node.des1);
      ClearDescendant(node.des2);
    end;
    if (node.attrindex > 0) and (node.attrindex <> node.anc.attrindex) then
    begin
      DeleteAttrib(node.attrindex);
      SetAttrindex;
    end;
  end;

begin
  if (nodeindex < 1) or (nodeindex > NoOfNodes) then exit;
  if recursive and (not Node[nodeindex].OTU) then
  begin
    ClearDescendant(Node[nodeindex].des1);
    ClearDescendant(Node[nodeindex].des2);
  end;

  if Node[nodeindex] = Root then exit;

  if (Node[nodeindex].attrindex > 0) and (Node[nodeindex].attrindex <> Node[nodeindex].anc.attrindex) then
  begin
    DeleteAttrib(Node[nodeindex].attrindex);
    Node[nodeindex].compressed := false;
    Node[nodeindex].autoCompressed := False;
    SetAttrindex;
  end;
end;

procedure TTreeBox.OverwriteAttribDownstream(nodeindex: integer);
var
  a: TNodeAttrib;

  procedure Overwrite(node: TpNode);
  begin
    if not node.OTU then
    begin
      Overwrite(node.des1);
      Overwrite(node.des2);
    end;
    if (node.attrindex > 0) and (node.attrindex <> node.anc.attrindex) then
      AttribListArray[TreeIndex][node.attrindex-1].AssignGraphAttrib(a);
  end;

begin
  if (nodeindex < 1) or (nodeindex > NoOfNodes) then exit;
  if Node[nodeindex].OTU then exit;

  a := TNodeAttrib.Create;
  a.AssignGraphAttrib(AttribList[Node[nodeindex].attrindex]);

  Overwrite(Node[nodeindex].des1);
  Overwrite(Node[nodeindex].des2);

  a.Free;

  SetAttribListToAttributesForActiveTree;
  SetAttrindex;
end;

procedure TTreeBox.ClearAllSubtreeAttrib;
begin
  ClearSubtreeAttrib(Root.index, true);
end;

procedure TTreeBox.ClearOutgroups;
var
  i: Integer;
begin
  for i := 1 to NoOfOtus do
  begin
    Node[i].outgroup := False;
    Node[i].emphasized := False;
  end;
end;

procedure TTreeBox.ClearTemporaryAttributes;
var
  a: TNodeAttrib = nil;
  i: Integer;

  procedure ClearSubtreeAttribIndex(p: TpNode; removedIndex: Integer);
  begin
    if p.attrindex = removedIndex then
    begin
      p.attrindex := 0;
      if Assigned(p.des1) then
        ClearSubtreeAttribIndex(p.des1, removedIndex);

      if Assigned(p.des2) then
        ClearSubtreeAttribIndex(p.des2, removedIndex);
    end;
  end;

begin
  if AttribList.Count > 1 then
    for i := AttribList.Count - 1 downto 1 do
    begin
      a := AttribList.GetItems(i);
      if a.IsTemporaryAttribute then
      begin
        if a.NodeIndex >= 1 then
          ClearSubtreeAttribIndex(Node[a.NodeIndex], i);
        DeleteAttrib(i);
      end;
    end;
end;

procedure TTreeBox.HighlightIngroupRootBranch(aColor: TColor);
var
  rootAttrib: TNodeAttrib = nil;
  ingroupAttrib: TNodeAttrib = nil;

  function IngroupRootBlenIsZero: Boolean;
  begin
    Result := (CompareValue(Root.des1.branch.length, 0, FP_CUTOFF) = 0);
  end;

begin
  try
    if Root <> nil then
    begin
      rootAttrib := TNodeAttrib.Create;
      ingroupAttrib := TNodeAttrib.Create;
      GetSubtreeAttrib(rootAttrib, Root.index);
      GetSubtreeAttrib(ingroupAttrib, Root.des1.index);
      rootAttrib.IsTemporaryAttribute := True;
      rootAttrib.LineColor := aColor;
      ingroupAttrib.BranchOption := boNoBranch;
      ingroupAttrib.BracketStyle := brsNone;
      ingroupAttrib.LineColor := clBlack;
      rootAttrib.OverwriteDownstream := False;
      rootAttrib.NodeIndex := Root.index;
      ClearSubtreeAttrib(Root.index, False);
      SetSubtreeAttrib(rootAttrib, Root.index);
      ClearSubtreeAttrib(Root.des1.index, False);
      SetSubtreeAttrib(ingroupAttrib, Root.des1.index);
      if IngroupRootBlenIsZero then
      begin
        ingroupAttrib.BranchOption := boFullBranch;
        if Assigned (Root.des1.des1) then
          SetSubTreeAttrib(ingroupAttrib, Root.des1.des1.index);
        if Assigned (Root.des1.des2) then
          SetSubTreeAttrib(ingroupAttrib, Root.des1.des2.index);
      end;
      AttribList[Node[Root.des1.index].attrindex].IsTemporaryAttribute := True;
    end;
  finally
    if Assigned(rootAttrib) then
      rootAttrib.Free;
    if Assigned(ingroupAttrib) then
      ingroupAttrib.Free;
  end;
end;

procedure TTreeBox.SetGroupInfo(groupinfo: TStringList);
var
  i,j: Integer;
  index: Integer;
  taxonName, groupName, oriName: String;
  gAttrib: TNodeAttrib = nil;
begin
  if (groupinfo = nil) or (groupinfo.Count = 0) then
    exit;
  GroupAttrib.DeleteAll;

  for i := 1 to NoOfOTUs do
    Node[i].groupindex := -1;

  for i := 0 to groupinfo.Count-1 do
  begin
    taxonName := groupInfo.Names[i];
    groupName := groupInfo.ValueFromIndex[i];
    if groupName = EmptyStr then
      continue;
    for j := 1 to NoOfOTUs do
    begin
      oriName := Node[j].oriName;
      if taxonName = oriName then
      begin
        if SameText('outgroup', groupName) then
          Node[j].outgroup := True;
        index := GroupAttrib.IndexOfName(groupName);
        if index >= 0 then
          Node[j].groupindex := index
        else
        begin
          gAttrib := TNodeAttrib.Create;
          gAttrib.Assign(AttribList[0]);
          gAttrib.Name := groupName;
          gAttrib.Caption := groupName;
          gAttrib.ShowBracket := True;
          gAttrib.ShowCaption := True;
          GroupAttrib.Add(gAttrib);
          UseGroupAttrib := True;
          Node[j].groupindex := GroupAttrib.Count - 1;
        end;
        break;
      end;
    end;
  end;
  SetAttrindex;
end;

function TTreeBox.GetGroupInfo: TStringList;
var
  i: Integer = -1;
  n: TpNode = nil;
begin
  Result := TStringList.Create;
  if NoOfOTUs > 0 then
    for i := 1 to NoOfOTUs do
    begin
      n := Node[i];
      if n.groupindex >= 0 then
        Result.Add(Format('%s=%s', [n.name, GroupAttrib[n.groupindex].Name]));
    end;
end;

function TTreeBox.GetGroupNames: TStringList;
var
  i: Integer = -1;
begin
  Result := TStringList.Create;
  if GroupAttrib.Count > 0 then
    for i := 0 to GroupAttrib.Count - 1 do
      Result.Add(GroupAttrib[i].Name);
end;

procedure TTreeBox.GetGroupAttrib(Attrib: TNodeAttrib; GroupName: AnsiString);
var
  i: integer;
begin
  if GroupAttrib.Count = 0 then exit;
  for i := 0 to GroupAttrib.Count-1 do
    if GroupAttrib[i].Name = GroupName then
    begin
      Attrib.Assign(GroupAttrib[i]);
      break;
    end;
end;

procedure TTreeBox.SetGeneDuplications(AGeneTree: TSimpleTreeNodeArray);
var
  i: Integer;
  ARoot: Integer;
begin
  for i := 1 to NoOfNodes do
  begin
    if Node[i].OTU then
      continue;
    Node[i].IsGeneDuplication := AGeneTree[i - 1].IsDuplicationEvent;
    Node[i].IsSpeciationEvent := AGeneTree[i - 1].IsSpeciationEvent;
    TreeList[TreeIndex - 1].IsGeneDupEvent[AGeneTree[i - 1].NodeIndex] := AGeneTree[i - 1].IsDuplicationEvent;  { also need to update the TTreeData so that session files will retain this info}
    TreeList[TreeIndex - 1].IsSpeciationEvent[AGeneTree[i - 1].NodeIndex] := AGeneTree[i - 1].IsSpeciationEvent;
    if (AGeneTree[i - 1].IsRoot) or (AGeneTree[i - 1].Ancestor = nil) then
      ARoot := i;
  end;
  FocusOnNode(ARoot);
  MakeRootOnBranch;
  FIsRooted := True;
  SortClusterForShape;
end;

procedure TTreeBox.SetGroupAttrib(Attrib: TNodeAttrib; GroupName: AnsiString);
var
  i: integer;
begin
  if GroupAttrib.Count = 0 then exit;
  for i := 0 to GroupAttrib.Count-1 do
    if GroupAttrib[i].Name = GroupName then
    begin
      GroupAttrib[i].Assign(Attrib);
      GroupAttrib[i].Name := GroupName;
      break;
    end;
  SetAttrindex;
end;

procedure TTreeBox.SetTaxonMarkerOfGroup(GroupName: AnsiString; marker: TNodeMarker);
var
  i,n: integer;
begin
  if GroupAttrib.Count = 0 then exit;
  n := -1;
  for i := 0 to GroupAttrib.Count-1 do
    if GroupAttrib[i].Name = GroupName then
    begin
      n := i;
      break;
    end;
  if n >= 0 then
    for i := 1 to NoOfOTUs do
      if Node[i].groupindex = n then
        Node[i].marker := marker;
end;

procedure TTreeBox.SetAttribListToAttributesForActiveTree;
var
  i: integer = -1;
begin
  while AttribList.Count > 1 do
    AttribList.Delete(AttribList.Count - 1);
  if Assigned(AttribListArray[TreeIndex]) then
    for i := 0 to AttribListArray[TreeIndex].Count - 1 do
      AttribList.Add(AttribListArray[TreeIndex][i]);
end;

procedure TTreeBox.SetCompressedAttribList;
begin
  CompressedNodeAttributes := CompressedAttributesArray[TreeIndex];
end;

procedure TTreeBox.SetShowOTUMarker(b : boolean);
var
  i: integer;
begin
  inherited;

  if Assigned(AttribListArray) and Assigned(AttribListArray[TreeIndex]) then
    for i := 0 to AttribListArray[TreeIndex].Count-1 do
      AttribListArray[TreeIndex][i].ShowTaxonMarker := b;
end;

procedure TTreeBox.DeleteAttrib(index: integer);
begin
  if (index = 0) or (index >= AttribList.Count) then Exit;
  AttribList[index].Free;
  AttribList.delete(index);
  AttribListArray[TreeIndex].delete(index-1);
  if AttribListArray[TreeIndex].Count = 0 then
  begin
    AttribListArray[TreeIndex].Free;
    AttribListArray[TreeIndex] := nil;
  end;
end;

procedure TTreeBox.DeleteCompressedAttrib(index: Integer);
begin
  if (index = 0) or (not Assigned(CompressedNodeAttributes)) or (index >= CompressedNodeAttributes.Count) then Exit;
  CompressedNodeAttributes[index].Free;
  CompressedNodeAttributes.delete(index);
end;

procedure TTreeBox.MoveRoot(p: TpNode; midpoint: boolean);
var q: TpNode;
    i: integer;
begin
  if AttribList.Count > 1 then
    for i := AttribList.Count-1 downto 1 do begin
      q := p.anc;
      while q <> Root do begin
        if (q.attrindex = i) and (q.attrindex <> q.anc.attrindex) then begin
          DeleteAttrib(q.attrindex);
          q.name := '';
          Break;
        end;
        q := q.anc;
      end;
    end;
  SetAttrindex;
  inherited;
end;



constructor TNodeInfo.Create(Source: TTreeCustomControl);
begin
  inherited Create;
  Tree := Source;
end;

destructor TNodeInfo.Destroy;
begin
  inherited;
end;

function TNodeInfo.GetIndex: integer;
begin
  Result := Tree.FocusedIndex;
end;

function TNodeInfo.GetDepth: Integer;
begin
  Result := Tree.Node[Tree.FocusedIndex].Depth;
end;

function TNodeInfo.GetIsGeneDup: Boolean;
begin
  Result := Tree.Node[Tree.FocusedIndex].IsGeneDuplication;
end;

function TNodeInfo.GetIsOtu: Boolean;
begin
  Result := Tree.Node[Tree.FocusedIndex].OTU;
end;

function TNodeInfo.GetIsOutgroup: Boolean;
begin
  Result := Tree.Node[Tree.FocusedIndex].outgroup;
end;

function TNodeInfo.GetIsSpeciation: Boolean;
begin
  Result := Tree.Node[Tree.FocusedIndex].IsSpeciationEvent;
end;

function TNodeInfo.GetAncIndex: integer;
begin
  with Tree do
    if FocusedIndex = Root.index then
      Result  := 0
    else
      Result  := Node[FocusedIndex].anc.index;
end;

function TNodeInfo.GetDataCoverage: Double;
begin
  with Tree do
    if FocusedIndex = 0 then
      Result := 0.0
    else
      Result := Node[FocusedIndex].dataCoverage;
end;

procedure TNodeInfo.SetName(AValue: AnsiString);
begin
  if Tree.FocusedIndex = 0 then Exit;
  Tree.Node[Tree.FocusedIndex].name := AValue;
  Tree.TreeList[0].InternalNodeLabel[Tree.FocusedIndex - Tree.NoOfOTUs - 1] := AValue;
end;

function TNodeInfo.GetDes1Index: integer;
begin
  with Tree do
    if Node[FocusedIndex].OTU then
      Result := 0
    else
      Result := Node[FocusedIndex].des1.index;
end;

function TNodeInfo.GetDes2Index: integer;
begin
  with Tree do
    if Node[FocusedIndex].OTU then
      Result := 0
    else
      Result := Node[FocusedIndex].des2.index;
end;

function TNodeInfo.GetName: AnsiString;
begin
  if Tree.FocusedIndex = 0 then
    Result := EmptyStr
  else
    Result := Tree.Node[Tree.FocusedIndex].Name;
end;

function TNodeInfo.GetDivTime: double;
begin
  with Tree do
    if FocusedIndex = 0 then
      Result := 0.0
    else
      Result := GetDivergenceTime;
end;

function TNodeInfo.GetSize: Integer;
begin
  if Tree.FocusedIndex >= 1 then
    Result := Tree.Node[Tree.FocusedIndex].Size
  else
    Result := Tree.Root.size;
end;

function TNodeInfo.GetHeight: double;
begin
  with Tree do
    if FocusedIndex = 0 then
      Result := 0.0
    else
      Result := Node[FocusedIndex].height;
end;

function TNodeInfo.GetNodeType: TNodeType;
begin
  with Tree do
    if FocusedIndex = 0 then
      Result := ntNone
    else if Node[FocusedIndex].anc = nil then
      Result := ntRoot
    else if Node[FocusedIndex].compressed then
      Result := ntCompressed
    else if Node[FocusedIndex].hidden then
      Result := ntHidden
    else
      Result := ntInterior;
end;

function TNodeInfo.GetSpeciesName: AnsiString;
begin
  with Tree do
    if Node[FocusedIndex].OTU then
      Result := Node[FocusedIndex].SpeciesName
    else
      Result := EmptyStr;
end;

function TNodeInfo.IsOrHasChildInOutgroup: Boolean;
var
  FocusedNode: TNode;

  function HasChildInOutgroupRecursive(ANode: TNode): Boolean;
  begin
    Result := False;
    if ANode.OTU then
    begin
      if ANode.outgroup then
      begin
        Result := True;
        Exit;
      end;
    end
    else
    begin
      if Assigned(ANode.des1) then
        Result := HasChildInOutgroupRecursive(ANode.des1^);
      if Assigned(ANode.des2) then
        Result := Result or HasChildInOutgroupRecursive(ANode.des2^);
    end;
  end;
begin
  Result := False;
  FocusedNode := Tree.Node[Tree.FocusedIndex]^;
  if FocusedNode.outgroup then
  begin
    Result := True;
    Exit;
  end
  else
  begin
    Result := HasChildInOutgroupRecursive(FocusedNode);
  end;
end;

{ TBranchInfo }

constructor TBranchInfo.Create(Source: TTreeCustomControl);
begin
  inherited Create;
  Tree := Source;
end;

destructor TBranchInfo.Destroy;
begin
  inherited;
end;

function TBranchInfo.GetNodeIndex: integer;
begin
  with Tree do
    Result := FocusedIndex
end;

function TBranchInfo.GetMeanBcl: Double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := 0
    else
      Result := Node[FocusedIndex].branch.MeanBcl;
end;

function TBranchInfo.GetAncNodeIndex: integer;
begin
  with Tree do
    if FocusedIndex = 0 then
      Result := 0
    else if FocusedIndex = Root.index then
      Result := 0
    else
      Result := Node[FocusedIndex].anc.index;
end;

function TBranchInfo.GetBranchType: TBranchType;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := btNone
    else if Node[FocusedIndex].Anc = Root then
      if Root.des1.OTU or Root.des2.OTU then
        Result := btRootedExterior
      else
        Result := btRootedInterior
    else if Node[FocusedIndex].OTU then
      Result := btExterior
    else
      Result := btInterior;
end;

function TBranchInfo.GetLength: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := 0.0
    else if FIsSamplingTimes then
      Result := (GetDivergenceTime(Node[FocusedIndex].index) - GetDivergenceTime(Node[FocusedIndex].anc.index))
    else if FTreeBoxType = tttReltimeTree then
      Result := (GetDivergenceTime(Node[FocusedIndex].anc.index) - GetDivergenceTime(Node[FocusedIndex].index))
    else if (IsLinearized or ForceLinearized) then
      Result := Node[FocusedIndex].anc.height-Node[FocusedIndex].height
    else
      Result := Node[FocusedIndex].branch.length;
end;

function TBranchInfo.GetStats2: Integer;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := 0
    else
      Result := Node[FocusedIndex].branch.stat2;
end;

function TBranchInfo.GetStatsStdDev: Double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := 0.0
    else
      Result := Tree.Node[FocusedIndex].branch.StatsStdDev;
end;

function TBranchInfo.GetTotalLength : double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := 0.0
    else if FIsSamplingTimes then
    begin
      if (Node[FocusedIndex].anc = Root) and (not isRooted) then
        Result := (GetDivergenceTime(Root.des1.index) - GetDivergenceTime(Root.index)) + (GetDivergenceTime(Root.des2.index) - GetDivergenceTime(Root.index))
          //Result := Root.des1.branch.length + Root.des2.branch.length
      else
        Result := GetLength
    end
    else if FTreeBoxType = tttReltimeTree then
    begin
      Result := (GetDivergenceTime(Root.index) - GetDivergenceTime(Root.des1.index)) + (GetDivergenceTime(Root.index) - GetDivergenceTime(Root.des2.index))
    end
    else if (Node[FocusedIndex].anc = Root) and (not isRooted) then
      Result := Root.des1.branch.length + Root.des2.branch.length
    else
      Result := Node[FocusedIndex].branch.length;
end;

function TBranchInfo.GetMaxLen1: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := 0.0
    else
      Result := Node[FocusedIndex].branch.maxlen1;
end;

function TBranchInfo.GetMaxLen2: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := 0.0
    else
      Result := Node[FocusedIndex].branch.maxlen2;
end;

function TBranchInfo.GetSE: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := 0.0
    else
      Result := Tree.Node[FocusedIndex].branch.SE;
end;

function TBranchInfo.GetStats: double;
begin
  with Tree do
    if (FocusedIndex = 0) or (FocusedIndex = Root.index) then
      Result := 0.0
    else
      Result := Node[FocusedIndex].branch.stats;
end;
{ TNodeAttrib}

constructor TNodeAttrib.Create;
begin
  inherited Create;
  FIsTemporaryAttribute := False;
  FLinePen := TPen.Create;
  FBracketPen := TPen.Create;
  FFont := TFont.Create;
  FFont.Name := DEFAULT_FONT_NAME;
  FFont.Size := 12;
  {$IFDEF MSWINDOWS}
  FFont.Charset := DEFAULT_CHARSET;
  FFont.Color := clWindowText;
  FFont.Style := [];
  {$ENDIF}
  FCaptionFont := TFont.Create;
  FCaptionFont.Size := FFont.Size;
  FBrush := TBrush.Create;
  Bitmap := TBitmap.Create;
  JPEGImage   := TJPEGImage.Create;
  GraphicAlign := gaRight;
  FNodeIndex := 0;
  FManualCompressed := False;
  FAutoCompressed  := False;
  FShowTaxonName   := true;
  FShowSpeciesName := False;
  FShowTaxonMarker := true;
  FShowNodeMarker  := true;
  FShowCaption     := False;
  FShowBracket     := False;
  FShowImage       := False;
  FBranchOption    := boHalfBranch;
  Marker.Shape := msNone;
  Marker.Color := clBlack;
  IsJPEG := false;
end;

destructor TNodeAttrib.Destroy;
begin
  JPEGImage.Free;
  Bitmap.Free;
  FBrush.Free;
  FFont.Free;
  FCaptionFont.Free;
  FBracketPen.Free;
  FLinePen.Free;
  inherited;
end;

procedure TNodeAttrib.SetFont(newfont: TFont);
begin
  FFont.Assign(newfont);
end;

function TNodeAttrib.GetManualCompressed: Boolean;
begin
  Result := FManualCompressed;
end;

function TNodeAttrib.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TNodeAttrib.SetAutoCompressed(AValue: Boolean);
begin
  if FAutoCompressed = AValue then Exit;
  FAutoCompressed := AValue;
end;

procedure TNodeAttrib.SetIsTemporaryAttribute(AValue: Boolean);
begin
  if FIsTemporaryAttribute = AValue then Exit;
  FIsTemporaryAttribute := AValue;
end;

procedure TNodeAttrib.SetManualCompressed(AValue: Boolean);
begin
  FManualCompressed := AValue
end;

procedure TNodeAttrib.SetCaptionFont(newfont: TFont);
begin
  FCaptionFont.Assign(newfont);
end;

procedure TNodeAttrib.SetLineColor(value: TColor);
begin
  FLinePen.Color := value;
  FBrush.Color := value;
end;

function TNodeAttrib.GetLineColor: TColor;
begin
  Result := FLinePen.Color;
end;

procedure TNodeAttrib.SetLineWidth(value: integer);
begin
  FLinePen.Width := value;
end;

function TNodeAttrib.GetLineWidth:integer;
begin
  Result := FLinePen.Width;
end;

procedure TNodeAttrib.SetLineStyle(value: TPenStyle);
begin
  FLinePen.Style := value;
end;

function TNodeAttrib.GetLineStyle: TPenStyle;
begin
  Result := FLinePen.Style;
end;

procedure TNodeAttrib.SetBracketLineWidth(value: integer);
begin
  FBracketPen.Width := value;
end;

function TNodeAttrib.GetBracketLineWidth:integer;
begin
  Result := FBracketPen.Width;
end;

procedure TNodeAttrib.SetBracketLineColor(value: TColor);
begin
  FBracketPen.Color := value;
end;

function TNodeAttrib.GetBracketLineColor: TColor;
begin
  Result := FBracketPen.Color;
end;

procedure TNodeAttrib.SetFillStyle(value: TBrushStyle);
begin
  FBrush.Style := value;
end;

function TNodeAttrib.GetFillStyle: TBrushStyle;
begin
  Result := FBrush.Style;
end;

function TNodeAttrib.GetBoldFont: TFont;
begin
  Result := TFont.Create;
  Result.Assign(FFont);
  Result.Style := Result.Style + [fsBold];
end;

procedure TNodeAttrib.SetNodeIndex(AValue: Integer);
begin
  if FNodeIndex = AValue then Exit;
  FNodeIndex := AValue;
end;

procedure TNodeAttrib.SetShowBracket(AValue: boolean);
begin
  if FShowBracket=AValue then Exit;
  FShowBracket:=AValue;
end;

procedure TNodeAttrib.Assign(Source: TNodeAttrib);
begin
    NodeIndex    := Source.NodeIndex;
    //FIsTemporaryAttribute := Source.IsTemporaryAttribute;
    FManualCompressed   := Source.FManualCompressed;
    FAutoCompressed := Source.FAutoCompressed;
    Caption      := Source.Caption;
    Marker       := Source.Marker;
    IsJPEG       := Source.IsJPEG;
    GraphicAlign := Source.GraphicAlign;
    Bitmap.Assign(Source.Bitmap);
    JPEGImage.Assign(Source.JPEGImage);

    FLinePen.Assign(Source.FLinePen);
    FBracketPen.Assign(Source.FBracketPen);
    FFont.Assign(Source.FFont);
    FCaptionFont.Assign(Source.FCaptionFont);
    FBrush.Assign(Source.FBrush);

    FShowTaxonName   := Source.FShowTaxonName;
    FShowSpeciesName := Source.FShowSpeciesName;
    FShowTaxonMarker := Source.FShowTaxonMarker;
    FShowNodeMarker  := Source.FShowNodeMarker;
    FShowCaption     := Source.FShowCaption;
    FShowBracket     := Source.FShowBracket;
    FShowImage       := Source.FShowImage;
    FBranchOption    := Source.FBranchOption;
    FBracketStyle    := Source.FBracketStyle;

    OverwriteMarker  := Source.OverwriteMarker;
    OverwriteDownstream  := Source.OverwriteDownstream;
end;

procedure TNodeAttrib.AssignGraphAttrib(Source: TNodeAttrib);
begin
    GraphicAlign := Source.GraphicAlign;

    FLinePen.Assign(Source.FLinePen);
    FBracketPen.Assign(Source.FBracketPen);
    FFont.Assign(Source.FFont);
    FCaptionFont.Assign(Source.FCaptionFont);
    FBrush.Assign(Source.FBrush);

    FShowTaxonName   := Source.FShowTaxonName;
    FShowSpeciesName := Source.FShowSpeciesName;
    FShowTaxonMarker := Source.FShowTaxonMarker;
    FShowNodeMarker  := Source.FShowNodeMarker;
    FShowCaption     := Source.FShowCaption;
    FShowBracket     := Source.FShowBracket;
    FShowImage       := Source.FShowImage;
    FBranchOption    := Source.FBranchOption;
    FBracketStyle    := Source.FBracketStyle;
end;

procedure TNodeAttrib.AssignBitmap(image: TBitmap);
begin
  Bitmap.Assign(image);
  if IsJPEG then
  begin
    JPEGImage.Free;
    JPEGImage := TJPEGImage.Create;
    IsJPEG := false;
  end;
end;

procedure TNodeAttrib.AssignJPEG(image: TJPEGImage);
begin
  JPEGImage.Assign(image);
  Bitmap.Assign(JPEGImage);
  IsJPEG := true;
end;

procedure TNodeAttrib.LoadBitmapFromFile(filename: AnsiString);
begin
  Bitmap.LoadFromFile(filename);
  if IsJPEG then
  begin
    JPEGImage.Free;
    JPEGImage := TJPEGImage.Create;
    IsJPEG := false;
  end;
end;

procedure TNodeAttrib.LoadJPEGFromFile(filename: AnsiString);
begin
  JPEGImage.LoadFromFile(filename);
  Bitmap.Assign(JPEGImage);
  IsJPEG := true;
end;

procedure TNodeAttrib.ClearImage;
begin
  Bitmap.Free;
  Bitmap := TBitmap.Create;
  if IsJPEG then
  begin
    JPEGImage.Free;
    JPEGImage := TJPEGImage.Create;
    IsJPEG := false;
  end;
end;

function TNodeAttrib.IsImage: boolean;
begin
  Result := not Bitmap.Empty;
end;

function TNodeAttrib.IsSameFont(aFont: TFont): Boolean;
begin
  Result := ((FFont.Name = aFont.Name) and
             (FFont.Size = aFont.Size) and
             (FFont.Color = aFont.Color) and
             (FFont.Style = aFont.Style));
end;

function TNodeAttrib.ShowLabel: boolean;
begin
  if Caption = '' then
    result := false
  else if ManualCompressed then
    result := FShowTaxonName
  else
    result := FShowCaption;
end;

function TNodeAttrib.DebugString: String;
begin
  Result := Format('NodeIndex: %d %s', [NodeIndex, LineEnding]);
  Result := Result + Format('ManualCompressed: %s %s', [BoolToStr(FManualCompressed, True), LineEnding]);
  Result := Result + Format('AutoCompressed: %s %s', [BoolToStr(FAutoCompressed, True), LineEnding]);
  Result := Result + Format('Caption: %s %s', [Caption, LineEnding]);
  Result := Result + Format('OverwriteDownstream: %s %s', [BoolToStr(OverwriteDownstream, True), LineEnding]);
  Result := Result + Format('Name: %s %s', [Name, LineEnding]);
  Result := Result + Format('FBranchOption: %s %s', [BranchOptionToStr(FBranchOption), LineEnding]);
  Result := Result + Format('FBracketStyle: %s ', [BracketStyleToString(FBracketStyle)]);
  Result := Result + Format('LineColor: %s', [SColorToHtmlColor(LineColor)]);
end;

procedure TNodeAttrib.DebugStrings(var aList: TStringList);
begin
  //add node info for all nodes that are attribute holders
  aList.Add(Format('FNodeIndex            = %d', [FNodeIndex]));
  aList.Add(Format('Name                  = %s', [Name]));
  aList.Add(Format('Caption               = %s', [Caption]));
  aList.Add(Format('CaptionSize           = %d', [CaptionSize]));
  aList.Add(Format('FShowCaption          = %s', [BoolToStr(FShowCaption, True)]));
  aList.Add(Format('FShowBracket          = %s', [BoolToStr(FShowBracket, True)]));
  aList.Add(Format('FShowImage            = %s', [BoolToStr(FShowImage, True)]));
  aList.Add(Format('Marker                = %s', [NodeMarkerShapeToHtmlString(Marker.Shape)]));
  aList.Add(Format('OverwriteMarker       = %s', [BoolToStr(OverwriteMarker, True)]));
  aList.Add(Format('Overwrite Downstream  = %s', [BoolToStr(OverwriteDownstream, True)]));
  aList.Add(Format('GraphicAlign          = %s', [GraphicAlignToString(GraphicAlign)]));
  aList.Add(Format('FAutoCompressed       = %s', [BoolToStr(FAutoCompressed, True)]));
  aList.Add(Format('FIsTemporaryAttribute = %s', [BoolToStr(FIsTemporaryAttribute, True)]));
  aList.Add(Format('FManualCompressed     = %s', [BoolToStr(FManualCompressed, True)]));
  aList.Add(Format('FShowTaxonName        = %s', [BoolToStr(FShowTaxonName, True)]));
  aList.Add(Format('FShowSpeciesName      = %s', [BoolToStr(FShowSpeciesName, True)]));
  aList.Add(Format('FShowTaxonMarker      = %s', [BoolToStr(FShowTaxonMarker, True)]));
  aList.Add(Format('FShowNodeMarker       = %s', [BoolToStr(FShowNodeMarker, True)]));
  aList.Add(Format('FBranchOption         = %s', [BranchOptionToStr(FBranchOption)]));
  aList.Add(Format('FBracketStyle         = %s', [BracketStyleToString(FBracketStyle)]));
  aList.Add(Format('IsJPEG                = %s', [BoolToStr(IsJPEG, True)]));
  aList.Add(Format('FImageScaled          = %s', [BoolToStr(FImageScaled, True)]));
  aList.Add(Format('NameFontSize          = %d', [FFont.Size]));
  aList.Add(Format('CaptFontSize          = %d', [FCaptionFont.Size]));
end;

function TNodeAttrib.IsCompressed: Boolean;
begin
  Result := (FManualCompressed or FAutoCompressed);
end;

function TNodeAttrib.ReadFromFile(var data: File; SessionFileVersion: integer; TreeSessionVersion: Integer):boolean;
var
  stream: TMemoryStream;
  buffer : array[0..4095] of AnsiChar;
  color: TColor = clBlack;
  penstyle: TPenStyle = psClear;
  fontstyle: TFontStyles = [];
  brushstyle: TBrushStyle = bsClear;
  m: TNodeMarkerShape = msNone;
  n: integer = -1;
  penStyleSize: Integer = -1;
begin
  for n := Low(buffer) to High(buffer) do
    buffer[n] := #0;
  if TreeSessionVersion >= 703 then
    penStyleSize := SizeOf(TPenStyle)
  else
    penStyleSize := 1;
  BlockRead(data, n, 4);
  if n > -11 then
    NodeIndex := n
  else
  begin
    SessionFileVersion := -n;
    BlockRead(data, FNodeIndex, 4);
  end;
  BlockRead(data, n, 4);   // for Name
  if SessionFileVersion >= 6 then
    if n > 0 then
    begin
      BlockRead(data, buffer, n);
      buffer[n] := #0;
      Name := StrPas(buffer);
    end;

  BlockRead(data, n, 4);
  if n > 0 then
  begin
    BlockRead(data, buffer, n);
    buffer[n] := #0;
    Caption := StrPas(buffer);
  end;

  BlockRead(data, n, 4);   // for Bracket
  BlockRead(data, n, 4);
  BlockRead(data, n, 4);
  BlockRead(data, n, 4);

  BlockRead(data, FBranchOption, SizeOf(TBranchOption));
  BlockRead(data, FShowCaption, SizeOf(Boolean));
// from version 7
  if SessionFileVersion >= 7 then
    BlockRead(data, FShowNodeMarker, SizeOf(Boolean));
// from version 5
  if SessionFileVersion >= 5 then
    BlockRead(data, FShowBracket, SizeOf(Boolean));
// from version 6
  if SessionFileVersion >= 6 then
    BlockRead(data, FBracketStyle, SizeOf(TBracketStyle));
  if SessionFileVersion >= 9 then
    BlockRead(data, FManualCompressed, SizeOf(Boolean));
  if TreeSessionVersion >= 1007 then
    BlockRead(data, FAutoCompressed, SizeOf(Boolean));
  BlockRead(data, color, SizeOf(TColor));
  FLinePen.Color := color;
  BlockRead(data, n, 4);
  FLinePen.Width := n;
  BlockRead(data, penstyle, penStyleSize);
  FLinePen.Style := penstyle;

  BlockRead(data, color, SizeOf(TColor));
  FBracketPen.Color := color;
  BlockRead(data, n, 4);
  FBracketPen.Width := n;
  BlockRead(data, penstyle, penStyleSize);
  FBracketPen.Style := penstyle;

  BlockRead(data, n, 4);
  if n > 0 then
  begin
    BlockRead(data, buffer, n);
    buffer[n] := #0;
    FFont.Name := StrPas(buffer);
  end;
  BlockRead(data, color, SizeOf(TColor));
  FFont.Color := color;
  BlockRead(data, n, 4);
  FFont.Size := n;
  if TreeSessionVersion >= 703 then
    BlockRead(data, fontstyle, SizeOf(TFontStyle))
  else
    BlockRead(data, fontstyle, 1); // in Delphi, the size was 1
  FFont.Style := ValidFontStyles(fontstyle);

  if SessionFileVersion > 0 then
  begin
    BlockRead(data, n, 4);
    if n > 0 then
    begin
      BlockRead(data, buffer, n);
      buffer[n] := #0;
      FCaptionFont.Name := StrPas(buffer);
    end;
    BlockRead(data, color, SizeOf(TColor));
    FCaptionFont.Color := color;
    BlockRead(data, n, 4);
    FCaptionFont.Size := n;
    if TreeSessionVersion >= 703 then
      BlockRead(data, fontstyle, SizeOf(TFontStyle))
    else
      BlockRead(data, fontstyle, 1); // in Delphi, the size was 1
    FCaptionFont.Style := ValidFontStyles(fontstyle);
  end;

  BlockRead(data, color, SizeOf(TColor));
  FBrush.Color := color;
  if TreeSessionVersion >= 703 then
    BlockRead(data, brushstyle, SizeOf(TBrushStyle))
  else
    BlockRead(data, brushstyle, 1); // size was 1 in Delphi
  FBrush.Style := brushstyle;

  if SessionFileVersion >= 4 then
  begin
    BlockRead(data, m, SizeOf(TNodeMarkerShape));
    Marker.Shape := m;
    BlockRead(data, color, SizeOf(TColor));
    Marker.Color := color;

    BlockRead(data, n, 4);
    if n > 0 then
    begin
      stream := TMemoryStream.Create;
      stream.SetSize(n);
      BlockRead(data, stream.Memory^, n);
      stream.Position := 0;
      BlockRead(data, n, 4);
      if n = 0 then
      begin
        Bitmap.LoadFromStream(stream);
        IsJPEG := false;
      end
      else if n = 1 then
      begin
        JPEGImage.LoadFromStream(stream);
        Bitmap.Assign(JPEGImage);
        IsJPEG := true;
      end;
      stream.Free;
    end;
  end;
  if SessionFileVersion >= 5 then
  begin
    BlockRead(data, FShowTaxonName, SizeOf(boolean));
    BlockRead(data, FShowTaxonMarker, SizeOf(boolean));
  end;
  if SessionFileVersion >= 10 then
    BlockRead(data, OverwriteMarker, SizeOf(boolean));
  if SessionFileVersion >= 11 then
    BlockRead(data, OverwriteDownstream, SizeOf(boolean));

  result := true;
end;

procedure TNodeAttrib.WriteToFile(var data: File);
var
  stream: TMemoryStream;
  buffer : array[0..4095] of AnsiChar;
  color: TColor;
  penstyle: TPenStyle;
  fontstyle: TFontStyles;
  brushstyle: TBrushStyle;
  m: TNodeMarkerShape;
  n: integer;
  version: integer;
begin
  version := -11;
  BlockWrite(data, version, 4);  // from Version 11;

  BlockWrite(data, NodeIndex, 4);  // NodeIndex
// Name
  n := Length(Name);
  BlockWrite(data, n, 4);
  StrPCopy(buffer, Name);
  BlockWrite(data, buffer, n);
// Caption
  n := Length(Caption);
  BlockWrite(data, n, 4);
  StrPCopy(buffer, Caption);
  BlockWrite(data, buffer, n);
// Bracket
  BlockWrite(data, n, 4);
  BlockWrite(data, n, 4);
  BlockWrite(data, n, 4);
  BlockWrite(data, n, 4);

  BlockWrite(data, FBranchOption, SizeOf(TBranchOption));
  BlockWrite(data, FShowCaption, SizeOf(Boolean));
// from version 7
  BlockWrite(data, FShowNodeMarker, SizeOf(Boolean));
// from version 5
  BlockWrite(data, FShowBracket, SizeOf(Boolean));
// from version 6
  BlockWrite(data, FBracketStyle, SizeOf(TBracketStyle));
// from version 9
  BlockWrite(data, ManualCompressed, SizeOf(Boolean));
  BlockWrite(data, FAutoCompressed, SizeOf(Boolean));
// LinePen
  color := FLinePen.Color;
  BlockWrite(data, color, SizeOf(TColor));
  n := FLinePen.Width;
  BlockWrite(data, n, 4);
  penstyle := FLinePen.Style;
  BlockWrite(data, penstyle, SizeOf(TPenStyle));
// Bracketpen
  color := FBracketPen.Color;
  BlockWrite(data, color, SizeOf(TColor));
  n := FBracketPen.Width;
  BlockWrite(data, n, 4);
  penstyle := FBracketPen.Style;
  BlockWrite(data, penstyle, SizeOf(TPenStyle));
// Font
  n := Length(Font.Name);
  BlockWrite(data, n, 4);
  StrPCopy(buffer, Font.Name);
  BlockWrite(data, buffer, n);
  color := Font.Color;
  BlockWrite(data, color, SizeOf(TColor));
  n := Font.Size;
  BlockWrite(data, n, 4);
  fontstyle := Font.Style;
  BlockWrite(data, fontstyle, SizeOf(TFontStyle));

// CaptionFont
  n := Length(CaptionFont.Name);
  BlockWrite(data, n, 4);
  StrPCopy(buffer, CaptionFont.Name);
  BlockWrite(data, buffer, n);
  color := CaptionFont.Color;
  BlockWrite(data, color, SizeOf(TColor));
  n := CaptionFont.Size;
  BlockWrite(data, n, 4);
  fontstyle := CaptionFont.Style;
  BlockWrite(data, fontstyle, SizeOf(TFontStyle));
// Brush
  color := LineColor;//a.FillColor;
  BlockWrite(data, color, SizeOf(TColor));
  brushstyle := FillStyle;
  BlockWrite(data, brushstyle, SizeOf(TBrushStyle));

// From version 4
  m := Marker.Shape;
  BlockWrite(data, m, SizeOf(TNodeMarkerShape));
  color := Marker.Color;
  BlockWrite(data, color, SizeOf(TColor));

  if not IsImage then
  begin
    n := 0;
    BlockWrite(data, n, 4);
  end
  else
  begin
    stream := TMemoryStream.Create;
    if IsJPEG then
      JPEGImage.SaveToStream(stream)
    else
      Bitmap.SaveToStream(stream);
    stream.Position := 0;
    n := stream.Size;
    BlockWrite(data, n, 4);
    BlockWrite(data, stream.Memory^, n);
    if IsJPEG then
      n := 1
    else
      n := 0;
    BlockWrite(data, n, 4);
    stream.Free;
  end;
// From version 5
  BlockWrite(data, FShowTaxonName, SizeOf(Boolean));
  BlockWrite(data, FShowTaxonMarker, SizeOf(Boolean));
// From version 10
  BlockWrite(data, OverwriteMarker, SizeOf(Boolean));
// From version 11
  BlockWrite(data, OverwriteDownstream, SizeOf(Boolean));
end;


{ TNodeAttribList}

function TNodeAttribList.GetItems(Index:integer): TNodeAttrib;
var
  p: Pointer = nil;
begin
  p := inherited Items[Index];
  Result := TNodeAttrib(p);
end;

procedure TNodeAttribList.SetItems(Index:integer; AItem: TNodeAttrib);
begin
  inherited Items[Index] := AItem;
end;

function TNodeAttribList.DebugStrings: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      Result.Add(Format('AttributeIndex        = %d', [i]));
      Items[i].DebugStrings(Result);
      Result.Add('------------------------------');
    end;
end;

function TNodeAttribList.GetNames: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if Count > 0 then
    for i := 0 to Count - 1 do
      Result.Add(Items[i].Name);
end;

procedure TNodeAttribList.DeleteAll;
var
  i: Integer;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
      GetItems(i).Free;
  inherited Clear;
end;

function TNodeAttribList.IndexOfName(aName: String): Integer;
var
  i: Integer;
  attrib: TNodeAttrib;
begin
  Result := -1;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      attrib := inherited Items[i];
      if aName = attrib.Name then
      begin
        Result := i;
        break;
      end;
    end;
end;

function TNodeAttribList.IsSameFont(index: Integer; aFont: TFont): Boolean;
begin
  Result := GetItems(index).IsSameFont(aFont);
end;

function TNodeAttribList.Add(Item: Pointer): Integer;
begin
  Result := inherited Add(Item);
end;

{ TTreeEditBox}

constructor TTreeEditBox.Create(AOwner: TComponent; aType: TTreeBoxType);
begin
  inherited Create(AOwner, aType);
  SkipDrawing := False;
  UndoStack := TStack.Create;
  ControlStyle := ControlStyle + [csDisplayDragImage];
  FFocusedBranchPen := TPen.Create;
  DivTimeFunc := HeightAsDivergenceTime;
  SetLength(FNodeMarkerPoints, 4);
  SetLength(FPolyLinePoints, 3);
  SetLength(FLinePoints, 2);
end;

destructor TTreeEditBox.Destroy;
begin
  ClearUndoStack;
  FFocusedBranchPen.Free;

  inherited;
end;

function TTreeEditBox.GetIndexOfPrivateName(Name: AnsiString): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=1 to NoOfOTUs do
  begin
      if AnsiCompareStr(Name, GetOTUPrivateName(i)) = 0 then
    begin
      result := i;
      break;
    end;
  end;
end;

function TTreeEditBox.GetOTUPrivateName(index: integer): AnsiString;
begin
   if index = 0 then
        if FocusedIndex = 0 then
            Result := ''
        else
            Result := Node[FocusedIndex].PrivateName
    else
        Result := Node[index].PrivateName;
end;

procedure TTreeEditBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Dragging then
  begin
    Dragging := false;
    { We need to manually handle starting and stopping drag-drop operations } 
    EndDrag(true);
    Cursor := crDefault;
    DragCursor := crDefault;
    Invalidate;
  end;
  if FShowSelection then
    if BranchFocused and (TargetBranch > 0) then
      MouseDown(Button, Shift, X, Y);
  inherited;
end;

procedure TTreeEditBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  BranchCursorAccept: TCursorImage = nil;
  BranchCursorDeny: TCursorImage = nil;
begin
  if EditEnabled then
  begin
    if TreeStyle = tsTraditional then
      if Button = mbLeft then
      begin
        { don't start any drag operations if an EditBox is active }
        if Screen.ActiveControl is TEdit then
        begin
          TEdit(EditBox).Hide;
          Exit;
        end;

        if (ssAlt in Shift) or (ssCtrl in Shift) or (ssShift in Shift) then
          if BranchFocused then
            TargetBranch := GetBranchIndexAtCoords(x,y);

        if BranchFocused and (TargetBranch > 0) then
        begin
          try
            MoveNode(FocusedIndex, TargetBranch);
          finally
            { in case of an exception, need to reset TargetBranch or exceptions can continue indefinitely}
            ClearFocus;
            TargetBranch := 0;
          end;

          Refresh;
          { FocusedIndex is still accurate - let's just jump to it directly
            no need to call the FocusBranch to find it again 
            especially since this iterates through all tree branches.
            could be a problem for large trees. }
          FocusOnBranch(FocusedIndex);
          exit;
        end
        else
        begin
          { FocusTime also sets FocusedIndex
            so adding it here lets branch lengths be a valid click target for initiating drag-drog }
          FocusTime(x, y);
          if FocusedTimeIndex = 0 then
            FocusBranch(x, y);

          if FocusedIndex > 0 then
          begin
            try
              Dragging := true;
              { create custom cursor images that are visual representations of the branch(es)
                to be drag-dropped and store them in Screen.Cursors. They can then be accessed
                simply, for example by setting Self.Cursor :=  CURSOR_DRAG_BRANCH
                Note: default cursors are stored in Screen.Cursors using negative indices. We
                add any new cursors we want in Screen.Cursors using positive indices. The default
                cursors should NEVER be overridden as they are used globally throughout the application.}
              BranchCursorDeny := BranchImageCursor(Node[FocusedIndex]);
              Screen.Cursors[CURSOR_DRAG_BRANCH] := BranchCursorDeny.ReleaseHandle;
              BranchCursorAccept := BranchImageCursor(Node[FocusedIndex], True);
              Screen.Cursors[CURSOR_DROP_BRANCH] := BranchCursorAccept.ReleaseHandle;
              Cursor := CURSOR_DRAG_BRANCH;
              DragCursor := CURSOR_DRAG_BRANCH;
              BeginDrag(false,4); { prevents the drag operation from starting until the user has moved the cursor at least 4 pixels away }
            finally
              BranchCursorDeny.Free;
              BranchCursorAccept.Free;
            end;
          end;
          Draw;
          if FocusedIndex = 0 then
            FocusName(x,y);
        end;
      end;
  end
  else
  begin
    if IsResizing then
    begin
      GetCursorPos(FDragStartPoint);
      FDragStartPoint := ScreenToClient(FDragStartPoint);
      FStartPixelsPerOTU := PixelsPerOTU;
      FStartTreeWidth := TreeWidth;
      FStartTreeHeight := TreeHeight;
      BeginDrag(false,4);
    end;
  end;
  inherited;
end;

procedure TTreeEditBox.MouseMove(Shift: TShiftState; X, Y: Integer);

  function IsParentOfFocusedNode(aNodeIndex: Integer): Boolean;
  begin
    Result := (aNodeIndex = Node[FocusedIndex].anc.index);
  end;

  function IsSiblingOfFocusedNode(aNodeIndex: Integer): Boolean;
  var
    aNode: TpNode = nil;
  begin
    Result := False;
    if aNodeIndex <= 0 then
      Exit;

    aNode := Node[aNodeIndex];
    if not Assigned(aNode.anc) then
      Exit;

    if aNode = aNode.anc.des1 then
      Result := (aNode.anc.des2.index = FocusedIndex)
    else
      Result := (aNode.anc.des1.index = FocusedIndex);
  end;

  function IsDescendantOfFocusedIndex(aNodeIndex: Integer): Boolean;
  var
    aNode: TpNode = nil;
    aFocusedNode: TpNode = nil;
  begin
    Result := False;
    if aNodeIndex <= 0 then
      Exit;
    aFocusedNode := Node[FocusedIndex];
    aNode := Node[aNodeIndex];

    while Assigned(aNode.anc) do
    begin
      if aNode.anc = aFocusedNode then
        Exit(True);
      aNode := aNode.anc;
    end;
  end;

var
  FoundBranchIndex: Integer = -1;
begin
  if EditEnabled and BranchFocused and Dragging then
  begin
    FoundBranchIndex := GetBranchIndexAtCoords(X, Y);
    Cursor := CURSOR_DRAG_BRANCH;
    DragCursor := CURSOR_DRAG_BRANCH;
    if (FoundBranchIndex = FocusedIndex) or
       IsParentOfFocusedNode(FoundBranchIndex) or
       IsDescendantOfFocusedIndex(FoundBranchIndex) or
       IsSiblingOfFocusedNode(FoundBranchIndex) then
      TargetBranch := 0
    else
      TargetBranch := FoundBranchIndex;
  end
  else
    Cursor := crDefault;
  inherited;
  Invalidate;
end;

function TTreeEditBox.BranchImageCursor(SelectedNode: TpNode; FoundTarget: Boolean = False): TCursorImage;
var
  BranchImage: TBitmap = nil;
  CroppedImage: TBitmap = nil;
  linePoints: TPointArray;
  MinX, MinY, MaxX, MaxY: Integer;
  NodeX, NodeY: Integer;
  srcRect, destRect: TRect;

  procedure DrawEachTrailer(node: TpNode);
  begin
    setLength(linePoints, 4);
    linePoints[0].x := round((node.des1.position.x));
    linePoints[0].y := round((node.des1.position.y));
    linePoints[1].x := round((node.position.x));
    linePoints[1].y := round((node.des1.position.y));
    linePoints[2].x := round((node.position.x));
    linePoints[2].y := round((node.des2.position.y ));
    linePoints[3].x := round((node.des2.position.x ));
    linePoints[3].y := round((node.des2.position.y));

    MinX := min(linePoints[2].x ,MinX);
    MinY := min(min(linePoints[0].y, linePoints[3].y),MinY);
    MaxX := max(max(linePoints[0].x, linePoints[3].x),MaxX);
    MaxY := max(max(linePoints[0].y, linePoints[3].y),MaxY);

    BranchImage.Canvas.Polyline(linePoints);

    if not (node.des1.OTU or node.des1.compressed) then
      DrawEachTrailer(node.des1);
    if not (node.des2.OTU or node.des2.compressed) then
      DrawEachTrailer(node.des2);
  end;

begin
  try
    BranchImage := TBitmap.Create;
    BranchImage.PixelFormat := pf32Bit;
    BranchImage.Width := ScrollWidth;
    BranchImage.Height := ScrollHeight;
    BranchImage.TransparentColor := clNone;
    BranchImage.Transparent := True;

    { we use a 32-bit color bitmap in order to support transparent pixels.
      so that the drag-drop cursor will have transparency.

      To make this work with a standard Canvas, we must take a convoluted approach.

      FPC-defined colors are unusable because they effectively have an alpha channel
      of 0 and will be fully transparent\invisible on 32-bit canvases.
      They also store RGB data in a different order.

      We cannot directly assign a 32-bit color (unsigned int) to a TPen (signed int).

      Instead we define a transparent color and use pmNotCopy to invert the color value,
      making it opaque. This means the RGB values we provide must be the inverse
      of the color we want to appear. }

    { draw the node marker }
    BranchImage.Canvas.Pen.Style := psSolid;
    BranchImage.Canvas.Pen.Mode := pmNotCopy;
    if not FoundTarget then
    begin
      BranchImage.Canvas.Pen.Color := RGB($FF, 0, 0) xor $FFFFFF;  // inverted red
      BranchImage.Canvas.Pen.Width := 2;
    end
    else
    begin
      BranchImage.Canvas.Pen.Color := RGB(0, $FF, $FF) xor $FFFFFF; // inverted light blue
      BranchImage.Canvas.Pen.Width := 2;
    end;

    FNodeMarkerPoints[0].x := round((SelectedNode.anc.position.x));
    FNodeMarkerPoints[1].x := FNodeMarkerPoints[0].x-5;
    FNodeMarkerPoints[2].x := FNodeMarkerPoints[0].x;
    FNodeMarkerPoints[3].x := FNodeMarkerPoints[0].x+5;
    if Node[FocusedIndex] = SelectedNode.anc.des1 then
    begin
      FNodeMarkerPoints[0].y := round((SelectedNode.position.y)) +(yunit div 8)-5;
      FNodeMarkerPoints[1].y := FNodeMarkerPoints[0].y+5;
      FNodeMarkerPoints[2].y := FNodeMarkerPoints[1].y+5;
      FNodeMarkerPoints[3].y := FNodeMarkerPoints[1].y;
    end
    else
    begin
      FNodeMarkerPoints[0].y := round((SelectedNode.position.y)) -(yunit div 8)+5;
      FNodeMarkerPoints[1].y := FNodeMarkerPoints[0].y-5;
      FNodeMarkerPoints[2].y := FNodeMarkerPoints[1].y-5;
      FNodeMarkerPoints[3].y := FNodeMarkerPoints[1].y;
    end;

    MinX := min(FNodeMarkerPoints[1].x, FNodeMarkerPoints[3].x);
    MinY := min(FNodeMarkerPoints[0].y, FNodeMarkerPoints[2].y);
    MaxX := max(FNodeMarkerPoints[1].x, FNodeMarkerPoints[3].x);
    MaxY := max(FNodeMarkerPoints[0].y, FNodeMarkerPoints[2].y);

    NodeX := FNodeMarkerPoints[0].x;
    NodeY := FNodeMarkerPoints[0].y;
    BranchImage.Canvas.Polygon(FNodeMarkerPoints);

    { draw the selected branch }

    BranchImage.Canvas.Pen.Mode := pmNotCopy;
    BranchImage.Canvas.Pen.Color := clDkGray;
    BranchImage.Canvas.Pen.Width := 1;
    BranchImage.Canvas.Pen.Style := psDot;

    if yunit < 40 then
    begin
      FLinePoints[0].x := round((SelectedNode.anc.position.x));
      FLinePoints[0].y := round((SelectedNode.position.y));
      FLinePoints[1].x := round((SelectedNode.position.x));
      FLinePoints[1].y := FLinePoints[0].y;
      BranchImage.Canvas.Polyline(FLinePoints);

      MinX := min(min(FLinePoints[0].x, FLinePoints[1].x),MinX);
      MinY := min(min(FLinePoints[0].y, FLinePoints[1].y),MinY);
      MaxX := max(max(FLinePoints[0].x, FLinePoints[1].x),MaxX);
      MaxY := max(max(FLinePoints[0].y, FLinePoints[1].y),MaxY);
    end
    else
    begin

      FPolyLinePoints[0].x := round((SelectedNode.anc.position.x));
      if SelectedNode = SelectedNode.anc.des1 then
        FPolyLinePoints[0].y := round((SelectedNode.position.y)) +(yunit div 8) -5
      else
        FPolyLinePoints[0].y := round((SelectedNode.position.y)) -(yunit div 8) +5;
      FPolyLinePoints[1].x := FPolyLinePoints[0].x;
      FPolyLinePoints[1].y := round((SelectedNode.position.y));
      FPolyLinePoints[2].x := round((SelectedNode.position.x));
      FPolyLinePoints[2].y := FPolyLinePoints[1].y;
      BranchImage.Canvas.Polyline(FPolyLinePoints);

      MinX := min(min(FPolyLinePoints[0].x, FPolyLinePoints[2].x),MinX);
      MinY := min(min(FPolyLinePoints[0].y, FPolyLinePoints[1].y),MinY);
      MaxX := max(max(FPolyLinePoints[0].x, FPolyLinePoints[2].x),MaxX);
      MaxY := max(max(FPolyLinePoints[0].y, FPolyLinePoints[1].y),MaxY);
    end;

    { draw the child branches }

    if not (SelectedNode.OTU or SelectedNode.compressed) then
      DrawEachTrailer(SelectedNode);

    MinX := max(0, MinX);
    MinY := max(0, MinY);

    srcRect := rect(MinX, MinY, MaxX, MaxY+1);
    destRect := rect(0,0,MaxX-MinX,MaxY-MinY+1);

    NodeX := NodeX - MinX;
    NodeY := NodeY - MinY;

    CroppedImage := TBitmap.Create;
    CroppedImage.PixelFormat := pf32Bit;
    CroppedImage.SetSize(destRect.Right,destRect.Bottom);
    CroppedImage.Canvas.CopyRect(destRect,BranchImage.Canvas,srcRect);
    CroppedImage.TransparentColor := clNone;
    CroppedImage.Transparent := True;

    Result := TCursorImage.Create;
    Result.Add(CroppedImage.PixelFormat,CroppedImage.Height,CroppedImage.Width);
    Result.AssignImage(CroppedImage);
    Result.Mask(clNone);
    Result.Masked := True;
    Result.HotSpot:= Point(NodeX, NodeY);
  finally
    if Assigned(BranchImage) then
      BranchImage.Free;
    if Assigned(CroppedImage) then
      CroppedImage.Free;
  end;
end;

{ TTreeEditBox of GetBranchPen specifies alternate pen for drawing selected branches }
function TTreeEditBox.GetBranchPen: TPen;
begin
  if FDrawingFocusedNode then
  begin
    FFocusedBranchPen.Assign(FBranchPen);
    FFocusedBranchPen.Style := psClear;

    Result := FFocusedBranchPen;
  end
  else
    Result := FBranchPen;
end;

procedure TTreeEditBox.DrawBranch(p: TpNode);
begin
  { set a flag to specify alternate drawing behavior for the selected branch
    decided not to use this approach for the moment, but will revisit
    if we need to draw dashed lines on circular\radiation trees }
  //FDrawingFocusedNode := (FocusedIndex <> 0) and (p = Node[FocusedIndex]);
  inherited;
  //FDrawingFocusedNode := False;
end;

procedure TTreeEditBox.DrawFocus;

  procedure DrawBranch(node: TpNode; Recursive: Boolean = true); // Added recursive here so that when we click on a branch we can select only that branch and not the whole subtree.
  var
    p : array[0..2] of TPoint;
    clientCoords: array[0..2] of TPoint;
  begin
    if (not (node.OTU or node.compressed)) and (Recursive) then
    begin
      DrawBranch(node.des1);
      DrawBranch(node.des2);
    end;
    if node.anc <> nil then
    begin
      p[0].x := round((node.anc.position.x +xbase));
      p[0].y := round((node.anc.position.y +ybase));
      p[1].x := p[0].x;
      p[1].y := round((node.position.y +ybase));
      p[2].x := round((node.position.x +xbase));
      p[2].y := p[1].y;
      ConvertToClientCoords(p, clientCoords, 3);
      MPolyLine(clientCoords);
    end;
  end;

begin
  if not EditEnabled then
    inherited
  else
  begin
    // we cover up the drawn branch so the dashed lines appear correctly
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Mode := pmNotCopy;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    DrawBranch(Node[FocusedIndex], false);

    // then draw the dashed lines
    Canvas.Pen.Style := psDash;
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Width := 3;
    DrawBranch(Node[FocusedIndex], false);
  end;

end;

function TTreeEditBox.GetLastTreeState:TTreeEditOperation;
begin
  Result := nil;
  try
    if UndoStack.Count > 0 then
      Result := TTreeEditOperation(UndoStack.Peek);
  except
    on E:Exception do
      raise Exception.Create('Application error when retrieving previous tree state: ' + E.Message);
  end;
end;

{ added behavior for adjusting branch lengths if user is working with timetrees }
function TTreeEditBox.MoveNode(p, target: TpNode):boolean;
var
  sibling: TpNode;
  sourceparent: TpNode;
  SourceDesLength, TargetDesLength: Double;
  Temp1, Temp2: Double;
begin
  Result := False;  
  if (p = target) then
    Exit;

  SourceDesLength := p.branch.maxlen2 - p.branch.length;
  TargetDesLength := target.branch.maxlen2 - target.branch.length;

  if p = p.anc.des1 then
  begin
    sibling := p.anc.des2;
    if IsTimetree then
      sibling.branch.length := sibling.branch.length + p.anc.branch.length;
    p.anc.des2 := target;
  end
  else
  begin
    sibling := p.anc.des1;
    if IsTimetree then
      sibling.branch.length := sibling.branch.length + p.anc.branch.length;
    p.anc.des1 := target;
  end;
  if p.anc = Root then
  begin
    sibling.anc := nil;
    Root := sibling;
  end
  else
  begin
    if p.anc = p.anc.anc.des1 then
      p.anc.anc.des1 := sibling
    else
      p.anc.anc.des2 := sibling;
    sibling.anc := p.anc.anc;
  end;
  if target = target.anc.des1 then
    target.anc.des1 := p.anc
  else
    target.anc.des2 := p.anc;
  p.anc.anc := target.anc;
  target.anc := p.anc;

  if IsTimetree then
  begin

  // if source maxlen2 > target deslength then
  // if source branch length < target branch length then
    // shorten target branch length accordingly

    // Scenario 1: source is shorter than target
    // Scenario 1A: source is greater length than target descendant
    if p.branch.maxlen2 > TargetDesLength then
    begin
      Temp1 := target.branch.maxlen2;
      Temp2 := p.branch.maxlen2;
      if target.branch.maxlen2 > p.branch.maxlen2 then
      begin
        target.anc.branch.length := target.branch.length - (p.branch.maxlen2 - TargetDesLength);
        target.branch.length := (p.branch.maxlen2 - TargetDesLength);
      end
      else
      begin
        p.anc.branch.length := 0;
        // shrink source node to fit in with target where possible
        if SourceDesLength <= TargetDesLength then
           p.branch.length := target.branch.length - (TargetDesLength - SourceDesLength)
        else if target.OTU and (SourceDesLength <= target.branch.maxlen2) then
           p.branch.length := target.branch.length - SourceDesLength;
        // otherwise, let the user deal with manually resizing branches.
      end;
    end
    else
    begin
      p.branch.length := p.branch.length + (TargetDesLength - p.branch.maxlen2);
      p.anc.branch.length := target.branch.length;
      target.branch.length := 0;
    end;

    // Scenario 1B: source is shorter than target descendant
    // lengthen source blen accordingly

    // Scenario 2: source is longer than target
    // attempt to shorten source blen
    // but what if source has descendants preventing this?

    // reset maxlen values
    SetMaxLength(Root);
  end;

  if Root <> Node[NoOfNodes] then SetRootAtLast;
  result := true;
end;

function TTreeEditBox.MoveNode(Source, Target: Integer): Boolean;
var
  SourceNode, TargetNode: TpNode;
begin
  SourceNode := Node[Source];
  TargetNode := Node[Target];
  Result := MoveNode(SourceNode, TargetNode);
  if Result then
     UndoStack.Push(TTreeEditOperation.CreateOpTopologyChanged(GetLastTreeState, uMoveNode));
end;

procedure TTreeEditBox.RemoveOTU(index: integer);
var
  newtree: TTreeData = nil;
  newname: TStringList = nil;
  newprivatename: TStringList = nil;
  UndoOp: TTreeEditOperation = nil;
begin
  if NoOfOTUs = 2 then Exit;
  if (index > 0) and (index <= NoOfOTUs) then
  begin
    UndoOp := TTreeEditOperation.CreateOpRemoveOTU(GetLastTreeState, index);
    UndoStack.Push(UndoOp);
    newTree := UndoOp.UndoTreeData;
    newname := UndoOp.UndoNameList;
    newprivatename := UndoOp.UndoPrivateNameList;
    ResetTree(newtree, newname, newprivatename);
  end;
end;

procedure TTreeEditBox.InsertOTU(nodeindex: integer; name: AnsiString);
var
  newtree: TTreeData = nil;
  newname: TStringList = nil;
  newprivatename: TStringList = nil;
  UndoOp: TTreeEditOperation = nil;
  NewIndex: Integer;
begin
  if (nodeindex > 0) and (nodeindex <= NoOfNodes) then
  begin
    UndoOp := TTreeEditOperation.CreateOpAddOTU(GetLastTreeState, nodeindex, name);
    UndoStack.Push(UndoOp);
    newTree := UndoOp.UndoTreeData;
    newname := UndoOp.UndoNameList;
    newprivatename := UndoOp.UndoPrivateNameList;
    ResetTree(newtree, newname, newprivatename);
    NewIndex := Node[nodeindex].anc.Des2.Index;
    FocusOnNode(NewIndex);
  end;
end;

procedure TTreeEditBox.Undo;
var
  i : integer = -1;
  UndoOp: TTreeEditOperation = nil;
  LastIndex: integer = 0;
begin
  if UndoStack.Count <= 1 then
    Exit;
  { remove the last operation from the top of the stack}
  UndoOp := TTreeEditOperation(UndoStack.Pop);
  LastIndex := UndoOp.UndoIndex;
  UndoOp.Free; // until we decide to implement redo, we can discard previous tree state

  { grab the operation on the top of the stack we want}
  UndoOp := TTreeEditOperation(UndoStack.Peek);
  with UndoOp do
  begin
    if NoOfOTUs = UndoTreeData.NoOfOTUs then
    begin
      for i := 1 to NoOfOTUs - 1 do
      begin
        Node[NoOfOTUs+i].des1 := Node[UndoTreeData[i-1].des1+1];
        Node[NoOfOTUs+i].des2 := Node[UndoTreeData[i-1].des2+1];
        Node[UndoTreeData[i-1].des1+1].anc := Node[NoOfOTUs+i];
        Node[UndoTreeData[i-1].des2+1].anc := Node[NoOfOTUs+i];
      end;

      for i := 1 to NoOfOTUs do
      begin
        Node[i].name := UndoNameList[i-1];
        Node[i].PrivateName := UndoPrivateNameList[i-1];
      end;

      for i := NoOfNodes downto NoOfOTUs + 1 do
        if Node[i].anc = nil then
        begin
          Root := Node[i];
          Break;
        end;
      if (UndoTreeData.BLenArray <> nil) then
      begin
        for i := 0 to NoOfNodes-1 do
          Node[i+ 1].branch.length := UndoTreeData.BLenArray[i];
      end;
    end
    else
    begin
      ResetTree(UndoTreeData, UndoNameList, UndoPrivateNameList);
    end;
    SetMaxLength(Root);  // update the MaxLen variables after branch length changes
  end;

  if LastIndex > 0 then
     FocusOnBranch(LastIndex);

  Refresh;
end;

function TTreeEditBox.CanUndo: Boolean;
begin
  result := (UndoStack.Count > 1);
end;

procedure TTreeEditBox.FlipCluster;
begin
  inherited;
  UndoStack.Push(TTreeEditOperation.CreateOpTopologyChanged(GetLastTreeState, uFlipCluster));
end;

procedure TTreeEditBox.FlipAllCluster;
begin
  inherited;
  UndoStack.Push(TTreeEditOperation.CreateOpTopologyChanged(GetLastTreeState, uFlipAllCluster));
end;

procedure TTreeEditBox.MakeRootOnMidPoint;
begin
  inherited;
  UndoStack.Push(TTreeEditOperation.CreateOpTopologyChanged(GetLastTreeState, uMakeRootOnMidPoint));
end;

procedure TTreeEditBox.MakeRootByOutgroup(OutgroupOnBottom: Boolean);
begin
  inherited;
  UndoStack.Push(TTreeEditOperation.CreateOpTopologyChanged(GetLastTreeState, uMakeRootByOutgroup));
end;

procedure TTreeEditBox.MakeRootOnBranch;
begin
  if not EditEnabled then
  begin
    inherited;
    UndoStack.Push(TTreeEditOperation.CreateOpTopologyChanged(GetLastTreeState, uMakeRootOnBranch));
    Exit;
  end;

  if FocusedIndex = 0 then Exit;
  if Node[FocusedIndex] = Root then Exit;
  if Node[FocusedIndex].Anc <> Root then
  begin
    MoveRoot(Node[FocusedIndex], false);
    SetClusterSize(Root);
    SetClusterWidth(Root);
  end;
  if Node[FocusedIndex] = Node[FocusedIndex].anc.des1 then
    SwapNode(Root.des1, Root.des2);
  UndoStack.Push(TTreeEditOperation.CreateOpTopologyChanged(GetLastTreeState, uMakeRootOnBranch));
  Refresh;
end;

procedure TTreeEditBox.ResetMem;
begin
  inherited;
end;

function TTreeEditBox.ResetTree(tree: TTreeData; name: TStringList; privateName: TStringList):boolean;
var
  i : integer;
begin
  if Assigned(EditBox) then
    TEdit(EditBox).Hide;
  try
    if NoOfOTUs > 0 then ResetMem;

    FNoOfOTUs := Tree.NoOfOTUs;
    InitMem;

    for i := 1 to NoOfNodes do begin
      Node[i].anc := nil;
      Node[i].compressed := false;
      Node[i].autoCompressed := False;
      Node[i].hidden := false;
      Node[i].flag := false;
    end;
    for i := 1 to NoOfOTUs-1 do
    begin
      Node[NoOfOTUs+i].des1 := Node[tree[i-1].des1+1];
      Node[NoOfOTUs+i].des2 := Node[tree[i-1].des2+1];
      Node[tree[i-1].des1+1].anc := Node[NoOfOTUs+i];
      Node[tree[i-1].des2+1].anc := Node[NoOfOTUs+i];
      Node[NoOfOTUs+i].name := '';
      Node[NoOfOTUs+i].PrivateName := '';
    end;
    for i := NoOfNodes downto NoOfOTUs+1 do
      if Node[i].anc = nil then
      begin
        Root := Node[i];
        Break;
      end;
    if tree.BLenArray <> nil then
    begin
      for i := 1 to NoOfNodes-1 do
        Node[i].branch.length := Tree.BLenArray[i-1];
    end;

	{ remove NameLists, no longer in use }
    for i := 1 to NoOfOTUs do
    begin
        Node[i].name := name[i-1];
        Node[i].oriname := Node[i].name;
        Node[i].PrivateName := privateName[i-1];
    end;

    InitTree;
    Result := true;
  except
    ResetMem;
    Result := false;
  end;
end;

function TTreeEditBox.SetTreeData(isEditTopologyMode: Boolean; tree: TTreeData; name: TStringList; privatename: TStringList; IsRooted: Boolean = False):boolean;
var
  aOperation: TTreeEditOperation = nil;
begin
  Result := False;
  if tree.NoOfOTUs = 0 then Exit;

  FSource := 'TreeData';
  FisSE := False;
  FisStats := False;
  FisRooted := IsRooted;
  FisBranchFixed := true;
  FisValue := False;
  SetTopologyOnly(True);

  tree.isBLen := True; // All TreeEditBox trees must have branch lengths allocated to enable editing
  FisRooted := False;
  tree.RootIndex;

  Result := ResetTree(tree, name, privatename);
  if Result then
  begin
    ClearUndoStack;
    aOperation := TTreeEditOperation.CreateInitialTreeUndoOp(self, tree, name, privatename);
    if not isEditTopologyMode then
      aOperation.MarkUndefinedPrivateNames;
    UndoStack.Push(aOperation);
  end;
end;

function TTreeEditBox.GetIsBranchLength: Boolean;
begin
  Result := True;
end;

function TTreeEditBox.LargestWidthOfOTUNames: Integer;
var
  W, i, TempW: integer;
  BM: TBitmap;
  OTUFont, BoldOTUFont: TFont;
begin
  W := 0;
  try
    OTUFont := OTU_Font;
    BoldOTUFont := TFont.Create;
    BoldOTUFont.Assign(OTUFont);
    BoldOTUFont.Style := BoldOTUFont.Style + [fsBold];

    BM := TBitmap.Create;

    for i:=1 to NoOfOTUs do
    begin
      BM.Canvas.Font := BoldOTUFont;
      TempW := BM.Canvas.TextWidth(' ' + OTUPrivateName[i]);
      BM.Canvas.Font := OTUFont;
      TempW := TempW + BM.Canvas.TextWidth(' ' + OTUName[i]);
      if TempW > W then W := TempW;
    end;
    Result := W;
  finally
    if Assigned(BoldOTUFont) then
      BoldOTUFont.Free;
    if Assigned(BM) then
      BM.Free;
  end;
end;

function  TTreeEditBox.MaxWidthOTUNames: Integer;
var
  i, MaxWidth: Integer;
begin
  MaxWidth := 0;
  for i:=1 to NoOfOTUs do
  begin
    if Length(OTUName[i]) > MaxWidth then
      MaxWidth := Length(OTUName[i]);
  end;
  result := MaxWidth;
end;

procedure TTreeEditBox.SetOTUPrivateName(index: integer; name: AnsiString; Speedy: Boolean);
var
  UndoOp: TTreeEditOperation = nil;
begin
  if Node[index].PrivateName = name then { to prevent the undo stack having duplicates}
    Exit;
  if (index > 0) and (index <= NoOfOTUs) then
  begin
    if not Speedy then
    begin
      UndoOp := TTreeEditOperation.CreateOpSetOTUPrivateName(GetLastTreeState, index, name);
      UndoStack.Push(UndoOp);
    end;
    Node[index].privateName := name;
  end;
  if not Speedy then
    Width := TreeWidth + (GetSystemMetrics(SM_CXVSCROLL) + LargestWidthOfOTUNames);
end;

procedure TTreeEditBox.SetOTUName(index: integer; name: AnsiString);
var
  UndoOp: TTreeEditOperation = nil;
begin
  if Node[index].oriname = name then { to prevent duplicates on the stack}
    Exit;
  if (index > 0) and (index <= NoOfOTUs) then
  begin
    UndoOp := TTreeEditOperation.CreateOpSetOTUName(GetLastTreeState, index, name);
    UndoStack.Push(UndoOp);

    inherited;
    if Node[index].oriname = '' then
      Node[index].oriname := name;
  end;
end;

procedure TTreeEditBox.SetBranchLength(index: integer; branchLength: Double);
begin
  inherited;
  UndoStack.Push(TTreeEditOperation.CreateOpTopologyChanged(GetLastTreeState, uEditBranchLength))
end;

function TTreeEditBox.HeightAsDivergenceTime(NodeId: Integer): double;
begin
  { in our system, we don't have a single dedicated implementation for node divergence times.
    instead, we have a function reference FDivTimeFunc which we assign different functions depending on the scenario.
    For TTreeEditBox, we have this function which simply retrieves node height.
    (Node height and divergence times refer to the same concept and the terms are sometimes used interchangably.) }
  Result := Node[NodeId].height;
end;

function TTreeEditBox.GetNewickTree: AnsiString;
var
  treelist: TTreeList;
  tree: TTreeData;
  i: integer;
  useBranchLength: Boolean;
begin
  useBranchLength := IsBranchLength;
  treelist := TTreeList.Create;
  tree := TTreeData.Create(NoOfOTUs,useBranchLength,false,false);
  GetTreeData(tree);
  treelist.Add(tree);
  { let the output Newick be rooted\unrooted depending on what the user's chosen mode }
  treeList.isRooted:=IsRooted;
  for i := 0 to NoOfOTUs-1 do
    treelist.OTUName[i] := OTUName[i+1];
  result := treelist.OutputNewickTree(0,useBranchLength,false, 0);
  treelist.Clear;
  treelist.Free;
end;

function TTreeEditBox.GetNewickPrivateTree: AnsiString;
var
  treelist: TTreeList;
  tree: TTreeData;
  i: integer;
  useBranchLength: Boolean;
begin
  useBranchLength := IsBranchLength;
  treelist := TTreeList.Create;
  tree := TTreeData.Create(NoOfOTUs,useBranchLength,false,false);
  GetTreeData(tree);
  treelist.Add(tree);
  for i := 0 to NoOfOTUs-1 do
    treelist.OTUName[i] := OTUPrivateName[i+1];
  result := treelist.OutputNewickTree(0,useBranchLength,false, 0);

  treelist.Clear;
  treelist.Free;
end;

procedure Register;
begin
  RegisterComponents('Tamura Tools', [TTreeBox, TTreeEditBox]);
end;

function TTreeBox.GetCodonPositions: AnsiString;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to Information.Count - 1 do
    if Pos('Codon Positions',Information[i]) > 0 then
    begin
      Result := Copy(Information[i],Pos(':',Information[i])+2,255);
    end;
end;

function TTreeBox.GetLabelledSites: AnsiString;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to Information.Count - 1 do
    if Pos('Labelled sites',Information[i]) > 0 then
    begin
      Result := Copy(Information[i],Pos(':',Information[i])+2,255);
    end;
end;

// Too much cut and paste!! make a function to fetch by key to save some space here - Joel
function TTreeBox.GetTreeCI: AnsiString;
var
  i, beginIdx : Integer;
begin
  Result := '';
  for i := 0 to Information.Count - 1 do
    if Pos('CI =',Information[i]) > 0 then
    begin
      beginIdx := Pos('=',Information[i])+2;
      Result := Copy(Information[i],beginIdx,Pos('(',Information[i])-beginIdx);
      Break;
    end;
end;

function TTreeBox.GetTreeCI_infosites: AnsiString;
var
  i, beginIdx : Integer;
begin
  Result := '';
  for i := 0 to Information.Count - 1 do
    if Pos('iCI =',Information[i]) > 0 then
    begin
      beginIdx := Pos('=',Information[i])+2;
      Result := Copy(Information[i],beginIdx,Pos('(',Information[i])-beginIdx);
      Break;
    end;
end;

procedure TTreeBox.GetTreeData(tree: TTreeData; UseTimeFactor: Boolean=False);
begin
  SaveTreeData(TreeIndex);
  inherited GetTreeData(tree, UseTimeFactor);
end;

procedure TTreeBox.AssignTreeListInternalNodeLabelFunc(var AFunc: TGetInternalNodeLabelFunc);
begin
  AFunc := TreeList[TreeIndex - 1].GetInternalNodeLbl;
end;

function TTreeBox.GetTreeRCI: AnsiString;
var
  i, beginIdx : Integer;
begin
  Result := '';
  for i := 0 to Information.Count - 1 do
    if Pos('RCI =',Information[i]) > 0 then
    begin
      beginIdx := Pos('=',Information[i])+2;
      Result := Copy(Information[i],beginIdx,Pos('(',Information[i])-beginIdx);
      Break;
    end;
end;

function TTreeBox.GetTreeRCI_infosites: AnsiString;
var
  i, beginIdx : Integer;
begin
  Result := '';
  for i := 0 to Information.Count - 1 do
    if Pos('iRCI =',Information[i]) > 0 then
    begin
      beginIdx := Pos('=',Information[i])+2;
      Result := Copy(Information[i],beginIdx,Pos('(',Information[i])-beginIdx);
      Break;
    end;
end;

function TTreeBox.GetTreeRI: AnsiString;
var
  i, beginIdx : Integer;
begin
  Result := '';
  for i := 0 to Information.Count - 1 do
    if Pos('RI =',Information[i]) > 0 then
    begin
      beginIdx := Pos('=',Information[i])+2;
      Result := Copy(Information[i],beginIdx,Pos('(',Information[i])-beginIdx);
      Break;
    end;
end;

function TTreeBox.GetTreeRI_infosites: AnsiString;
var
  i, beginIdx : Integer;
begin
  Result := '';
  for i := 0 to Information.Count - 1 do
    if Pos('iRI =',Information[i]) > 0 then
    begin
      beginIdx := Pos('=',Information[i])+2;
      Result := Copy(Information[i],beginIdx,Pos('(',Information[i])-beginIdx);
      Break;
    end;
end;

function TTreeBox.GetSiteIndexForCaption: AnsiString;
begin
  result := Format('%.0n', [(SiteIndex + 1)*1.0]);
end;

function TTreeBox.GetSpeciesNamesList: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(TreeList.SpeciesNamesList);
end;

function TTreeBox.LargestWidthOfOTUNames: Integer;
var
  i: integer;
  tempFont: TFont;
  longest: String = '';
  len: Integer = 0;
  len2: Integer = 0;
begin
  Result := 0;
  tempFont := Canvas.Font;
  Canvas.Font := FOTU_Font;
  for i := 1 to NoOfOTUs do
  begin
    len := Length(OTUName[i]);
    if len > len2 then
    begin
      longest := OTUName[i];
      len2 := len;
    end;
  end;
  Result := Canvas.TextWidth(' ' + longest);
  Canvas.Font := tempFont;
end;

function TTreeCustomControl.SearchLabelledNodes(Query: String;
  Contains: Boolean; HighlightAll: Boolean; QueryChanged: Boolean;
  var focusedResultIndex: Integer): Integer;
var
  nodeInfoList: TNodeDisplayInfoList = nil;
  i: Integer = -1;
begin
  try
    nodeInfoList := GetInternalNodeDisplayInfoList(Root); { this list has nodes ordered the same as they appear in the tree, which can vary depending on rooting of the tree and user actions (e.g. swap subtree)}
    Assert(NodeInfoList.Count = (NoOfNodes - NoOfOTUs));
    Result := DoSearch(nodeInfoList, Query, Contains, HighlightAll, QueryChanged, focusedResultIndex);
  finally
    if Assigned(nodeInfoList) then
    begin
      for i := 0 to nodeInfoList.Count - 1 do
        nodeInfoList[i].Free;
      nodeInfoList.Free;
    end;
  end;
end;

function TTreeCustomControl.SearchGeneDup(var Index: Integer; var NodeId: Integer; const SearchDir: TSearchDirection; const IsDup: Boolean): Boolean;
var
  i: Integer;
  Start: Integer;
begin
  Result := False;

  { find a starting point}
  if (FocusedIndex > 0) and (FocusedIndex < NoOfNodes) then { we are somewhere in the tree already}
  begin
    if SearchDir = sdForward then
      Start := FocusedIndex + 1
    else
      Start := FocusedIndex - 1;
  end
  else
  begin
    if SearchDir = sdForward then
      Start := NoOfOtus + 1 { start at the first internal node}
    else
    if FocusedIndex = NoOfNodes then
      Start := NoOfNodes - 1
     else
      Start := NoOfNodes; { start under the root}
  end;

  ClearFocus;
  { do the SearchTipNames}
  if SearchDir = sdForward then
  begin
    for i:=Start to NoOfNodes do
    begin
      if IsDup then
      begin
        if Node[i].IsGeneDuplication then
        begin
          Result := True;
          FocusOnNode(i);
          Index := GeneDupFocusedIndex(IsDup);
          NodeId := i;
          Exit;
        end;
      end
      else
      begin
        if Node[i].IsSpeciationEvent then
        begin
          Result := True;
          FocusOnNode(i);
          Index := GeneDupFocusedIndex(IsDup);
          NodeId := i;
          Exit;
        end;
      end;
    end;
  end
  else
  begin
    for i := Start downto NoOfOtus + 1 do
    begin
      if IsDup then
      begin
        if Node[i].IsGeneDuplication then
        begin
          Result := True;
          FocusOnNode(i);
          Index := GeneDupFocusedIndex(IsDup);
          NodeId := i;
          Exit;
        end;
      end
      else
      begin
        if Node[i].IsSpeciationEvent then
        begin
          Result := True;
          FocusOnNode(i);
          Index := GeneDupFocusedIndex(IsDup);
          NodeId := i;
          Exit;
        end;
      end;
    end;
  end;

  { if we didn't find anything and didn't SearchTipNames all nodes, then try the rest}
  if ((not Result) and (SearchDir = sdForward) and (Start <> 1)) or ((not Result) and (SearchDir = sdBackward) and (Start <> NoOfNodes)) then
  begin
    FocusOnNode(0);
    Result := SearchGeneDup(Index, NodeId, SearchDir, IsDup);
  end;
  Refresh;
end;

function TTreeCustomControl.GeneDupFocusedIndex(IsDup: Boolean = True): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := FocusedIndex downto NoOfOtus + 1 do
  begin
    if IsDup then
    begin
      if Node[i].IsGeneDuplication then
        inc(Result);
    end
    else
    begin
      if Node[i].IsSpeciationEvent then
        inc(Result);
    end;
  end;
end;

function TTreeCustomControl.GetAncestorName(i: integer): AnsiString;
begin
  Result := GetOTUName(Node[i].anc.index);
  if Result = EmptyStr then  // When there is no name, return the direct decendant numbers as a method of identifying the node.
    Result := GetCoords(Node[i].anc.index);
end;

function TTreeEditBox.FocusOnClosest(cursorPos: TPoint; searchRadius: Integer): Boolean;
var
  lastPt, focusPt: TPoint;
  x, y: Integer;
  radius, angle: integer;
begin
  Result := False;
  lastPt := Point(-1, -1);
  for radius := 0 to searchRadius do
  begin
    for angle := 0 to 360 do
    begin
      x := Floor((radius*cos(angle)));
      y := Floor((radius*sin(angle)));
      if (x <> lastPt.X) or (y <> lastPt.Y) then
      begin
        lastPt := Point(x, y);
        focusPt := Point(cursorPos.x + x, cursorPos.y + y);
        if FocusOnPoint(focusPt) then
        begin
          result := true;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TTreeEditBox.ClearUndoStack;
begin
  if (UndoStack <> nil) then
    while UndoStack.Count > 0 do
      TTreeEditOperation(UndoStack.Pop).Free;
end;

{$IFDEF DEBUG}
function TTreeEditBox.UndoStackDebugStrings: TStringList;
var
  i: Integer = -1;
  aArray: TTreeEditOperationArray = nil;
begin
  Result := TStringList.Create;
  if (not Assigned(UndoStack)) or (UndoStack.Count = 0) then Exit;

  SetLength(aArray, UndoStack.Count);
  i := 0;
  while UndoStack.Count > 0 do
  begin
    aArray[i] := UndoStack.Pop;
    Result.Add(aArray[i].DebugString);
    inc(i)
  end;

  for i := High(aArray) downto Low(aArray) do
    UndoStack.Push(aArray[i]);
end;
{$ENDIF}

function TTreeCustomControl.GetAncestorNodeNumber(i: integer): integer;
begin
  Result  := -1;
  if (i <> 0) and (Node[i] <> nil) and (Node[i].anc <> nil) then
    result := Node[i].anc.index;
end;

{ TTreeEditOperation }

{ When we create a new TTreeEditBox tree, its initial state is stored in a TTreeEditOperation on the undo stack.
  Every operation adds a new TTreeEditOperation (TTEO) to the stack.  
  
  TTEO stores tree state in UndoTreeData, UndoNameList, and UndoPrivateNameList.
  
  To minimize storing redundant data, we use TreeDataChanged, NameListChanged, and PrivateNameListChanged
  to track whether the operation altered these tree state variables. 
  
  i.e. if an operation does not change the name list, then the new TTEO's UndoNameList will simply point to
  the UndoNameList of the previous TTEO and NameListChanged is set to False. }

constructor TTreeEditOperation.CreateInitialTreeUndoOp(aTreeBox: TTreeCustomControl; aTree: TTreeData; names: TStringList; privateNames: TStringList);
var
  i: Integer = -1;
begin
  { The data variables (UndoTreeData, UndoNameList, and UndoPrivateNameList)
    point to those of the previous undo objects if they are unchanged. To avoid
    double frees, whether they were changed is tracked using the booleans (TreeDataChanged,
    NameListChanged, and PrivateNameListChanged). The data variables should only be freed
    if they were changed. State change variables are set to True for the initial undo object
    so that initial data varaiables are changed. }

  UndoIndex := 0;
  OperationType := uNone;
  TreeBox := aTreeBox;
  UndoTreeData := TTreeData.Create(aTree.NoOfOTUs, true, false, true);
  UndoTreeData.Assign(aTree);
  UndoNameList := TStringList.Create;
  UndoNameList.Assign(names);
  UndoPrivateNameList := TStringList.Create;
  UndoPrivateNameList.Assign(privateNames);
  TreeDataChanged := True;
  NameListChanged := True;
  PrivateNameListChanged := True;
end;

constructor TTreeEditOperation.CopyLastUndoState(var EditOp: TTreeEditOperation; opType: UndoAction);
begin
  inherited Create;
  OperationType := opType;
  TreeBox := EditOp.TreeBox;

  UndoTreeData := EditOp.UndoTreeData;
  UndoNameList := EditOp.UndoNameList;
  UndoPrivateNameList := EditOp.UndoPrivateNameList;
  UndoIndex := EditOp.UndoIndex;

  TreeDataChanged := False;
  NameListChanged := False;
  PrivateNameListChanged := False;

  { The tree data should generally not be changed except by TTreeEditOperations.
    These lines are insurance against unanticipated changes by keeping
    the undo data synchronized with any changes to the tree data.
    During testing, they should be disabled to help catch outside changes. }
  //TreeBox.GetTreeData(UndoTreeData);
  //TreeDataChanged := True;
end;

constructor TTreeEditOperation.CreateOpTopologyChanged(EditOp: TTreeEditOperation;
  opType: UndoAction);
begin
  { this is used for any operation that modifies the tree topology, but also branch lengths }
  CopyLastUndoState(EditOp, opType);
  UndoTreeData := TTreeData.Create(TreeBox.NoOfOTUs, true, false, false);

  UndoIndex := TreeBox.FocusedIndex;

  TreeBox.GetTreeData(UndoTreeData);
  TreeDataChanged := True;
end;

constructor TTreeEditOperation.CreateOpAddOTU(EditOp: TTreeEditOperation; node: integer; name: AnsiString);
begin
  CopyLastUndoState(EditOp, uInsertOTU);

  // create new name lists since they are modified from the previous state
  UndoNameList := TStringList.Create;
  UndoNameList.Assign(EditOp.UndoNameList);
  UndoPrivateNameList := TStringList.Create;
  UndoPrivateNameList.Assign(EditOp.UndoPrivateNameList);

  UndoIndex := node;
  
  // as well as tree data since the topology will be altered
  UndoTreeData := TTreeData.Create(TreeBox.NoOfOTUs, true, false, false);
  TreeBox.GetTreeData(UndoTreeData);
  UndoTreeData.AddOTU(node-1);

  UndoNameList.Add(name);
  UndoPrivateNameList.Add('');

  TreeDataChanged := True;
  NameListChanged := True;
  PrivateNameListChanged := True;
end;

constructor TTreeEditOperation.CreateOpRemoveOTU(EditOp: TTreeEditOperation;
  index: integer);
begin
  CopyLastUndoState(EditOp, uRemoveOTU);

  // create new name lists since they are modified from the previous state
  UndoNameList := TStringList.Create;
  UndoNameList.Assign(EditOp.UndoNameList);
  UndoPrivateNameList := TStringList.Create;
  UndoPrivateNameList.Assign(EditOp.UndoPrivateNameList);

  UndoIndex := TreeBox.FocusedIndex;
  
  // as well as tree data since the topology will be altered
  UndoTreeData := TTreeData.Create(TreeBox.NoOfOTUs, true, false, false);
  TreeBox.GetTreeData(UndoTreeData);
  UndoTreeData.RemoveOTU(index-1, False, EmptyStr);

  UndoNameList.Delete(index-1);
  UndoPrivateNameList.Delete(index-1);

  TreeDataChanged := True;
  NameListChanged := True;
  PrivateNameListChanged := True;
end;

constructor TTreeEditOperation.CreateOpSetOTUName(EditOp: TTreeEditOperation;
  index: integer; name: AnsiString);
begin
  CopyLastUndoState(EditOp, uSetOTUName);
  // create a fresh UndoNameList since it will be different from the previous state
  UndoNameList := TStringList.Create;
  UndoNameList.Assign(EditOp.UndoNameList);
  UndoIndex := index;

  UndoNameList[index-1] := name;

  NameListChanged := True;
end;

constructor TTreeEditOperation.CreateOpSetOTUPrivateName(
  EditOp: TTreeEditOperation; index: integer; name: AnsiString);
begin  
  CopyLastUndoState(EditOp, uSetOTUPrivateName);
  UndoPrivateNameList := TStringList.Create;
  // create a fresh UndoPrivateNameList since it will be different from the previous state
  UndoPrivateNameList.Assign(EditOp.UndoPrivateNameList);
  UndoIndex := index;

  UndoPrivateNameList[index-1] := name;

  PrivateNameListChanged := True;
end;

destructor TTreeEditOperation.Destroy;
begin
  // only free the object if they are marked as different from the previous EditOperation
  // otherwise, this causes a double free
  if (NameListChanged) then
    UndoNameList.Free;

  if (PrivateNameListChanged) then
    UndoPrivateNameList.Free;

  if TreeDataChanged and (UndoTreeData <> nil) then
    UndoTreeData.Free;
  inherited;
end;

function TTreeEditOperation.DebugString: String;
var
  aAction: String = '';
begin
  case OperationType of
    uNone: aAction := 'no-action';
    uSetOTUName: aAction := 'set-otu-name';
    uSetOTUPrivateName: aAction := 'set-private-name';
    uMoveNode: aAction := 'move-node';
    uRemoveOTU: aAction := 'remove-otu';
    uInsertOTU: aAction := 'insert-otu';
    uFlipCluster: aAction := 'flip-cluster';
    uFlipAllCluster: aAction := 'flip-all-cluster';
    uMakeRootOnMidPoint: aAction := 'root-midpoint';
    uMakeRootByOutgroup: aAction := 'root-outgroup';
    uMakeRootOnBranch: aAction := 'root-branch';
    uEditBranchLength: aAction := 'edit-branch-length';
  end;

  Result := Format('%d %s td=%s nl=%s pn=%s', [UndoIndex, aAction, BoolToStr(TreeDataChanged, True), BoolToStr(NameListChanged, True), BoolToStr(PrivateNameListChanged, True)]);
end;

procedure TTreeEditOperation.UpdateActiveNamesMap(var activeNames: TStringList);
var
  i: Integer = -1;
  index: Integer = -1;
begin
  Assert(UndoNameList.Count = UndoPrivateNameList.Count);
  if activeNames.Count > 0 then
    for i := 0 to activeNames.Count - 1 do
    begin
      index := UndoPrivateNameList.IndexOf(activeNames.Names[i]);
      if index >= 0 then
      begin
        if UndoNameList[index] = UndefinedStr then
          activeNames[i] := Format('%s=', [UndoPrivateNameList[index]])
        else
          activeNames[i] := Format('%s=%s', [UndoPrivateNameList[index], UndoNameList[index]]);
      end
      else
        activeNames[i] := Format('%s=', [activeNames.Names[i]]);
    end;
end;

procedure TTreeEditOperation.MarkUndefinedPrivateNames;
var
  i: Integer = -1;
begin
  if UndoPrivateNameList.Count > 0 then
    for i := 0 to UndoPrivateNameList.Count - 1 do
      if UndoPrivateNameList[i] = EmptyStr then
        UndoPrivateNameList[i] := UndefinedStr;
end;

{$ENDIF VISUAL_BUILD}
end.
