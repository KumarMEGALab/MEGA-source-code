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

unit MAlignGrid;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntF, LCLType, Classes, Controls, StdCtrls, SysUtils, Forms, Graphics,
  Grids, Clipbrd, Dialogs, megautils, MD_Sequences, MRuntimeProgressDlg,
  MegaConsts, typinfo, MV_Columns, syncobjs;

type
  TColumnEditedNotify = procedure(SiteIndex: Int64) of object;
  TFixedRowStyle = (frConserved, frStrictConsensus, frMajorityConsensus);
  TUndoProc = (upDelete, upInsert, upSeqMove, upMark, upAddSeq, upDelSeq, upGrpName, upSeqName, upReplace);

  { TUpdateHeaderStringThread }

  TUpdateHeaderStringThread = class(TThread)
    private
      FLog: TStringList;
      FIsCancelled: Boolean;
      FIsSuccess: Boolean;
      function GetLogText: String;
    protected
      FSequences: TSequenceList;
      FHeaderString: AnsiString;
      FFixedRowStyle: TFixedRowStyle;
      FStartSite: Int64;
      FProgressString: AnsiString;
      procedure DoExecute;
      procedure Execute; override;
      procedure DoCheckAbort;
      procedure DoUpdateStatus;
    public
      UpdateStatusProc: TRunStatusProc;
      CheckAbort: TCheckAbortFunc;
      constructor Create(HeaderStr: AnsiString; seqData: TSequenceList; RowStyle: TFixedRowStyle; startSite: Int64);
      destructor Destroy; override;
      property HeaderString: AnsiString read FHeaderString write FHeaderString;
      property IsSuccess: Boolean read FIsSuccess;
      property IsCancelled: Boolean read FIsCancelled;
      property LogText: String read GetLogText;
  end;

  TUndoInfo = class
  public
    Proc: TUndoProc;
    x0,y0,x1,y1: integer;
    Pos: TPoint;
    Ori: TPoint;
    Sel: TGridRect;
    data: TStringList;
    goNext: boolean;

    constructor Create;
    Destructor Destroy; override;
  end;

  { TUndoInfoList }

  TUndoInfoList = class(TList)
  private
    function GetItems(index: integer):TUndoInfo;
    procedure SetItems(index: integer; value: TUndoInfo);
  public
    ColumnEditedNotify: TColumnEditedNotify;
    function Add(Item: Pointer): Integer;
    constructor Create(colEditedNotify: TColumnEditedNotify);
    property Items[Index: integer]: TUndoInfo read GetItems write SetItems; default;

    procedure Delete(Index: integer);
    procedure Clear; override;

    destructor Destroy; override;
  end;

  { TAlignGrid }

  TAlignGrid = class(TCustomDrawGrid)
  private
    FSkipHeaderUpdates: Boolean;
    EditBox: TEdit;
    FInstanceName: String;
    SeqList: TSequenceList;
    DNASeqs: TStringList;
    UndoInfoList: TUndoInfoList;

    FShowColor: boolean;
    FShowBGColor: boolean;
    FEditEnabled: boolean;
    FAlignEnabled: boolean;
    FForceAlignment: boolean;
    FModified: boolean;
    Translated: boolean;

    FSearchBox: AnsiString;
    FHighlightSearchBox: boolean;

    DNASites, AASites: Set of AnsiChar;
    Ori: TPoint;

    FShowFixedRow: boolean;
    FShowFixedCol: boolean;
    FFixedRowStyle: TFixedRowStyle;

    FArrow : Array[0..2] of TPoint;

    FMarkConsensus: boolean;
    FConsensusValue: integer;

    FFixSequenceOrder: boolean;
    FBaseCounts: T2dArrayOfLongInt;
    ColWidthSet: Boolean;
    function SiteIndexToColumn(site: Integer): Integer;
    procedure EditBoxOnExit(Sender : TObject);
    procedure EditBoxOnGroupExit(Sender : TObject);
    procedure EditBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditBoxOnGroupKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ChangeSeqName(index: integer; newname: AnsiString);
    procedure ChangeGrpName(index: integer; newname: AnsiString);

    procedure ResetColCount;
    procedure ResetRowCount;
    procedure ResetColWidth;
    procedure ResetRowHeight;
    procedure Initialize;

    procedure SetCursorCenter;
    procedure SetHeaderString(AValue: AnsiString);

    procedure SetTitle(value: AnsiString);
    function GetTitle: AnsiString;
    procedure SetIsDNA(value: boolean);
    function GetIsDNA: boolean;
    procedure SetIsProteinCoding(value: boolean);
    function GetIsProteinCoding: boolean;
    function GetNoOfSeqs: integer;
    function GetMaxNoOfSites: integer;
    function GetMinNoOfSites: integer;
    function GetNoOfSites(index: integer): integer;
    function GetSequence(index: integer):TSequence;

    procedure GetRangeOfSelectedSites(var startSite, endSite: integer);
    procedure GetRangeOfSelectedSeqs(var startSeq, endSeq: integer);
    procedure InsertBlock(x, y: integer; block: TStringList);
    procedure DeleteBlock(x0,y0,x1,y1: integer);
    procedure InsertBasesAt(SeqIndex, SiteIndex: integer; Bases: AnsiString);
    procedure DeleteBasesAt(SeqIndex, SiteIndex, Count: integer);
    procedure InsertSequenceAT(index: integer; seq: TSequence);
    procedure DeleteSequenceAT(index: integer);
    procedure SelectSequenceAt(index: integer);
    procedure SelectSiteAt(index: integer);

    procedure MarkSiteAt(SeqIndex, SiteIndex: integer);
    procedure SetSearchBox(seq: AnsiString);

    procedure SetShowFixedRow(value: boolean);
    procedure SetShowFixedCol(value: boolean);

    function GetAA(Codon: array of AnsiChar):AnsiChar;
    function MatchSearchBox(box: AnsiString):boolean;
    function SiteInSearchBox(seqindex, siteindex: integer):boolean;
    function FindFrom(seqindex, siteindex: integer):integer;
    function FindBackFrom(seqindex, siteindex: integer):integer;
    function FindFromWithGap(seqindex, siteindex: integer):integer;
    function FindBackFromWithGap(seqindex, siteindex: integer):integer;

    procedure ParseFASTA(sl: TStringList; seqs: TSequenceList; check: boolean);

    function CheckMEGASeqName(AName: AnsiString): integer;
    procedure FilterSeqName(seq: TSequence);
    function IsTaxaNameCol(aCol: LongInt): Boolean;
    function IsGroupNameCol(aCol: LongInt): Boolean;
    function CheckFixedColWidth(aCol: LongInt): Boolean;
    procedure UpdateConsensusSequence;
    function IsConsensusBase(aBase: AnsiChar; aCol: LongInt): Boolean;
    function IsSameNucleotide(base1, base2: Char): Boolean;
    procedure UpdateHeaderThreadDone(aThread: TObject);
  protected
    FUpdateHeaderThread: TUpdateHeaderStringThread;
    //FSelectionData: TGridRect;
    FHeaderString: AnsiString;
    FCodeTable: AnsiString;
    FCodeTableName: AnsiString;
    FHeaderStringInvalid: Boolean;
    FHeaderStringInvalidFrom: Integer;
    procedure MoveRow(FromIndex, ToIndex: Longint);
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure DrawTaxaNameCell(ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure DrawGroupNameCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure DrawHeaderCell(ACol: Longint; ARect: TRect; AState: TGridDrawState);
    procedure DrawAlignmentCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure DrawSortArrow(aCol: LongInt; aRect: TRect);
    procedure DrawFixedCellBorder(aCol, aRow: LongInt; aRect: TRect);
    procedure DrawDataCellBorder(aCol, aRow: LongInt; aRect: TRect);
    procedure DrawCurrentColumnBorder(aRect: TRect);
    procedure ColRowMoved(IsColumn: Boolean; FromIndex, ToIndex: Longint); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var KeyW: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure OnInvalidate;
    procedure InitFont;
    function RowIsSelected(aRow: Integer): Boolean;
    function RectsAreEqual(r1: TGridRect; r2: TGridRect): Boolean;

    //procedure WMHScroll(var message: TLMHScroll); message LM_HSCROLL;

  public
    SearchHighlightBgColor: TColor;
    SearchHighlightFontColor: TColor;
    ClickNotify: TNotifyEvent;
    Cols: TColList;
    procedure UpdateHeaderString(aCol: Int64);
    function IsFixedColumn(aCol: LongInt): Boolean;
    function DataTypeString: String;
    function IsVisibleCell(aCol, aRow: Integer): Boolean;
    procedure Invalidate; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState);
    property Title: AnsiString read GetTitle write SetTitle;
    property Sequence[index: integer]: TSequence read GetSequence;
    property NoOfSeqs:  integer read GetNoOfSeqs;
    property MaxNoOfSites: integer read GetMaxNoOfSites;
    property MinNoOfSites: integer read GetMinNoOfSites;
    property NoOfSites[index: integer]: integer read GetNoOfSites;
    property IsDNA: boolean read GetIsDNA write SetIsDNA;
    property IsProteinCoding: boolean read GetIsProteinCoding write SetIsProteinCoding;
    property Modified: boolean read FModified;
    property SearchBox: AnsiString read FSearchBox write SetSearchBox;
    property MarkConsensus: boolean read FMarkConsensus write FMarkConsensus;
    property ConsensusValue: integer read FConsensusValue write FConsensusValue;
    property FixSequenceOrder: boolean read FFixSequenceOrder write FFixSequenceOrder;

    function NumFixedCols: Integer;
    function NumFixedColsSelected: Integer;
    function Empty: boolean;
    function CopyEnabled: boolean;
    function DeleteEnabled: boolean;
    function PasteEnabled: boolean;
    function UndoEnabled: boolean;
    function DelGapEnabled: boolean;
    function Marked: boolean;
    function SequenceSelected: boolean;
    function SiteSelected: boolean;
    function BlockSelected: boolean;
    function AllSelected: Boolean;
    function Selected(ACol, ARow: integer): boolean;
    function GetNoOfSelectedSites: integer;
    function GetNoOfSelectedSeqs: integer;
    function SiteIndex(gap: boolean):integer;
    function HasGroupNames: Boolean;
    procedure ReplaceTaxaNames(aList: TStringList);
    procedure JumpToSite(index: integer; gap: boolean);

    procedure Clear;
    procedure ResetGrid;
    procedure ResetSize;
    procedure Select(startCol, startRow, endCol, endRow: integer);
    procedure SelectedBlock(var site0, seq0, site1, seq1: integer);
    procedure SelectBlock(site0, seq0, site1, seq1: integer);

    procedure SelectSequence;
    procedure SelectSite;
    procedure SelectAll;
    procedure ClearSelection;

    procedure InsertGaps;
    procedure DeleteGaps;
    procedure ToUpper;
    procedure Polish;
    procedure Pack;

    procedure MoveRight;
    procedure MoveLeft;
    procedure MoveSequenceUp;
    procedure MoveSequenceDown;

    procedure Insert(Bases: TStringList);
    procedure Replace(Bases: TStringList);
    procedure Delete;
    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure Undo;
    function Find: boolean;
    function FindNext: boolean;
    function FindPrev: boolean;
    function FindMark: boolean;

    procedure AppendBlankSequence;
    procedure AppendSequenceFromClipBoard;
    procedure AppendSequence(seq: TSequence);
    procedure AppendSequenceList(seqs: TSequenceList);
    procedure InsertBlankSequence;
    procedure InsertSequenceFromClipBoard;
    procedure InsertFASTASequenceFromClipBoard;
    procedure InsertSequence(seq: TSequence);
    procedure InsertSequenceList(seqs: TSequenceList);
    procedure DeleteSequence;
    procedure EditSeqName(SeqIndex: integer);
    procedure EditGrpName(SeqIndex: integer);
    function CheckSeqName(AName: AnsiString): integer;

    procedure MarkSite;
    procedure MarkSearchBox;
    procedure UnMarkSite;
    procedure AlignMakedSites;

    procedure Translate;
    procedure UnTranslate;
    procedure ReverseCompliment;
    procedure ReverseSelection;
    procedure ComplimentSelection;

    procedure SortByName(ASortOrder: TColumnsSortOrder);

    function LoadFromFASTAFile(filename: String): boolean;
    function InsertFile(filename: String): boolean;
    function InsertTextFile(filename: String): boolean;
    function InsertFASTAFile(filename: String): boolean;
    function InsertABIFile(filename: String): boolean;
    function InsertSCFFile(filename: String): boolean;

    procedure SetSeqData(data: TSequenceList);
    procedure TransferSeqData(data: TSequenceList);
    procedure GetSeqData(data: TSequenceList);
    procedure GetSelectedData(data: TSequenceList);
    procedure GetAllData(var data: TSequenceList);
    function CanAlignSelectedData: Boolean;
    function ReadFromFile(var f: File):boolean;
    function InsertFromFile(var f: File):boolean;
    procedure WriteToFile(var f: File; var UserAborted: Boolean; aProgress: TRuntimeProgress);
    procedure SaveToFASTAFile(filename: String);
    procedure SaveToMEGAFile(filename: String);
    procedure SaveToPAUPFile(filename: String);

    procedure CacheSelectionData(var aRect: TRect);
    procedure SetSelectionFromCachedData(const aRect: TRect);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HeaderString: AnsiString read FHeaderString write SetHeaderString;
    property InstanceName: String read FInstanceName write FInstanceName;
    property SkipHeaderUpdates: Boolean read FSkipHeaderUpdates write FSkipHeaderUpdates;
    property Font;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property ColCount;
    property Constraints;
    {$IFNDEF FPC}
    property Ctl3D;
    {$ENDIF}
    property DefaultColWidth;
    property DefaultRowHeight;
    property DefaultDrawing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property FixedRowStyle: TFixedRowStyle read FFixedRowStyle write FFixedRowStyle;
    property RowCount;

    property GridLineWidth;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    {$IFNDEF FPC}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowFixedRow: boolean read FShowFixedRow write SetShowFixedRow;
    property ShowFixedCol: boolean read FShowFixedCol write SetShowFixedCol;
    property ShowHint;
    property TabOrder;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;
    property OnClick;
    {$IFNDEF FPC}
    property OnColumnMoved;
    {$ENDIF}
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditMask;
    property OnGetEditText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

    property OnColRowMoved;

    property OnSelectCell;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;

    property EditEnabled: boolean read FEditEnabled write FEditEnabled;
    property AlignEnabled: boolean read FAlignEnabled write FAlignEnabled;
    property ForceAlignment: boolean read FForceAlignment write FForceAlignment;

    property ShowColor: boolean read FShowColor write FShowColor;
    property ShowBGColor: boolean read FShowBGColor write FShowBGColor;

    property CodeTableName : AnsiString read FCodeTableName write FCodeTableName;
    property CodeTable : AnsiString read FCodeTable write FCodeTable;

    property HighlightSearchBox: boolean read FHighlightSearchBox write FHighlightSearchBox;

  end;


procedure Register;

var
  HeaderCS: TCriticalSection;

implementation

uses
  RegExpr, MPleaseWait, math, mcolorcodes, dateutils, LCLProc, mstringbuilder;

procedure Register;
begin
  RegisterComponents('Tamura Tools', [TAlignGrid]);
end;

{ TUpdateHeaderStringThread }

function TUpdateHeaderStringThread.GetLogText: String;
begin
  if Assigned(FLog) then
    Result := FLog.Text;
end;

procedure TUpdateHeaderStringThread.DoExecute;
var
  IsConserved: Boolean = True;
  ch: AnsiChar = #0;
  i,j: integer;
  max: Integer = 0;
  imax: Integer = 0;
  count: array[1..21] of integer;
  site, seq: Integer;
  updateTime: TDateTime;
begin
  try
    HeaderCS.Acquire;
    updateTime := Now;
    case FFixedRowStyle of
      frConserved:
        begin
          if FSequences.Count >= 2 then
          begin
            for site := FStartSite to Length(FHeaderString) do
            begin
              IsConserved := true;
              for seq := 1 to FSequences.Count - 1 do
              begin
                if Terminated then Exit;
                if MillisecondsBetween(Now, updateTime) > 200 then
                begin
                  Synchronize(DoCheckAbort);
                  if FIsCancelled then
                    raise EAbort.Create('thread aborted');
                  FProgressString := Format('computing site conservation %d', [Round(site/Length(FHeaderString)*100)]);
                  Synchronize(DoUpdateStatus);
                  updateTime := Now;
                end;
                if Assigned(FSequences[0]) and Assigned(FSequences[seq]) then
                  if (site <= FSequences[0].NoOfSites) and (site <= FSequences[seq].NoOfSites) then
                    if UpCase(FSequences[seq].SeqData[site]) <> UpCase(FSequences[0].SeqData[site]) then
                    begin
                      IsConserved := false;
                      break;
                    end;
              end;
              if IsConserved then
                FHeaderString[site] :=  '*'
              else
                FHeaderString[site] := #0;
            end;
          end
          else if FSequences.Count = 1 then
          begin
            for site := FStartSite to Length(FHeaderString) do
              FHeaderString[site] := '*';
          end;
        end;
      frMajorityConsensus:
        if FSequences.Count > 0 then
        begin
          for site := FStartSite to Length(FHeaderString) do
          begin
            for i := 1 to 21 do
              count[i] := 0;
            if FSequences.IsDNA then
            begin
              for seq := 0 to FSequences.Count - 1 do
              begin
                if Terminated then Exit;
                if FSequences[seq].NoOfSites < site then
                  continue;
                case upcase(FSequences[seq].SeqData[site]) of
                  'A': inc(count[1], 4);
                  'T': inc(count[2], 4);
                  'U': inc(count[2], 4);
                  'C': inc(count[3], 4);
                  'G': inc(count[4], 4);
                  'R': begin
                         inc(count[1], 2);
                         inc(count[4], 2);
                       end;
                  'Y': begin
                         inc(count[2], 2);
                         inc(count[3], 2);
                       end;
                  'S': begin
                         inc(count[3], 2);
                         inc(count[4], 2);
                       end;
                  'W': begin
                         inc(count[1], 2);
                         inc(count[2], 2);
                       end;
                  'K': begin
                         inc(count[2], 2);
                         inc(count[4], 2);
                       end;
                  'M': begin
                         inc(count[1], 2);
                         inc(count[3], 2);
                       end;
                  'B': begin
                         inc(count[2], 1);
                         inc(count[3], 1);
                         inc(count[4], 1);
                       end;
                  'V': begin
                         inc(count[1], 1);
                         inc(count[3], 1);
                         inc(count[4], 1);
                       end;
                  'D': begin
                         inc(count[1], 1);
                         inc(count[2], 1);
                         inc(count[4], 1);
                       end;
                  'H': begin
                         inc(count[1], 1);
                         inc(count[2], 1);
                         inc(count[3], 1);
                       end;
                  '-': inc(count[5],4);
                end;
              end;
              for j := 1 to 5 do
                if count[j] > max then
                begin
                  max := count[j];
                  case j of
                    1: imax := 1;
                    2: imax := 2;
                    3: imax := 4;
                    4: imax := 8;
                    5: imax := 16;
                  end;
                end
                else if count[j] = max then
                  case j of
                    1: imax := imax or 1;
                    2: imax := imax or 2;
                    3: imax := imax or 4;
                    4: imax := imax or 8;
                  end;
              case imax of
                 0: ch := 'N';
                 1: ch := 'A';
                 2: ch := 'T';
                 3: ch := 'W';
                 4: ch := 'C';
                 5: ch := 'M';
                 6: ch := 'Y';
                 7: ch := 'H';
                 8: ch := 'G';
                 9: ch := 'R';
                10: ch := 'K';
                11: ch := 'D';
                12: ch := 'S';
                13: ch := 'V';
                14: ch := 'B';
                15: ch := 'N';
                16: ch := '-';
              else
                ch := 'N'
              end;
            end
            else
            begin
              for seq := 0 to FSequences.Count - 1 do
              begin
                if Terminated then Exit;
                if FSequences[seq].NoOfSites < site then
                  continue;
                case upcase(FSequences[seq].SeqData[site]) of
                  'A': inc(count[1]);
                  'C': inc(count[2]);
                  'D': inc(count[3]);
                  'E': inc(count[4]);
                  'F': inc(count[5]);
                  'G': inc(count[6]);
                  'H': inc(count[7]);
                  'I': inc(count[8]);
                  'K': inc(count[9]);
                  'L': inc(count[10]);
                  'M': inc(count[11]);
                  'N': inc(count[12]);
                  'P': inc(count[13]);
                  'Q': inc(count[14]);
                  'R': inc(count[15]);
                  'S': inc(count[16]);
                  'T': inc(count[17]);
                  'V': inc(count[18]);
                  'W': inc(count[19]);
                  'Y': inc(count[20]);
                  '-': inc(count[21]);
                end;
              end;
              for j := 1 to 21 do
                if count[j] > max then
                begin
                  max := count[j];
                  imax := j;
                end
                else if count[j] = max then
                  imax := 22;
              case imax of
                 0: ch := 'X';
                 1: ch := 'A';
                 2: ch := 'C';
                 3: ch := 'D';
                 4: ch := 'E';
                 5: ch := 'F';
                 6: ch := 'G';
                 7: ch := 'H';
                 8: ch := 'I';
                 9: ch := 'K';
                10: ch := 'L';
                11: ch := 'M';
                12: ch := 'N';
                13: ch := 'P';
                14: ch := 'Q';
                15: ch := 'R';
                16: ch := 'S';
                17: ch := 'T';
                18: ch := 'V';
                19: ch := 'W';
                20: ch := 'Y';
                21: ch := '-';
              else
                ch := 'X';
              end;
            end;
            FHeaderString[site] := ch;
          end;
        end;
      frStrictConsensus:
          for site := FStartSite to Length(FHeaderString) do
        begin
          if True then
          begin
            if FSequences.IsDNA then
            begin
              max := 0;
              for seq := 0 to FSequences.Count - 1 do
              begin
                if Terminated then Exit;
                if FSequences[seq].NoOfSites <= site then
                  continue;
                case upcase(FSequences[seq].SeqData[site]) of
                  'A': max := max or  1;
                  'T': max := max or  2;
                  'U': max := max or  2;
                  'C': max := max or  4;
                  'G': max := max or  8;
                  'R': max := max or  9;
                  'Y': max := max or  6;
                  'S': max := max or 12;
                  'W': max := max or  3;
                  'K': max := max or 10;
                  'M': max := max or  5;
                  'B': max := max or 14;
                  'V': max := max or 13;
                  'D': max := max or 11;
                  'H': max := max or  7;
                  'N': max := max or 15;
                  '-': max := max or 16;
                end;
              end;
              case max of
                 0: ch := 'N';
                 1: ch := 'A';
                 2: ch := 'T';
                 3: ch := 'W';
                 4: ch := 'C';
                 5: ch := 'M';
                 6: ch := 'Y';
                 7: ch := 'H';
                 8: ch := 'G';
                 9: ch := 'R';
                10: ch := 'K';
                11: ch := 'D';
                12: ch := 'S';
                13: ch := 'V';
                14: ch := 'B';
                15: ch := 'N';
                16: ch := '-';
              else
                ch := 'N';
              end;
            end
            else
            begin
              max := 0;
              for seq := 0 to FSequences.Count - 1 do
              begin
                if Terminated then Exit;
                if FSequences[seq].NoOfSites < site then
                  continue;
                case upcase(FSequences[seq].SeqData[site]) of
                  'A': max := max or       1;
                  'C': max := max or       2;
                  'D': max := max or       4;
                  'E': max := max or       8;
                  'F': max := max or      16;
                  'G': max := max or      32;
                  'H': max := max or      64;
                  'I': max := max or     128;
                  'K': max := max or     256;
                  'L': max := max or     512;
                  'M': max := max or    1024;
                  'N': max := max or    2048;
                  'P': max := max or    4096;
                  'Q': max := max or    8192;
                  'R': max := max or   16384;
                  'S': max := max or   32768;
                  'T': max := max or   65536;
                  'V': max := max or  131072;
                  'W': max := max or  262144;
                  'Y': max := max or  524288;
                  '-': max := max or 1048576;
                end;
              end;
              case max of
                      0: ch := 'X';
                      1: ch := 'A';
                      2: ch := 'C';
                      4: ch := 'D';
                      8: ch := 'E';
                     16: ch := 'F';
                     32: ch := 'G';
                     64: ch := 'H';
                    128: ch := 'I';
                    256: ch := 'K';
                    512: ch := 'L';
                   1024: ch := 'M';
                   2048: ch := 'N';
                   4096: ch := 'P';
                   8192: ch := 'Q';
                  16384: ch := 'R';
                  32768: ch := 'S';
                  65536: ch := 'T';
                 131072: ch := 'V';
                 262144: ch := 'W';
                 524288: ch := 'Y';
                1048576: ch := '-';
              else
                ch := 'X';
              end;
            end;
            FHeaderString[site] := ch;
          end;
        end;
    end;
  finally
    HeaderCS.Release;
  end;
end;

procedure TUpdateHeaderStringThread.Execute;
var
  isDone: Boolean = False;
begin
  try
    while not isDone do
    begin
      DoExecute;
      isDone := True;
    end;
  except
    on E:EAbort do
    begin
      FIsSuccess := False;
      FIsCancelled := True;
      FLog.Add(E.Message);
    end;
    on E:Exception do
    begin
      FIsSuccess := False;
      FIsCancelled := False;
      FLog.Add(E.Message);
    end;
  end;
end;

procedure TUpdateHeaderStringThread.DoCheckAbort;
begin
  if Assigned(CheckAbort) then
    FIsCancelled := CheckAbort;
end;

procedure TUpdateHeaderStringThread.DoUpdateStatus;
begin
  if Assigned(UpdateStatusProc) then
    UpdateStatusProc('Status', FProgressString);
end;

constructor TUpdateHeaderStringThread.Create(HeaderStr: AnsiString; seqData: TSequenceList; RowStyle: TFixedRowStyle; startSite: Int64);
var
  i: Integer;
begin
  inherited Create(True);
  FProgressString := '0';
  SetLength(FHeaderString, Length(HeaderStr));
  if Length(FHeaderString) > 0 then
    for i := 1 to Length(FHeaderString) do
      FHeaderString[i] := HeaderStr[i];
  FSequences := seqData;
  FFixedRowStyle := RowStyle;
  FStartSite := startSite;
  FreeOnTerminate := False;
  FLog := TStringList.Create;
  FIsSuccess := True;
  FIsCancelled := False;
end;

destructor TUpdateHeaderStringThread.Destroy;
begin
  if Assigned(FLog) then
    FLog.Free;
  inherited Destroy;
end;


//// TUndoInfo

constructor TUndoInfo.Create;
begin
  inherited;
  data := TStringList.Create;
end;

destructor TUndoInfo.Destroy;
begin
  data.Free;
  inherited;
end;

//// TUndoInfoList

destructor TUndoInfoList.Destroy;
begin
  Clear;
  inherited;
end;

function TUndoInfoList.GetItems(index: integer):TUndoInfo;
begin
  result := TUndoInfo(inherited Items[index]);
end;

procedure TUndoInfoList.SetItems(index: integer; value: TUndoInfo);
begin
  inherited Items[index] := value;
end;

function TUndoInfoList.Add(Item: Pointer): Integer;
var
  ui: TUndoInfo;
begin
  Result := inherited Add(Pointer(Item));
  if Assigned(ColumnEditedNotify) then
  begin
    ui := TUndoInfo(Item);
    case ui.Proc of
      upDelete, upInsert: ColumnEditedNotify(ui.x0);
      upAddSeq, upDelSeq, upReplace: ColumnEditedNotify(1);
    end;
  end;
end;

constructor TUndoInfoList.Create(colEditedNotify: TColumnEditedNotify);
begin
  inherited Create;
  ColumnEditedNotify := colEditedNotify;
end;

procedure TUndoInfoList.Delete(Index: integer);
begin
  if (Count > 0) and (Index < Count) then
  begin
    Items[Index].Free;
    inherited;
  end;
end;

procedure TUndoInfoList.Clear;
var
  i: integer;
begin
  for i := 0 to Count-1 do
    Items[i].Free;
  inherited;
end;

////  TAlignGrid

constructor TAlignGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SearchHighlightBgColor := clYellow;
  SearchHighlightFontColor := clRed;
  FHeaderStringInvalid := False;
  FSkipHeaderUpdates := False;
  Self.SetSubComponent(True);
  BiDiMode := bdLeftToRight;
  ClickNotify := nil;
  DoubleBuffered := true;
  SeqList := TSequenceList.Create;
  DNASeqs := TStringList.Create;
  Cols := TColList.Create(Self);
  SeqList.IsDNA := true;
  SeqList.IsProteinCoding := true;

  UndoInfoList := TUndoInfoList.Create(UpdateHeaderString);

  EditBox := TEdit.Create(Application.MainForm);
  EditBox.Parent := self;
  EditBox.Color := clWhite;
  EditBox.BorderStyle := bsNone;
  EditBox.Visible := false;
  EditBox.MaxLength := 1024;
  EditBox.OnExit := EditBoxOnExit;
  EditBox.OnKeyDown := EditBoxOnKeyDown;

  DNASites := ['A','T','C','G','U','R','Y','M','K','W','S','B','V','D','H','N','?'];
  AASites := ['A','C','D','E','F','G','H','I','K','L','M','N','P','Q','R','S','T','V','W','Y','X','*','?'];

  FEditEnabled  := true;
  FAlignEnabled := true;
  FShowColor    := true;
  FShowFixedRow := true;
  FShowFixedCol := true;

  CodeTable := GetStandardGeneticCode;

  FConsensusValue := 100;

  ColWidthSet := false;
  InitFont;
  DefaultDrawing := False;
end;

destructor TAlignGrid.Destroy;
begin
  EditBox.Free;
  UndoInfoList.Free;
  DNASeqs.Free;
  SeqList.Free;
  Cols.Free; // previously leaked memory
  inherited Destroy;
end;

procedure TAlignGrid.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  Self.KeyDown(Key, Shift);
end;

procedure TAlignGrid.SetShowFixedRow(value: boolean);
begin
  FShowFixedRow := value;
  if value then
    FixedRows := 1
  else
    FixedRows := 0;
end;

procedure TAlignGrid.SetShowFixedCol(value: boolean);
begin
  if value = FShowFixedCol then exit;
  FShowFixedCol := value;
  if value then
    FixedCols := 1
  else
    FixedCols := 0;
end;

procedure TAlignGrid.SetCursorCenter;
var
  x: integer;
begin
  if (Selection.Left >= LeftCol) and (Selection.Right < LeftCol+VisibleColCount) then exit;

  x := (Selection.Right+Selection.Left-VisibleColCount) div 2;
  if x <= 0 then
    x := 1
  else if x > ColCount-VisibleColCount+1 then
    x := ColCount-VisibleColCount+1;

  LeftCol := x;
  Invalidate;
end;

procedure TAlignGrid.SetHeaderString(AValue: AnsiString);
begin
  if FHeaderString = AValue then Exit;
  FHeaderString := AValue;
  Invalidate;
end;

function TAlignGrid.GetTitle: AnsiString;
begin
  result := SeqList.Title;
end;

procedure TAlignGrid.SetTitle(value: AnsiString);
begin
  SeqList.Title := value;
end;

procedure TAlignGrid.Clear;
begin
  SeqList.Clear;
  UndoInfoList.Clear;
  ResetColWidth;
  ResetRowHeight;
  ResetColCount;
  ResetRowCount;
  FModified := False;
end;

procedure TAlignGrid.JumpToSite(index: integer; gap: boolean);
var
  i,j: integer;
  s: TGridRect;
begin
  if index < 1 then exit;
  if index > SeqList[Row-FixedRows].NoOfSites then exit;
  if gap then
    s.Left := index +FixedCols -1
  else
  begin
    j := 0;
    for i := 1 to SeqList[Row-FixedRows].NoOfSites do
    begin
      if SeqList[Row-FixedRows][i] <> '-' then inc(j);
      if j = index then
      begin
        s.Left := i +FixedCols -1;
        break;
      end;
    end;
  end;
  s.Right  := s.Left;
  s.Top    := Selection.Top;
  s.Bottom := Selection.Bottom;
  Selection := s;
  Col := s.Left;
  SetCursorCenter;
end;

function TAlignGrid.SiteIndex(gap: boolean):integer;
var
  i: integer;
begin
  result := 0;
  if empty or ((Row - FixedRows) < 0) then exit;
  result := Col -FixedCols +1;
  if result > SeqList[Row-FixedRows].NoOfSites then
    result := SeqList[Row-FixedRows].NoOfSites;
  if not gap then
    for i := 1 to result do
      if SeqList[Row-FixedRows][i] = '-' then dec(result);
  if Result < 0 then
    Result := 0;
end;

function TAlignGrid.HasGroupNames: Boolean;
begin
  Result := SeqList.HasGroupNames;
end;

procedure TAlignGrid.ReplaceTaxaNames(aList: TStringList);
var
  i: Integer;
begin
  if (aList.Count <> SeqList.Count) then
    raise Exception.Create('invalid replacement names list');
  if aList.Count > 0 then
    for i := 0 to aList.Count - 1 do
      SeqList[i].FSeqName := aList[i];
  Invalidate;
end;

function TAlignGrid.GetAA(Codon: array of AnsiChar):AnsiChar;
var
  x, y, z: LongInt;
begin
  x := -1; y := -1; z := -1;
  case Upcase(Codon[0]) of  'U', 'T': x:=0; 'C': x:=1; 'A': x:=2; 'G': x:=3; end;
  case Upcase(Codon[1]) of  'U', 'T': y:=0; 'C': y:=1; 'A': y:=2; 'G': y:=3; end;
  case Upcase(Codon[2]) of  'U', 'T': z:=0; 'C': z:=1; 'A': z:=2; 'G': z:=3; end;

  if (codon[0]='-') and (codon[1]='-') and (codon[2]='-') then
    Result := '-'
  else if (x < 0) or (y < 0) or (z < 0) then
    Result := '?'
  else
    Result := CodeTable[x*16+y*4+z+1];
end;

procedure TAlignGrid.Polish;
begin
  SeqList.Polish;
  ResetColCount;
end;

procedure TAlignGrid.Pack;
var
  i,j,k, undoListStartPos: integer;
  ui: TUndoInfo;
  flag,gonext,undoListOverflow: boolean;
begin
  gonext := false;
  undoListStartPos := undoInfoList.Count;
  UndoInfoList.ColumnEditedNotify := nil;
  undoListOverflow := false;
  for j := MaxNoOfSites downto 1 do
  begin
    flag := false;
    for i := 0 to NoOfSeqs-1 do
    begin
      if (j <= SeqList[i].NoOfSites) and (SeqList[i].FSeqData[j] <> '-') then
      begin
        flag := true;
        break;
      end;
    end;
    if not flag then
      if j <= MinNoOfSites then
      begin
        try
          if not undoListOverflow then
          begin
            ui := TUndoInfo.Create;
            ui.Proc := upDelete;
            ui.x0 := j;
            ui.y0 := 0;
            ui.x1 := j;
            ui.y1 := NoOfSeqs-1;
            for k := 0 to NoOfSeqs-1 do
              ui.data.Add('-');
            ui.Pos.X := Col;
            ui.Pos.Y := Row;
            ui.Ori.X := LeftCol;
            ui.Ori.Y := TopRow;
            ui.Sel := Selection;
            ui.goNext := gonext;
            UndoInfoList.Add(ui);
          end;
        except on EOutOfMemory do
        begin
          undoListOverflow := true;
          for k := undoInfoList.Count-1 downto undoListStartPos do
          begin
            undoInfoList.Delete(k);
          end;
        end;
        end;
        gonext := true;
        DeleteBlock(j,0,j,NoOfSeqs-1);
      end
      else
        for i := 0 to NoOfSeqs-1 do
          if j <= SeqList[i].NoOfSites then
          begin
            DeleteBasesAt(i, j, 1);
            UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
            gonext := true;
          end;
  end;

  ResetColCount;
  UpdateHeaderString(MaxNoOfSites);
  UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  if undoListOverflow then
    MessageDlg('You can NOT undo this delete gaps operation. MEGA tried to save the previous state but there was not enough memory to do so.', mtError, [mbOK], 0);
end;

procedure TAlignGrid.Translate;
var
  i,j: integer;
  codon: array[0..2] of AnsiChar;
  pw: TPleaseWait = nil;
  updateTime: TDateTime;
begin
  if not IsDNA then exit;
  try
    pw := TPleaseWait.Create(nil);
    pw.Caption := 'Please Wait';
    pw.Action := 'Translating DNA sequences';
    pw.SetShowCancel(False);
    pw.Show;
    updateTime := Now;
    DNASeqs.Clear;
    for i := 0 to SeqList.Count-1 do
    begin
      DNASeqs.Add(SeqList[i].SeqData);
      for j := 0 to (SeqList[i].NoOfSites div 3)-1 do
      begin
        codon[0] := SeqList[i][j*3+1];
        codon[1] := SeqList[i][j*3+2];
        codon[2] := SeqList[i][j*3+3];
        SeqList[i][j+1] := GetAA(codon);
      end;
      System.Delete(SeqList[i].FSeqData, (SeqList[i].NoOfSites div 3)+1, SeqList[i].NoOfSites);
      if MillisecondsBetween(Now, updateTime) > 200 then
      begin
        pw.Action := Format('Translating DNA sequences %d%%', [Round(i/SeqList.Count*100)]);
        updateTime := Now;
        Application.ProcessMessages;
      end;
    end;
    IsDNA := false;
    Translated := true;

    ClearSelection;
    Col := FixedCols;
    Row := FixedRows;
    ResetColCount;
  finally
    UpdateHeaderString(1);
    if Assigned(pw) then
      pw.Free;
  end;
end;

procedure TAlignGrid.UnTranslate;
var
  seq: AnsiString;
  i,j: integer;
  pw: TPleaseWait = nil;
  updateTime: TDateTime;
  DNAStringBuilder: TMegaStringBuilder = nil;
  CurrentCodon: AnsiString;
  CurrentCodonIndex: Integer;
  CodonMatch: Boolean;
begin
  if not Translated then exit;
  try

    pw := TPleaseWait.Create(nil);
    pw.Caption := 'Please Wait';
    pw.Action := 'Untranslating amino acid sequences';
    pw.SetShowCancel(False);
    pw.Show;
    updateTime := Now;
    DNAStringBuilder := TMegaStringBuilder.Create;

    { more efficient untranslate function builds string one codon at a time,
      rather than using string inserts\deletes.

      We iterate through the translated protein sites one by one.
      if the site is a gap, stringbuilder adds a complete gap codon (---).
      if the site is not a gap, stringbuilder adds the next codon from the original
      DNA sequence that is not ---.

      This rebuilds the original DNA sequence with its gap codons replaced
      with gap codons corresponding to the gaps in the protein sequence. }

    { Developer warning: Updates to this function should also be considered
      for TSequence.Untranslate, as it performs essentially the same function.
      Reason why we maintain two copies:

      TSequence maintains good separation between its original and translated
      sequences with two strings (FSeqData and FSeqDataTranslated).

      AlignGrid does not and uses FSeqData for many edit functions, including storing
      translated sequences.

      These edit functions would have to be carefully untangled from AlignGrid to avoid
      breaking functionality.

      Ideally FSeqData should be private and modification access dictated only by
      TSequence functions.  }

    for i := 0 to NoOfSeqs-1 do
    begin
      seq := DNASeqs[i];
      //DNAStringBuilder.Capacity := Length(SeqList[i]) * 3;
      CurrentCodonIndex := 1;
      for j := 1 to SeqList[i].NoOfSites do
      begin
        if (SeqList[i][j] = '-') then
        begin
           DNAStringBuilder.Add('---');

           // necessary to handle translated sequence having a - at the end
           CurrentCodon := System.Copy(seq, CurrentCodonIndex, 3);
           if (CurrentCodon = '---') then
             inc(CurrentCodonIndex, 3);
        end
        else
        begin
          CurrentCodon := System.Copy(seq, CurrentCodonIndex, 3);
          while (CurrentCodon = '---') do
          begin
            inc(CurrentCodonIndex, 3);
            CurrentCodon := System.Copy(seq, CurrentCodonIndex, 3);
          end;
          DNAStringBuilder.Add(CurrentCodon);
          inc(CurrentCodonIndex, 3);
        end;
      end;

      // appends remaining DNA sites omitted from the initial translation
      while (Length(seq) > CurrentCodonIndex) do
      begin
        CurrentCodon := System.Copy(seq, CurrentCodonIndex, 3);
        if (CurrentCodon <> '---') then
           DNAStringBuilder.Add(System.Copy(seq, CurrentCodonIndex, 3));
        inc(CurrentCodonIndex, 3);
      end;

      SeqList[i].SeqData := DNAStringBuilder.GenerateString;
      DNAStringBuilder.Clean;
      if MilliSecondsBetween(Now, updateTime) > 200 then
      begin
        pw.Action := Format('Untranslating amino acid sequences %d%%', [Round(i/NoOfSeqs*100)]);
        updateTime := Now;
        Application.ProcessMessages;
      end;
    end;
    DNASeqs.Clear;
    Translated := False;
    IsDNA := True;
    ClearSelection;
    Col := FixedCols;
    Row := FixedRows;
    ResetColCount;
    Invalidate;
  finally
    if Assigned(pw) then
      pw.Free;
    if Assigned(DNAStringBuilder) then
      DNAStringBuilder.Free;
  end;
end;

function TAlignGrid.Empty: boolean;
begin
  result := SeqList.Count = 0;
end;

procedure TAlignGrid.SetIsDNA(value: boolean);
begin
  SeqList.IsDNA := value;
end;

function TAlignGrid.GetIsDNA: boolean;
begin
  result := SeqList.IsDNA;
end;

procedure TAlignGrid.SetIsProteinCoding(value: boolean);
begin
  SeqList.isProteinCoding := value;
end;

function TAlignGrid.GetIsProteinCoding: boolean;
begin
  if not IsDNA then
    result := false
  else
    result := SeqList.isProteinCoding;
end;

function TAlignGrid.GetSequence(index: integer):TSequence;
begin
  if (index < 0) or (index >= NoOfSeqs) then
    result := nil
  else
    result := SeqList[index];
end;

function TAlignGrid.GetNoOfSeqs: integer;
begin
  if not Assigned(SeqList) then
    Result := 0
  else
    Result := SeqList.Count;
end;

function TAlignGrid.GetNoOfSites(index: integer): integer;
begin
  if (index >= 0) and (index < NoOfSeqs) then
    result := Length(SeqList[index].FSeqData)
  else
    result := 0;
end;

function TAlignGrid.GetMaxNoOfSites:integer;
begin
  result := SeqList.MaxNoOfSites;
end;

function TAlignGrid.GetMinNoOfSites: integer;
begin
  result := SeqList.MinNoOfSites;
end;

procedure TAlignGrid.GetRangeOfSelectedSites(var startSite, endSite: integer);
begin
  if Selection.Left = 0 then
    startSite := 1
  else
    startSite :=  Selection.Left - FixedCols + 1;
  if (Selection.Right > 1) and (Selection.Right >= MaxNoOfSites + FixedCols) then
    endSite := MaxNoOfSites
  else
    endSite := Selection.Right - FixedCols + 1;
  if endSite = 0 then startSite := 0;
end;

procedure TAlignGrid.GetRangeOfSelectedSeqs(var startSeq, endSeq: integer);
begin
  if Selection.Top = 0 then
    startSeq := 0
  else
    startSeq := Selection.Top - FixedRows;
  if (NoOfSeqs > 0) and (Selection.Bottom >= NoOfSeqs + FixedRows) then
    endSeq := NoOfSeqs-1
  else
    endSeq := Selection.Bottom - FixedRows;
  if endSeq = 0 then startSeq := 0;
end;

procedure TAlignGrid.ResetColCount;
begin
  ColCount := MaxNoOfSites+FixedCols+1;
end;

procedure TAlignGrid.ResetRowCount;
begin
  if Empty then
    RowCount := FixedRows+1
  else
    RowCount := NoOfSeqs+FixedRows;
end;

procedure TAlignGrid.ResetColWidth;
var
  i,max, origCol0Width: integer;
begin
  origCol0Width := ColWidths[0];
  Canvas.Font := Font;
  DefaultColWidth := Canvas.TextWidth('W')+2;

  if Empty then exit;
  if Cols.Count > ColCount then Exit;

  if not ColWidthSet then
  begin
    origCol0Width := (Width * 60) div 100; // if we haven't set the width, then default it to a MAX of 60% of the available width.
    colWidthSet := True;
  end;

  {$IFNDEF UNIX}
  { it's unclear if these lines still serve any purpose
    because the column widths are resized again almost immediately by CheckFixedColWidth
    but on Linux\MacOS triggers a bug where first site column is too wide until redrawn }
  for i:=0 to Cols.Count-1 do  // this is for future columns (retrieved information from GenBank)
    ColWidths[i] := Canvas.TextWidth(Cols.Col[i].ColName)+Canvas.TextWidth('     ');
  {$ENDIF}

  max := ColWidths[0];
  if NoOfSeqs > 0 then
  for i := 0 to NoOfSeqs-1 do
    if max < Canvas.TextWidth(SeqList[i].FSeqName) then
      max := Canvas.TextWidth(SeqList[i].FSeqName);
  max := max+Canvas.TextWidth('     ');
  // Set the column to be the MAX if the MAX is smaller than the origCol0Width
  if max > origCol0Width then
    max := origCol0Width;
  if max > width-Canvas.TextWidth('W')-4-(GetSystemMetrics(SM_CXVSCROLL)) then  // This needs to take into account a scrollbar (just assume it exists)
    max := width -Canvas.TextWidth('W')-4-(GetSystemMetrics(SM_CXVSCROLL));      // To keep space for at least a single character for sequences

  ColWidths[0] := max;
end;

procedure TAlignGrid.ResetRowHeight;
begin
  {$IFNDEF DARWIN}
  Canvas.Font := Font;
  if Canvas.Font.Height < 0 then
    DefaultRowHeight := Canvas.GetTextHeight('W') + 4
  else
    DefaultRowHeight := Canvas.GetTextHeight('W') + 4;
  {$ENDIF}
end;

procedure TAlignGrid.ResetGrid;
begin
  ResetColCount;
  ResetRowCount;
end;

procedure TAlignGrid.ResetSize;
begin
  ResetColWidth;
  ResetRowHeight;
end;

procedure TAlignGrid.Initialize;
var
  tmpOptions: TGridOptions;
begin
  if not Empty then
  begin
    UndoInfoList.Clear;
    FixedCols := FixedCols;
    FixedRows := FixedRows;


    ResetRowHeight;
    ResetColCount;
    ResetRowCount;
    ResetColWidth;
    tmpOptions := Options;
    Include(tmpOptions, goRangeSelect);
    Include(tmpOptions, goDrawFocusSelected);
    {$IFDEF DARWIN}
    Exclude(tmpOptions, goSmoothScroll);
    Exclude(tmpOptions, goThumbTracking);
    {$ELSE}
    Include(tmpOptions, goSmoothScroll);
    Include(tmpOptions, goThumbTracking);
    {$ENDIF}
    Exclude(tmpOptions, goColMoving);
    Exclude(tmpOptions, goEditing);
    Exclude(tmpOptions, goAlwaysShowEditor);
    Exclude(tmpOptions, goSelectionActive);
    Options := tmpOptions;

    Col := FixedCols;
    Row := FixedRows;
    Invalidate;
    FModified := false;
  end;
end;

function TAlignGrid.LoadFromFASTAFile(filename: String): boolean;
var
  sl: TStringList;
  seqs: TSequenceList;
begin
  Result := false;
  sl   := nil;
  seqs := nil;
  try
    sl := TStringList.Create;
    sl.LoadFromFile(filename);

    seqs := TSequenceList.Create;

    if sl.Count = 0 then exit;

    ParseFASTA(sl, seqs, false);
    if seqs.Count = 0 then exit;

    Clear;
    InsertSequenceList(seqs);
    Initialize;
    Result := true;
  finally
    seqs.Free;
    sl.Free;
  end;
end;

function TAlignGrid.InsertFile(filename: String): boolean;
var
  F: file;
  buffer: array[0..255] of AnsiChar;
  str: AnsiString;
  i,j: integer;
begin
  for i := Low(buffer) to High(buffer) do
    buffer[i] := #0;
  try
    AssignFile(F, filename);
    Reset(F, 1);
    i := 0;
    while not EOF(F) and (i < 256) do
    begin
      BlockRead(F, buffer[i], 1);
      inc(i);
    end;
  finally
    CloseFile(F);
  end;

  setlength(str, i);
  for j := 1 to i do
    str[j] := buffer[j-1];
  if (system.Copy(str, 1, 4) = 'ABIF') or (system.Copy(str, 129, 4) = 'ABIF') then
    Result := InsertABIFile(filename)
  else if (system.Copy(str, 1, 4) = '.scf') or (system.Copy(str, 129, 4) = '.scf') then
    Result := InsertSCFFile(filename)
  else if Pos('>', str) > 0 then
    Result := InsertFASTAFile(filename)
  else
    Result := InsertTextFile(filename);

  if Result then
    Invalidate;
end;

function TAlignGrid.InsertFASTAFile(filename: String): boolean;
var
  buffer: TStringList;
  seqs: TSequenceList;
begin
  buffer := nil;
  seqs := nil;
  Result := False;
  try
    buffer := TStringList.Create;
    buffer.LoadFromFile(filename);
    seqs := TSequenceList.Create;
    ParseFASTA(buffer, seqs, true);
    if seqs.Count = 0 then
      exit;
    InsertSequenceList(seqs);
    Result := True;
  finally
    seqs.Free;
    buffer.Free;
  end;
end;

function TAlignGrid.InsertTextFile(filename: String): boolean;
var
  buffer: TStringList;
  i: integer;
  seq: TSequence;
  str: AnsiString;
  mr: Word;
begin
  buffer := nil;
  seq := nil;
  result := True;

  try
    try
      buffer := TStringList.Create;
      buffer.LoadFromFile(filename);

      if buffer.Count > 0 then
      begin
        seq := TSequence.Create;
        seq.SeqName := ExtractFileName(filename);
        if ExtractFileExt(filename) <> '' then
          seq.SeqName := System.Copy(seq.SeqName, 1, Length(seq.SeqName)-Length(ExtractFileExt(filename)));

        i := 0;
        while i < buffer.Count do
        begin
          str := Trim(buffer[i]);
          if str <> '' then
          begin
            while Pos(' ', str) > 0 do
              System.Delete(str, Pos(' ', str), 1);
            seq.SeqData := seq.SeqData +str;
          end;
          inc(i);
        end;

        for i := 1 to seq.NoOfSites do
          if seq.FSeqData[i] = '-' then
            Continue
          else if (IsDNA and not (upcase(seq.FSeqData[i]) in DNASites)) or (not IsDNA and not (upcase(seq.FSeqData[i]) in AASites)) then
            begin
              mr := MessageDlg('Invalid character found. Continue?', mtWarning, [mbCancel, mbIgnore], 0);
              if mr = mrCancel then
              begin
                result := false;
                break;
              end;
            end;
        if not result then
          exit;
        InsertSequence(seq);
        seq := nil;
      end;
      Result := True;
    Except
      on E:Exception do
        ShowMessage('Oh no! An error occurred: ' + E.Message);
    end;
  finally
    buffer.Free;
    if seq <> nil then seq.Free;
  end;
end;

function TAlignGrid.InsertABIFile(filename: String): boolean;
type
  TArrayOfByte = array [0..MaxInt-1] of byte;
  PArrayOfByte = ^TArrayOfByte;
  TDataRecord = Record
    tagname:  array[0..3] of AnsiChar;
    tagindex: integer;
    datatype: smallint;
    elementlen: smallint;
    elementnum: integer;
    datalen: integer;
    datarecord: array[0..3] of AnsiChar;
    cryptic: integer;
  end;
  TDataRecordArray = array [0..(MaxInt div sizeof(TDataRecord))-1] of TDataRecord;

  procedure ConvRecord(datarecord: PArrayOfByte);
  var
    b: byte;
    p: ^integer;
  begin
    //  tagindex
    b := datarecord[4];
    datarecord[4] := datarecord[7];
    datarecord[7] := b;
    b := datarecord[5];
    datarecord[5] := datarecord[6];
    datarecord[6] := b;
    // datatype
    b := datarecord[8];
    datarecord[8] := datarecord[9];
    datarecord[9] := b;
  //  elementlen
    b := datarecord[10];
    datarecord[10] := datarecord[11];
    datarecord[11] := b;

    //  elementnum
    b := datarecord[12];
    datarecord[12] := datarecord[15];
    datarecord[15] := b;
    b := datarecord[13];
    datarecord[13] := datarecord[14];
    datarecord[14] := b;

    // datalen
    b := datarecord[16];
    datarecord[16] := datarecord[19];
    datarecord[19] := b;
    b := datarecord[17];
    datarecord[17] := datarecord[18];
    datarecord[18] := b;

  // datarecord
    p := addr(datarecord[16]);
    if p^ > 4 then
    begin
      b := datarecord[20];
      datarecord[20] := datarecord[23];
      datarecord[23] := b;
      b := datarecord[21];
      datarecord[21] := datarecord[22];
      datarecord[22] := b;
    end;

    // cryptic
    b := datarecord[24];
    datarecord[24] := datarecord[27];
    datarecord[27] := b;
    b := datarecord[25];
    datarecord[25] := datarecord[26];
    datarecord[26] := b;
  end;


var
  F: file;
  buffer: PArrayOfByte = nil;
  DataRecord: ^TDataRecordArray;
  p: ^integer;
  b4: array[0..3] of byte;
  i,j,n: integer;
  seq: TSequence;
  pw: TPleaseWait = nil;
  updateTime: TDateTime;
begin
  updateTime := Now;
  Result := False;
  if not IsDNA then
    if MessageDlg('You are inserting DNA data in protein data.', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
      exit;

  try
    pw := TPleaseWait.Create(nil);
    pw.Caption := 'Please Wait';
    pw.Action := 'Loading sequence data file';
    pw.SetShowCancel(False);
    pw.Show;
    AssignFile(F, filename);
    Reset(F, 1);
    GetMem(buffer, FileSize(F));
    for i := 0 to FileSize(F)-1 do
      BlockRead(F, buffer[i], 1);

    if (FileSize(F) > 132) and (AnsiChar(buffer[128])+AnsiChar(buffer[129])+AnsiChar(buffer[130])+AnsiChar(buffer[131]) = 'ABIF') then
      for i := 0 to FileSize(F)-129 do
        buffer[i] := buffer[i+128];

    if AnsiChar(buffer[0])+AnsiChar(buffer[1])+AnsiChar(buffer[2])+AnsiChar(buffer[3]) = 'ABIF' then
    begin
      seq := TSequence.Create;

      b4[0] := buffer[21];
      b4[1] := buffer[20];
      b4[2] := buffer[19];
      b4[3] := buffer[18];
      p := addr(b4);
      n := p^;
      b4[0] := buffer[29];
      b4[1] := buffer[28];
      b4[2] := buffer[27];
      b4[3] := buffer[26];
      p := addr(b4);
      DataRecord := addr(buffer[p^]);
      for i := 0 to n-1 do
        ConvRecord(addr(DataRecord[i]));
      if MillisecondsBetween(Now, updateTime) > 200 then
      begin
        updateTime := Now;
        Application.ProcessMessages;
      end;
      for i := 0 to n-1 do
      begin
        with DataRecord[i] do
          if tagname[0]+tagname[1]+tagname[2]+tagname[3] = 'SMPL' then
            if datalen > 4 then
              for j := 1 to buffer[integer(datarecord)] do
                seq.SeqName := seq.SeqName +AnsiChar(buffer[integer(datarecord)+j])
            else
            begin
              for j := 1 to integer(datarecord[0]) do
                seq.SeqName := seq.SeqName +datarecord[j];
            end
          else if (tagname[0]+tagname[1]+tagname[2]+tagname[3] = 'PBAS') and (tagindex = 1) then
          begin
            setlength(seq.FSeqData, datalen);
            for j := 0 to datalen-1 do
              seq.FSeqData[j+1] := AnsiChar(buffer[integer(datarecord)+j]);
          end;
        if MillisecondsBetween(Now, updateTime) > 200 then
        begin
          updateTime := Now;
          Application.ProcessMessages;
        end;
      end;
      InsertSequence(seq);
    end;
    Result := True;
  finally
    CloseFile(F);
    if Assigned(pw) then
      pw.Free;
    if assigned(buffer) and (buffer <> nil) then
    begin
      FreeMem(buffer);
      buffer := nil;
    end;
  end;
end;

function TAlignGrid.InsertSCFFile(filename: String): boolean;

type
  TArrayOfByte = array [0..MaxInt-1] of byte;
  PArrayOfByte = ^TArrayOfByte;
  TDataHeader = Record
    magic_number: array[0..3] of AnsiChar;
    samples: integer;
    samples_offset: integer;
    bases: integer;
    bases_left_clip: integer;
    bases_right_clip: integer;
    bases_offset: integer;
    comments_size: integer;
    comments_offset: integer;
    version: array[0..3] of AnsiChar;
    sample_size: integer;
    code_set: integer;
    private_size: integer;
    private_offset: integer;
  end;
  PDataHeader = ^TDataHeader;

  procedure ConvHeader(header: PArrayOfByte);
  var
    b: byte;
  begin
    //  samples
    b := header[4];
    header[4] := header[7];
    header[7] := b;
    b := header[5];
    header[5] := header[6];
    header[6] := b;

    // samples_offset
    b := header[8];
    header[8] := header[11];
    header[11] := b;
    b := header[9];
    header[9] := header[10];
    header[10] := b;

    //  bases
    b := header[12];
    header[12] := header[15];
    header[15] := b;
    b := header[13];
    header[13] := header[14];
    header[14] := b;

    // bases_left_clip
    b := header[16];
    header[16] := header[19];
    header[19] := b;
    b := header[17];
    header[17] := header[18];
    header[18] := b;

    // bases_right_clip
    b := header[20];
    header[20] := header[23];
    header[23] := b;
    b := header[21];
    header[21] := header[22];
    header[22] := b;

    // bases_offset
    b := header[24];
    header[24] := header[27];
    header[27] := b;
    b := header[25];
    header[25] := header[26];
    header[26] := b;

    // comments_size
    b := header[28];
    header[28] := header[31];
    header[31] := b;
    b := header[29];
    header[29] := header[30];
    header[30] := b;

    // comments_offset
    b := header[32];
    header[32] := header[35];
    header[35] := b;
    b := header[33];
    header[33] := header[34];
    header[34] := b;

    // sample_size
    b := header[40];
    header[40] := header[43];
    header[43] := b;
    b := header[41];
    header[41] := header[42];
    header[42] := b;

    // code_set
    b := header[44];
    header[44] := header[47];
    header[47] := b;
    b := header[45];
    header[45] := header[46];
    header[46] := b;

    // private_size
    b := header[48];
    header[48] := header[51];
    header[51] := b;
    b := header[49];
    header[49] := header[50];
    header[50] := b;

    // cprivate_offset
    b := header[52];
    header[52] := header[55];
    header[55] := b;
    b := header[53];
    header[53] := header[54];
    header[54] := b;
  end;

var
  F: file;
  buffer: PArrayOfByte;
  DataHeader: PDataHeader;
  i,p,cr,cl: integer;
  seq: TSequence;
  str: AnsiString;
begin
  Result := False;
  if not IsDNA then
    if MessageDlg('You are inserting DNA data in protein data.', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
      exit;

  buffer := nil;
  try
    AssignFile(F, filename);
    Reset(F, 1);
    GetMem(buffer, FileSize(F));
    for i := 0 to FileSize(F)-1 do
      BlockRead(F, buffer[i], 1);

    if (FileSize(F) > 132) and (AnsiChar(buffer[128])+AnsiChar(buffer[129])+AnsiChar(buffer[130])+AnsiChar(buffer[131]) = '.scf') then
      for i := 0 to FileSize(F)-129 do
        buffer[i] := buffer[i+128];

    if (AnsiChar(buffer[0])+AnsiChar(buffer[1])+AnsiChar(buffer[2])+AnsiChar(buffer[3]) = '.scf') and
       (strtoInt(AnsiChar(buffer[36])) >= 3) then
    begin
      ConvHeader(buffer);
      DataHeader := PDataHeader(buffer);
      seq := TSequence.Create;
      seq.SeqName := ExtractFileName(filename);
      if ExtractFileExt(filename) <> '' then
        seq.SeqName := System.Copy(seq.SeqName, 1, Length(seq.SeqName)-Length(ExtractFileExt(filename)));


      if DataHeader.bases_right_clip > 0 then
        cr := DataHeader.bases_right_clip
      else
        cr := DataHeader.bases+1;
      if DataHeader.bases_left_clip > 0 then
        cl := DataHeader.bases_left_clip
      else
        cl := 0;

      setlength(str, cr-cl-1);
      p := 128 +DataHeader.samples*DataHeader.sample_size*4 +DataHeader.bases*8;

      if cr-cl > 1 then
        for i := cl+1 to cr-1 do
          str[i-cl] := AnsiChar(buffer[p+i-1]);

      seq.FSeqData := str;
      InsertSequence(seq);
    end;

    Result := True;
  finally
    CloseFile(F);
    if assigned(buffer) or (buffer <> nil) then
    begin
      if buffer <> nil then
      FreeMem(buffer);
      buffer := nil;
    end;
  end;
end;

function TAlignGrid.BlockSelected:boolean;
begin
  Result := False;
  if not (goRangeSelect in Options) then exit;

  if NumFixedColsSelected > 0 then Exit;

  if (Selection.Left < Selection.Right) or (Selection.Top < Selection.Bottom) then
    result := true;
end;

function TAlignGrid.AllSelected: Boolean;
begin
  if Empty then Exit(False);
  Result := (Selection.Top <= 1) and
            (Selection.Bottom = (RowCount - 1)) and
            (Selection.Left <= FixedCols) and
            (Selection.Right = max(1, SiteIndexToColumn(MaxNoOfSites)));
end;

function TAlignGrid.Selected(ACol, ARow: integer):boolean;  //aok
begin
  Result := Inherited IsCellSelected[aCol, aRow];
end;

function TAlignGrid.SequenceSelected: boolean;  // FWN
var
  i: Integer;
begin
  Result := False;
  if Empty then exit;
  if (Selection.Top >= 1) and (Selection.Bottom <= NoOfSeqs) then
    for i := Selection.Top to Selection.Bottom do
    begin
      Result := Result or RowIsSelected(i);
      if Result then
        Exit;
    end;
  //Result := (Selection.Left >= 0) and (Selection.Left <= SiteIndexToColumn(FixedCols)) and (Selection.Top >= 1) and (Selection.Bottom <= NoOfSeqs);
end;

function TAlignGrid.GetNoOfSelectedSites: integer;
begin
  Result := 0;
  if (Selection.Left < 0) or (Selection.Right < 0) then Exit;
  if Selection.Right = Selection.Left then
    Exit(1);
  if Selection.Right > Selection.Left then
    Result := Selection.Right - Selection.Left + 1 - NumFixedColsSelected;
end;

function TAlignGrid.NumFixedCols: Integer;
begin
  Result := FixedCols;
end;

function TAlignGrid.NumFixedColsSelected: Integer;
begin
  Result := 0;
  if FixedCols = 0 then Exit;
  if (Selection.Left < 0) or (Selection.Left >= FixedCols) then Exit;
  Result := FixedCols - Selection.Left;
end;

function TAlignGrid.GetNoOfSelectedSeqs: integer;   //aok
begin
  if Selection.Top = 0 then
    result := Selection.Bottom-Selection.Top
  else
    result := Selection.Bottom-Selection.Top+1;
end;

function TAlignGrid.SiteSelected: boolean; //fwn
begin
  result := false;
  if Empty then exit;
  Result := (Selection.Top <= 1) and (Selection.Left >= FixedCols) and (Selection.Right <= SiteIndexToColumn(MaxNoOfSites));
end;

procedure TAlignGrid.SelectSequenceAt(index: integer);  //AOK
var
  s: TGridRect;
begin
  s.Top    := index +FixedRows;
  s.Bottom := index +FixedRows;
  s.Left   := 0;
  s.Right := ColCount-1;
  Selection := s;
  Ori.X := FixedCols;
  Ori.Y := index+FixedRows;
end;

procedure TAlignGrid.SelectSiteAt(index: integer);   //AOK
var
  s: TGridRect;
begin
  if index = (MaxNoOfSites + 1) then
  begin
    // continue. A bug fix so the last column can be selected (broken by adding the groups column)
  end
  else if index > MaxNoOfSites then
    exit;
  if not (goRangeSelect in Options) then exit;
  s.Top := 0;
  s.Bottom := RowCount-1;
  s.Left := index;
  s.Right := index;
  Selection := s;
  Ori.X := index;
  Ori.Y := 0;
end;

procedure TAlignGrid.SelectSequence;  //AOK
var
  s: TGridRect;
begin
  if Selection.Top = 0 then
    s.Top := 1
  else
    s.Top := Selection.Top;
  s.Bottom := Selection.Bottom;
  s.Left := 0;
  s.Right := ColCount-1;
  Selection := s;
  Ori.X := 0;
  Ori.Y := s.Top;
end;

procedure TAlignGrid.SelectSite;   //AOK
var
  s: TGridRect;
begin
  s.Top := 0;
  s.Bottom := RowCount-1;
  if Selection.Left = 0 then
    s.Left := 1
  else
    s.Left := Selection.Left;
  s.Right := Selection.Right;
  Selection := s;
  Ori.X := s.Left;
  Ori.Y := 0;
end;

procedure TAlignGrid.Select(startCol, startRow, endCol, endRow: integer);
var
  s: TGridRect;
begin
  if Empty then Exit;
  if MaxNoOfSites > 0 then
  begin
    Assert(InRange(startCol, 1, MaxNoOfSites), Format('invalid site - got %d but valid range is 1 to %d', [startCol, MaxNoOfSites]));
    Assert(InRange(endCol, 1, MaxNoOfSites + FixedCols), Format('invalid site - got %d but valid range is 1 to %d', [endCol, MaxNoOfSites + FixedCols]));
  end;
  Assert(InRange(startRow, 0, RowCount - 1), Format('invalid seq - got %d but valid range is 1 to %d', [startRow, RowCount - 1]));
  Assert(InRange(endRow, 0, RowCount - 1), Format('invalid seq - got %d but valid range is 1 to %d', [endRow, RowCount - 1]));
  if startCol < SiteIndexToColumn(1) then
    startCol := SiteIndexToColumn(1);
  if startRow < 0 then
    startRow := 0;
  if endCol > ColCount then
    endCol := ColCount;
  if endRow > RowCount then
    endRow := RowCount - 1;
  if endCol < startCol then
    endCol := startCol;
  if endRow < startRow then
    endRow := startRow;

  s.Left := startCol;
  s.Top := startRow;
  s.Right := endCol;
  s.Bottom := endRow;
  Selection := s;
  Invalidate;
end;

procedure TAlignGrid.SelectedBlock(var site0, seq0, site1, seq1: integer);
begin
  if SequenceSelected then
  begin
    site0 := 1;
    site1 := MaxNoOfSites;
  end
  else
  begin
    site0 := Selection.Left - FixedCols + 1;
    site1 := Selection.Right - FixedCols + 1;
  end;
  if SiteSelected then
  begin
    seq0 := 0;
    seq1 := NoOfSeqs - 1;
  end
  else
  begin
    seq0 := Selection.Top - 1;
    seq1 := Selection.Bottom - 1;
  end;
end;

procedure TAlignGrid.SelectBlock(site0, seq0, site1, seq1: integer);
var
  s: TGridRect;
  debug: Integer = -1;
begin
  s.Left   := site0 +FixedCols -1;
  s.Top    := seq0  +1;
  s.Right  := site1 +FixedCols -1;
  s.Bottom := seq1  +1;
  if s.Left < FixedCols then
    s.Left := FixedCols
  else if s.Left > ColCount then
    s.Left := ColCount;
  if s.Right < s.Left then
    s.Right := s.Left
  else if s.Right > ColCount then
    s.Right := ColCount;
  if s.Top < 1 then
    s.Top := 1
  else if s.Top > RowCount then
    s.Top := RowCount;
  if s.Bottom < s.Top then
    s.Bottom := s.Top
  else if s.Bottom > RowCount then
    s.Bottom := RowCount;

  if s.Bottom >= TopRow+VisibleRowCount then
    TopRow := s.Bottom-VisibleRowCount;
  if s.Top < TopRow then
    TopRow := s.Top;
  if s.Right >= LeftCol+VisibleColCount then
    LeftCol := s.Right-VisibleColCount;
  if s.Left < LeftCol then
  begin
    debug := LeftCol;
    LeftCol := s.Left;
  end;

  Selection := s;
  Invalidate;
end;

procedure TAlignGrid.SelectAll;  //fwn
var
  s: TGridRect;
begin
  s.Left := FixedCols;
  s.Top := FixedRows;
  if MaxNoOfSites = 0 then
    s.Right := SiteIndexToColumn(1)
  else
    s.Right := SiteIndexToColumn(MaxNoOfSites);
  if NoOfSeqs = 0 then
    s.Bottom := FixedRows
  else
    s.Bottom := RowCount-1;
  Selection := s;
  Invalidate;
end;

procedure TAlignGrid.ClearSelection;
var
  s : TGridRect;
begin
  s.Left   := Col;
  s.Right  := Col;
  s.Top    := Row;
  s.Bottom := Row;
  Selection := s;
end;

procedure TAlignGrid.SetSeqData(data: TSequenceList);
var
  seq: TSequence;
  i: integer;
begin
  if data.Count = 0 then exit;
  SeqList.Clear;
  for i := 0 to data.Count-1 do
  begin
    seq := TSequence.Create;
    seq.Assign(data[i]);
    FilterSeqName(seq);
    SeqList.Add(seq);
  end;
  SeqList.Title := data.Title;
  SeqList.IsDNA := data.IsDNA;
  SeqList.IsProteinCoding := data.IsProteinCoding;

  Initialize;
end;

procedure TAlignGrid.TransferSeqData(data: TSequenceList);
var
  seq: TSequence;
  i: integer;
begin
  if data.Count = 0 then exit;
  SeqList.Clear;
  for i := 0 to data.Count-1 do
  begin
    seq := data[i];
    SeqList.Add(seq);
  end;
  data.RelinquishSequences;
  SeqList.Title := data.Title;
  SeqList.IsDNA := data.IsDNA;
  SeqList.IsProteinCoding := data.IsProteinCoding;
  Initialize;
end;

procedure TAlignGrid.GetSeqData(data: TSequenceList);
var
  i: integer;
  seq: TSequence;
begin
  if Empty then exit;
  data.Clear;
  for i := 0 to NoOfSeqs-1 do
  begin
    seq := TSequence.Create;
    seq.Assign(SeqList[i]);
    data.Add(seq);
  end;
  data.Polish;
  if ForceAlignment then
    for i := 0 to data.Count-1 do
      if (data[i].NoOfSites < data.MaxNoOfSites) then
        data[i].SeqData := Concat(data[i].SeqData, StringOfChar('-', data.MaxNoOfSites-data[i].NoOfSites));

  data.FIsDNA           := IsDNA;
  data.FIsProteinCoding := IsProteinCoding;
  data.FTitle           := Title;
end;

procedure TAlignGrid.SaveToFASTAFile(filename: String);
var
  data: TextFile;
  str : AnsiString;
  i: integer;
begin
  if Empty then exit;

  AssignFile(data, filename);
  Rewrite(data);
  for i := 0 to SeqList.Count-1 do
  begin
    str := StringReplace(SeqList[i].SeqName, ' ', '_', [rfReplaceAll]);
    if SeqList[i].SeqInfo <> EmptyStr then
      str := str + ' ' + SeqList[i].SeqInfo;

    writeln(data, '>'+str);
    write(data, SeqList[i].SeqData);
    if ForceAlignment  then
      writeln(data, StringOfChar('-', MaxNoOfSites-SeqList[i].NoOfSites))
    else
      writeln(data);
  end;

  CloseFile(data);

  FModified := false;
end;

procedure TAlignGrid.SaveToPAUPFile(filename: String);
var
  data: TextFile;
  str, aName, datatype : AnsiString;
  i,j: integer;
  maxLength, maxNameLength: integer;
begin
  if Empty then exit;

  AssignFile(data, filename);
  Rewrite(data);
  writeln(data, '#NEXUS');
  writeln(data, '[ TITLE '+SeqList.Title+']');

  // write taxa block
  writeLn(data, 'BEGIN TAXA;');
  writeLn(data, '       DIMENSIONS NTAX=' + IntToStr(SeqList.Count) + ';');
  writeLn(data, '       TAXLABELS');
  maxLength := 0;
  maxNameLength := 0;
  for i := 0 to SeqList.Count-1 do
  begin
    aName := SeqList[i].SeqName;
    while Pos(' ', aName) > 0 do
      aName[Pos(' ', aName)] := '_';
    writeLn(data, '             ' + aName);
    if Length(SeqList[i].SeqData) > maxLength then
      maxLength := Length(SeqList[i].SeqData);
    if Length(SeqList[i].SeqName) > maxNameLength then
      maxNameLength := Length(SeqList[i].SeqName);
  end;
  writeLn(data, ';');
  writeLn(data, 'END;');

  // write the characters block
  writeLn(data, 'BEGIN CHARACTERS;');
  writeLn(data, '       DIMENSIONS NCHAR=' + IntToStr(maxLength) + ';');
  if isDNA then
    datatype := 'nucleotide'
  else
    datatype := 'protein';
  writeLn(data, '       FORMAT MISSING=' + '?' + ' GAP=' + '-' + ' MATCHCHAR=' + '.' + ' datatype=' + datatype + ';'); // NOT DOING INTERLEAVED
  writeLn(data, 'MATRIX');
  writeLn(data, EmptyStr); // just for spacing
  for i := 0 to SeqList.Count-1 do
  begin
    aName := SeqList[i].SeqName;
    while Pos(' ', aName) > 0 do
      aName[Pos(' ', aName)] := '_';
    str := SeqList[i].SeqData;
    if IsDNA then
    begin
      for j := 1 to length(str) do
        if upcase(str[j]) = 'N' then str[j] := '?'; // replace N with ?.
    end
    else
      for j := 1 to length(str) do
        if upcase(str[j]) = 'X' then str[j] := '?'; // replace X with ?.
    if ForceAlignment and (Length(str) < MaxNoOfSites) then
      str := str + StringOfChar('-', MaxNoOfSites-SeqList[i].NoOfSites);
    writeLn(data, aName + StringOfChar(' ', maxNameLength - Length(aName) + 4) + str);
  end;
  writeLn(data, ';');
  writeLn(data, 'END;');
  CloseFile(data);

  FModified := false;
end;

procedure TAlignGrid.CacheSelectionData(var aRect: TRect);
begin
  aRect.Left := Selection.Left;
  aRect.Top := Selection.Top;
  aRect.Right := Selection.Right;
  aRect.Bottom := Selection.Bottom;
end;

procedure TAlignGrid.SetSelectionFromCachedData(const aRect: TRect);
begin
  Col := aRect.Left;
  Row := aRect.Top;
  Select(aRect.Left, aRect.Top, min(aRect.Right, MaxNoOfSites), aRect.Bottom);
  Invalidate;
end;

procedure TAlignGrid.SaveToMEGAFile(filename: String);
var
  data: TextFile;
  str: AnsiString;
  i,j: integer;
begin
  if Empty then exit;

  AssignFile(data, filename);
  Rewrite(data);
  writeln(data, '#mega');
  writeln(data, '!Title '+SeqList.Title+';');
  if IsDNA then
    if IsProteinCoding then
      if CodeTableName <> '' then
      begin
        str := CodeTableName;
        while Pos(' ', str) > 0 do
          str[Pos(' ', str)] := '_';
        writeln(data, '!Format DataType=DNA indel=- CodeTable='+str+';')
      end
      else
        writeln(data, '!Format DataType=DNA indel=-;')
    else
      writeln(data, '!Format DataType=DNA indel=-;')
  else
    writeln(data, '!Format DataType=Protein indel=-;');
  writeln(data);

  if IsDNA and IsProteinCoding then
    writeln(data, '!Domain=Data property=Coding CodonStart=1;');
//  else
//    writeln(data, '!Domain=Data;');

  for i := 0 to SeqList.Count-1 do
  begin
    str := SeqList[i].SeqName;
    if SeqList[i].SeqInfo <> '' then
      str := str+' "'+SeqList[i].SeqInfo+'"';
    if Trim(SeqList[i].FGroupName) <> EmptyStr then
      str := str + '_{' + SeqList[i].FGroupName + '}' ;
    while Pos(' ', str) > 0 do
      str[Pos(' ', str)] := '_';
    writeln(data, '#'+str);
    str := SeqList[i].SeqData;
    if IsDNA then
    begin
      for j := 1 to length(str) do
        if upcase(str[j]) = 'N' then str[j] := '?';
    end
    else
      for j := 1 to length(str) do
        if upcase(str[j]) = 'X' then str[j] := '?';
    for j := 0 to length(str) div 60 do
      writeln(data, System.Copy(str, j*60+1, 60));
    if ForceAlignment  then
      writeln(data, StringOfChar('-', MaxNoOfSites-SeqList[i].NoOfSites))
    else
      writeln(data);
  end;

  CloseFile(data);

  FModified := false;
end;

procedure TAlignGrid.WriteToFile(var f: File; var UserAborted: Boolean; aProgress: TRuntimeProgress);
const
  PROGRESS_UPDATE_INTERVAL = 500;
var
  i,j,k,n: integer;
  c: AnsiChar;
  cw: Char;
  b: boolean;
  buffer : array[0..4095] of AnsiChar;
  fc: TColor;
  fs: TFontStyles;
  lastProgUpdate: TDateTime;
begin
  UserAborted := False;
  aProgress.UpdateRunStatusInfo('Status', 'Saving Sequence Data');
  aProgress.UpdatePercentProgress(0);
  lastProgUpdate := Time;
  i := MAS_SESSION_VERSION;
  BlockWrite(f, i, 4);  // write version #
  i := length(Title);
  BlockWrite(f, i, 4);
  if i > 0 then
    for j := 1 to i do
    begin
      c := Title[j];
      BlockWrite(f, c, 1);
    end;
  b := IsDNA;
  BlockWrite(f, b, 1);
  b := IsProteinCoding;
  BlockWrite(f, b, 1);
  i := length(CodeTableName);
  BlockWrite(f, i, 4);
  if i > 0 then
    for j := 1 to i do
    begin
      c := CodeTableName[j];
      BlockWrite(f, c, 1);
    end;
  i := length(CodeTable);
  BlockWrite(f, i, 4);
  if i > 0 then
    for j := 1 to i do
    begin
      c := CodeTable[j];
      BlockWrite(f, c, 1);
    end;

  i := 0;
  BlockWrite(f, i, 4);
  BlockWrite(f, i, 4);  // for future use

  n := NoOfSeqs;
  BlockWrite(f, n, 4);
  if n > 0 then
    for i := 0 to SeqList.Count-1 do
    begin
      j := length(SeqList[i].FSeqName);
      BlockWrite(f, j, 4);
      for k := 1 to j do
      begin
        c := SeqList[i].FSeqName[k];
        BlockWrite(f, c, 1);
      end;
      j := length(SeqList[i].FSeqInfo);
      BlockWrite(f, j, 4);
      for k := 1 to j do
      begin
        c := SeqList[i].FSeqInfo[k];
        BlockWrite(f, c, 1);
      end;
      j := length(SeqList[i].FUIDS);
      BlockWrite(f, j, 4);
      for k := 1 to j do
      begin
        c := SeqList[i].FUIDS[k];
        BlockWrite(f, c, 1);
      end;

      j := length(SeqList[i].FAccessionNum);     // KT: from MAS_SESSION_VERSION = 1011
      BlockWrite(f, j, 4);
      for k := 1 to j do
      begin
        c := SeqList[i].FAccessionNum[k];
        BlockWrite(f, c, 1);
      end;

      j := SeqList[i].NoOfSites;
      BlockWrite(f, j, 4);
      for k := 1 to j do
      begin
        c := SeqList[i].FSeqData[k];
        BlockWrite(f, c, 1);
      end;
      j :=  SeqList[i].MarkedSiteIndex;
      BlockWrite(f, j, 4);

      j := length(SeqList[i].FFileName);
      BlockWrite(f, j, 4);
      for k := 1 to j do
      begin
        cw := SeqList[i].FFileName[k];
        BlockWrite(f, cw, SizeOf(cw));
      end;

      j := 0;
      BlockWrite(f, j, 4);  // for future use
      if MillisecondsBetween(Time, lastProgUpdate) >= PROGRESS_UPDATE_INTERVAL then
      begin
        aProgress.UpdatePercentProgress(Round(i / SeqList.Count * 100));
        lastProgUpdate := Time;
        Application.ProcessMessages;
        {$IFDEF VISUAL_BUILD}
        if aProgress.StopBtn.Down then
        begin
          UserAborted := True;
          Exit;
        end;
        {$ENDIF}
      end;
    end;

  i := 0;
  BlockWrite(f, i, 4);  // for future use
  BlockWrite(f, i, 4);
  BlockWrite(f, i, 4);
  BlockWrite(f, i, 4);

  i:= length(SearchBox);
  BlockWrite(f, i, 4);
  if i > 0 then
    for j := 1 to i do
    begin
      c := FSearchBox[j];
      BlockWrite(f, c, 1);
    end;
  b := HighlightSearchBox;
  BlockWrite(f, b, 1);
  b := EditEnabled;
  BlockWrite(f, b, 1);
  b := AlignEnabled;
  BlockWrite(f, b, 1);
  b := ForceAlignment;
  BlockWrite(f, b, 1);
  b := ShowColor;
  BlockWrite(f, b, 1);
  b := ShowBGColor;
  BlockWrite(f, b, 1);

  i := Length(Font.Name);
  BlockWrite(f, i, 4);
  StrPCopy(buffer, Font.Name);
  BlockWrite(f, buffer, i);
  fc := Font.Color;
  BlockWrite(f, fc, SizeOf(TColor));
  i := Font.Size;
  BlockWrite(f, i, 4);
  fs := Font.Style;
  BlockWrite(f, fs, SizeOf(TFontStyle));

  i := LeftCol;
  BlockWrite(f, i, 4);
  i := TopRow;
  BlockWrite(f, i, 4);

  i := Selection.Left;
  BlockWrite(f, i, 4);
  i := Selection.Right;
  BlockWrite(f, i, 4);
  i := Selection.Top;
  BlockWrite(f, i, 4);
  i := Selection.Bottom;
  BlockWrite(f, i, 4);

  b := MarkConsensus;
  BlockWrite(f, b, 1);

  i := FConsensusValue;
  BlockWrite(f, i, 4);

  FModified := false;
end;

function TAlignGrid.InsertFromFile(var f: File):boolean;
var
  i: Integer = -1;
  j: Integer = -1;
  k: Integer = -1;
  n: Integer = -1;
  version: Integer = -1;
  c: AnsiChar = #0;
  cu: Char = #0; { unicode char}
  b: boolean = False;
  sl: TSequenceList = nil;
  seq: TSequence = nil;
  IOIssue: Integer = -1;
begin
  sl := TSequenceList.Create;

  try
    result := false;
    BlockRead(f, version, 4);  // write version #
    BlockRead(f, i, 4);
    if i > 0 then  // i = length of title
    begin
      SetLength(sl.FTitle, i);
      for j := 1 to i do  // loop through reading each char of the title
      begin
        BlockRead(f, c, SizeOf(c));
        sl.FTitle[j] := c;
      end;
    end;
    BlockRead(f, b, 1);
    sl.IsDNA := b;
    BlockRead(f, b, 1);
    sl.IsProteinCoding := b;
    BlockRead(f, i, 4);
    if i > 0 then  // i = length of the Genetic Code table name (if specified)
    begin
      for j := 1 to i do
      begin
        BlockRead(f, c, SizeOf(c));
      end;
    end;
    BlockRead(f, i, 4);
    if i > 0 then  // i = length of the genetic code (if specified)
    begin
      for j := 1 to i do
      begin
        BlockRead(f, c, SizeOf(c));
      end;
    end;

    BlockRead(f, i, 4);  // for future use
    BlockRead(f, i, 4);

    BlockRead(f, n, 4);
    if n > 0 then
      for i := 0 to n-1 do
      begin
        seq := TSequence.Create;
        BlockRead(f, j, 4);
        Setlength(seq.FSeqName, j);
        for k := 1 to j do
        begin
          BlockRead(f, c, SizeOf(c));
          seq.FSeqName[k] := c;
        end;
        BlockRead(f, j, 4);
        setlength(seq.FSeqInfo, j);
        for k := 1 to j do
        begin
          BlockRead(f, c, SizeOf(c));
          seq.FSeqInfo[k] := c;
        end;
        BlockRead(f, j, 4);
        setlength(seq.FUIDS, j);
        for k := 1 to j do
        begin
          BlockRead(f, c, SizeOf(c));
          seq.FUIDS[k] := c;
        end;

        if version >= 1011 then     // KT: from MAS_SESSION_VERSION = 1011
        begin
          BlockRead(f, j, 4);
          setlength(seq.FAccessionNum, j);
          for k := 1 to j do
          begin
            BlockRead(f, c, SizeOf(c));
            seq.FAccessionNum[k] := c;
          end;
        end;

        BlockRead(f, j, 4);
        setlength(seq.FSeqData, j);
        for k := 1 to j do
        begin
          BlockRead(f, c, SizeOf(c));
          seq.FSeqData[k] := c;
        end;
        BlockRead(f, j, 4);
        seq.MarkedSiteIndex := j;

        BlockRead(f, j, 4);
        setlength(seq.FFileName, j);
        for k := 1 to j do
        begin
          BlockRead(f, cu, SizeOf(cu));
          seq.FFileName[k] := cu;
        end;

        sl.Add(seq);
        seq := nil;

        BlockRead(f, j, 4);  // for future use
      end;
    InsertSequenceList(sl);

    Invalidate;

    result := true;

  finally
    IOIssue := IOResult; // can't check this twice, clears after the first check.
    if IOIssue <> 0 then // Check if an i/o exception occured and if so report it to the user. (I/O issues don't throw an exception unless the compiler option i/o checking is turned ON.  Which it is NOT for user builds.)
      ShowMessage('MEGA Encountered an error while attempting to read the supplied MAS file.' + #10#13 + 'Technical Info: IOResult = ' + IntToStr(IOIssue));
    sl.Free;
    if seq <> nil then
      seq.Free;
  end;
end;

function TAlignGrid.ReadFromFile(var f: File):boolean;
var
  i: Integer = -1;
  n: Integer = -1;
  j,k: Integer;
  version: integer = -1;
  c: AnsiChar = #0;
  cu: Char = #0;
  b: boolean = False;
  buffer : array[0..4095] of AnsiChar;
  fc: TColor = clBlack;
  fs: TFontStyles = [];
  sl: TSequenceList = nil;
  seq: TSequence = nil;
  s: TGridRect;
  IOIssue: Integer = -1;
begin
  for i := Low(buffer) to High(buffer) do
    buffer[i] := #0;
  sl := TSequenceList.Create;
  seq := nil;
  try
    result := false;
    BlockRead(f, version, 4);  // write version #
    BlockRead(f, i, 4);
    if i > 0 then  // i = length of title
    begin
      SetLength(sl.FTitle, i);
      for j := 1 to i do  // loop through reading each char of the title
      begin
        BlockRead(f, c, SizeOf(c));
        sl.FTitle[j] := c;
      end;
    end;
    BlockRead(f, b, 1);
    sl.IsDNA := b;
    BlockRead(f, b, 1);
    sl.IsProteinCoding := b;
    BlockRead(f, i, 4);
    if i > 0 then  // i = length of the Genetic Code table name (if specified)
    begin
      setlength(FCodeTableName, i);
      for j := 1 to i do
      begin
        BlockRead(f, c, SizeOf(c));
        FCodeTableName[j] := c;
      end;
    end;
    BlockRead(f, i, 4);
    if i > 0 then  // i = length of the genetic code (if specified)
    begin
      setlength(FCodeTable, i);
      for j := 1 to i do
      begin
        BlockRead(f, c, SizeOf(c));
        FCodeTable[j] := c;
      end;
    end;

    BlockRead(f, i, 4);  // for future use
    BlockRead(f, i, 4);

    BlockRead(f, n, 4);
    if n > 0 then
      for i := 0 to n-1 do
      begin
        seq := TSequence.Create;
        BlockRead(f, j, 4);
        Setlength(seq.FSeqName, j);
        for k := 1 to j do
        begin
          BlockRead(f, c, SizeOf(c));
          seq.FSeqName[k] := c;
        end;
        BlockRead(f, j, 4);
        setlength(seq.FSeqInfo, j);
        for k := 1 to j do
        begin
          BlockRead(f, c, 1);
          seq.FSeqInfo[k] := c;
        end;
        BlockRead(f, j, 4);
        setlength(seq.FUIDS, j);
        for k := 1 to j do
        begin
          BlockRead(f, c, SizeOf(c));
          seq.FUIDS[k] := c;
        end;

        if version >= 1011 then     // KT: from MAS_SESSION_VERSION = 1011
        begin
          BlockRead(f, j, 4);
          setlength(seq.FAccessionNum, j);
          for k := 1 to j do
          begin
            BlockRead(f, c, SizeOf(c));
            seq.FAccessionNum[k] := c;
          end;
        end;

        BlockRead(f, j, 4);
        setlength(seq.FSeqData, j);
        for k := 1 to j do
        begin
          BlockRead(f, c, SizeOf(c));
          seq.FSeqData[k] := c;
        end;
        BlockRead(f, j, 4);
        seq.MarkedSiteIndex := j;

        BlockRead(f, j, 4);
        setlength(seq.FFileName, j);
        for k := 1 to j do
        begin
          BlockRead(f, cu, SizeOf(cu));
          seq.FFileName[k] := cu;
        end;

        sl.Add(seq);
        seq := nil;

        BlockRead(f, j, 4);  // for future use
      end;
    SetSeqData(sl);  // SETTING SEQUENCE DATA FOR THE MALIGNGRID

    BlockRead(f, i, 4);  // for future use
    BlockRead(f, i, 4);
    BlockRead(f, i, 4);
    BlockRead(f, i, 4);

    BlockRead(f, i, 4);
    setlength(FSearchBox, i);
    if i > 0 then
      for j := 1 to i do
      begin
        BlockRead(f, c, SizeOf(c));
        FSearchBox[j] := c;
      end;
    BlockRead(f, b, 1);
    HighlightSearchBox := b;
    BlockRead(f, b, 1);
    EditEnabled := b;
    BlockRead(f, b, 1);
    AlignEnabled := b;
    BlockRead(f, b, 1);
    ForceAlignment := b;
    BlockRead(f, b, 1);
    ShowColor := b;
    BlockRead(f, b, 1);
    ShowBGColor := b;

    BlockRead(f, i, 4);
    if i > 0 then
    begin
      BlockRead(f, buffer, i);
      buffer[i] := #0;
      Font.Name := StrPas(buffer);
    end;
    BlockRead(f, fc, SizeOf(TColor));
    Font.Color := fc;
    BlockRead(f, i, 4);
    Font.Size := i;
    if version >= 113 then
      BlockRead(f, fs, SizeOf(TFontStyle))
    else
      BlockRead(f, fs, 1); // in Delphi, the size was 1
    Font.Style := ValidFontStyles(fs); ;
    BlockRead(f, i, 4);
    LeftCol := i;
    BlockRead(f, i, 4);
    TopRow := i;

    BlockRead(f, i, 4);
    s.Left := i;
    BlockRead(f, i, 4);
    s.Right := i;
    BlockRead(f, i, 4);
    s.Top := i;
    BlockRead(f, i, 4);
    s.Bottom := i;
    Selection := s;

    if version >= 110 then
    begin
      BlockRead(f, FMarkConsensus, 1);
      BlockRead(f, FConsensusValue, 4);
    end;

    Invalidate;

    result := true;

  finally
    IOIssue := IOResult; // can't check this twice, clears after the first check.
    if IOIssue <> 0 then // Check if an i/o exception occured and if so report it to the user. (I/O issues don't throw an exception unless the compiler option i/o checking is turned ON.  Which it is NOT for user builds.)
      ShowMessage('MEGA Encountered an error while attempting to read the supplied MAS file.' + #10#13 + 'Technical Info: IOResult = ' + IntToStr(IOIssue));
    sl.Free;
    if seq <> nil then
      seq.Free;
  end;
end;


procedure TAlignGrid.ColRowMoved(IsColumn: Boolean; FromIndex, ToIndex: Longint);
var
  ui: TUndoInfo;
begin
  if IsColumn then
    Exit;
  if FixSequenceOrder then
  begin
    ClearSelection;
    Invalidate;
    exit;
  end;
  
  if FromIndex > NoOfSeqs then Exit;
  if ToIndex > NoOfSeqs then Exit;

  SeqList.Move(FromIndex-FixedRows, ToIndex-FixedRows);
  if DNASeqs.Count = NoOfSeqs then
    DNASeqs.Move(FromIndex-FixedRows, ToIndex-FixedRows);

  ui := TUndoInfo.Create;
  ui.Proc := upSeqMove;
  ui.x0 := integer(Cols.Col[0].SortOrder);
  ui.y0 := FromIndex-FixedRows;
  ui.y1 := ToIndex-FixedRows;
  ui.Pos.X := Ori.X;
  ui.Pos.Y := Ori.Y;
  ui.Ori.X := LeftCol;
  ui.Ori.Y := TopRow;
  ui.Sel := Selection;
  ui.Sel.Top    := FromIndex;
  ui.Sel.Bottom := FromIndex;

  UndoInfoList.Add(ui);
  Invalidate;

  FModified := true;

  if SequenceSelected then
  begin
    Ori.X := FixedCols;
    Ori.Y := Selection.Top;
  end;

  Cols.Col[0].SortOrder := soNone;

  inherited;
end;


procedure TAlignGrid.MoveRow(FromIndex, ToIndex: Longint);
var
  ui: TUndoInfo;
begin
  if FixSequenceOrder then exit;

  if FromIndex > NoOfSeqs then Exit;
  if ToIndex > NoOfSeqs then Exit;
  SeqList.Move(FromIndex-1, ToIndex-1);
  if DNASeqs.Count = NoOfSeqs then
    DNASeqs.Move(FromIndex-1, ToIndex-1);

  ui := TUndoInfo.Create;
  ui.Proc := upSeqMove;
  ui.x0 := integer(Cols.Col[0].SortOrder);
  ui.y0 := FromIndex-1;
  ui.y1 := ToIndex-1;
  ui.Pos.X := Ori.X;
  ui.Pos.Y := Ori.Y;
  ui.Ori.X := LeftCol;
  ui.Ori.Y := TopRow;
  ui.Sel := Selection;
  UndoInfoList.Add(ui);

  FModified := true;

  Cols.Col[0].SortOrder := soNone;
end;

procedure TAlignGrid.MoveSequenceUp;
var
  s: TGridRect;
begin
  if not SequenceSelected then exit;
  if Selection.Top <= 1 then exit;
  MoveRow(Selection.Top-1, Selection.Bottom);

  s.Top    := Selection.Top-1;
  s.Bottom := Selection.Bottom-1;
  s.Left   := Selection.Left;
  s.Right  := Selection.Right;
  Selection := s;

  if SequenceSelected then
  begin
    Ori.X := 0;
    Ori.Y := Selection.Top;
  end;

  Invalidate;
end;

procedure TAlignGrid.MoveSequenceDown;
var
  s: TGridRect;
begin
  if not SequenceSelected then exit;
  if Selection.Bottom = RowCount-1 then exit;
  MoveRow(Selection.Bottom+1, Selection.Top);

  s.Bottom := Selection.Bottom+1;
  s.Top    := Selection.Top+1;
  s.Left   := Selection.Left;
  s.Right  := Selection.Right;
  Selection := s;

  if SequenceSelected then
  begin
    Ori.X := 0;
    Ori.Y := Selection.Top;
  end;

  Invalidate;
end;

procedure TAlignGrid.InsertBlock(x, y: integer; block: TStringList);
var
  c,i: integer;
begin
  if block.Count = 0 then exit;
  if y >= NoOfSeqs then exit;
  if y+block.Count > NoOfSeqs then
    c := NoOfSeqs-y
  else
    c := block.Count;
{
  for i := y to y+c-1 do
    if x > SeqList[i].NoOfSites+1 then
      exit;
}
  for i := 0 to c-1 do
  begin
    System.Insert(block[i], SeqList[i+y].FSeqData, x);
    if SeqList[i+y].MarkedSiteIndex >= x then
      SeqList[i+y].MarkedSiteIndex := SeqList[i+y].MarkedSiteIndex + Length(block[i]);
  end;

  FModified := true;
end;

procedure TAlignGrid.InsertBasesAt(SeqIndex, SiteIndex: integer; Bases: AnsiString);
var
  ui: TUndoInfo = nil;
begin
  ui := TUndoInfo.Create;
  if SiteIndex > SeqList[SeqIndex].NoOfSites+1 then
    SiteIndex := SeqList[SeqIndex].NoOfSites+1;
  System.Insert(Bases, SeqList[SeqIndex].FSeqData, SiteIndex);
  if SeqList[SeqIndex].MarkedSiteIndex >= SiteIndex then
    SeqList[SeqIndex].MarkedSiteIndex := SeqList[SeqIndex].MarkedSiteIndex + Length(Bases);
  with ui do
  begin
    Proc := upInsert;
    x0 := SiteIndex;
    y0 := SeqIndex;
    x1 := x0+Length(Bases)-1;
    y1 := y0;
    Pos.X := Col;
    Pos.Y := Row;
    Ori.X := LeftCol;
    Ori.Y := TopRow;
    Sel := Selection;
  end;
  UndoInfoList.Add(ui);
  FModified := true;
end;

procedure TAlignGrid.Insert(Bases: TStringList);
var
  ui: TUndoInfo = nil;
  s: TGridRect;
  max: integer;
  x: Integer = -1;
  y0: Integer = -1;
  i: Integer = -1;
  y1: integer = -1;
  flag, gonext: boolean;
begin
  if not EditEnabled then exit;
  if Bases.Count = 0 then exit;
  GetRangeOfSelectedSites(x, i);
  GetRangeOfSelectedSeqs(y0, y1);

  if y0+Bases.Count > NoOfSeqs then
  begin
    y1 := NoOfSeqs-1;
    for i := Bases.Count-1 downto NoOfSeqs-y0 do
      Bases.Delete(i);
  end
  else
    y1 := y0 +Bases.Count-1;

  if Bases.Count = 1 then
    max := Length(Bases[0])
  else
  begin
    flag := false;
    max := Length(Bases[0]);
    for i := 1 to Bases.Count-1 do
      if Length(Bases[i]) <> Length(Bases[0]) then
      begin
        flag := true;
        if max < Length(Bases[i]) then
          max := Length(Bases[i]);
      end;
    if flag then
      for i := 0 to Bases.Count-1 do
        if Length(Bases[i]) < max then
          if SeqList[y0+i].NoOfSites >= x then
            Bases[i] := Bases[i] +StringOfChar('-', max-Length(Bases[i]));
  end;

  gonext := false;
  for i := y0 to y1 do
    if SeqList[i].NoOfSites < x-1 then
    begin
      InsertBasesAt(i, SeqList[i].NoOfSites+1, StringOfChar('-', x-SeqList[i].NoOfSites-1));
      UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
      gonext := true;
    end;

  ui := TUndoInfo.Create;
  ui.Proc := upInsert;
  ui.x0 := x;
  ui.y0 := y0;
  ui.x1 := x +max-1;
  ui.y1 := y1;
  ui.Pos.X := Col;
  ui.Pos.Y := Row;
  ui.Ori.X := LeftCol;
  ui.Ori.Y := TopRow;
  ui.Sel := Selection;
  ui.goNext := gonext;

  InsertBlock(x, y0, Bases);

  ResetColCount;
  s.Left := ui.x0+Cols.Count-1;
  s.Top  := ui.y0+1;
  s.Right := ui.x1+Cols.Count-1;
  s.Bottom := ui.y1+1;
  Selection := s;
  Col := Selection.Right;
  Row := Selection.Bottom;
  try
    UndoInfoList.ColumnEditedNotify := nil;
    UndoInfoList.Add(ui);
  finally
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.InsertGaps;
var
  y0: Integer = -1;
  y1: integer = -1;
  gaps: TStringList = nil;
  i: integer;
  flag: boolean = False;
begin
  if not AlignEnabled then exit;
  if SequenceSelected then exit;
  flag := EditEnabled;
  EditEnabled := true;
  try
    gaps := TStringList.Create;
    if BlockSelected then
    begin
      GetRangeOfSelectedSeqs(y0,y1);
      for i := y0 to y1 do
        gaps.Add(StringOfChar('-', Selection.Right-Selection.Left+1));
      Insert(gaps);
      ResetColCount;
      Col := Selection.Right;
    end
    else
    begin
      InsertBasesAt(Row-FixedRows, Col-FixedCols+1, '-');
      ResetColCount;
      if Col >= SeqList[Row-FixedRows].NoOfSites+FixedCols then
        Col := SeqList[Row-FixedRows].NoOfSites+FixedCols
      else
        Col := Col+1;
    end;
    EditEnabled := flag;
    Invalidate;
  finally
    if Assigned(gaps) then
      gaps.Free;
  end;
end;

procedure TAlignGrid.Replace(Bases: TStringList);
var
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
  i: integer;
  ui: TUndoInfo = nil;
  s: TGridRect;
  flag, gonext: boolean;
begin
  GetRangeOfSelectedSites(x0,x1);
  GetRangeOfSelectedSeqs(y0,y1);

  if x0 = 0 then exit;
  try
    UndoInfoList.ColumnEditedNotify :=  nil;
    flag := false;
    for i := y0 to y1 do
      if SeqList[i].NoOfSites < x0 then
      begin
        flag := true;
        break;
      end;

    if flag then
    begin
      s.Left := x0+Cols.Count-1;
      s.Top  := y0+1;
      s.Right := x0+Cols.Count-1;
      s.Bottom := y1+1;
      gonext := false;
      for i := y0 to y1 do
        if SeqList[i].NoOfSites >= x0 then
        begin
          DeleteBasesAt(i, x0, x1-x0+1);
          UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
          gonext := true;
          if Length(Bases[i - y0]) > 0 then
          begin
            InsertBasesAt(i, x0, Bases[i - y0]);
            UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
            if s.Right < x0+Length(Bases[i - y0])-1+Cols.Count-1 then
              s.Right := x0+Length(Bases[i - y0])-1+Cols.Count-1;
          end;
        end;
    end
    else
    begin
      ui := TUndoInfo.Create;
      ui.Proc := upDelete;
      ui.x0 := x0;
      ui.y0 := y0;
      ui.x1 := x1;
      ui.y1 := y1;
      for i := y0 to y1 do
        ui.data.Add(System.Copy(SeqList[i].FSeqData, x0, x1-x0+1));
      ui.Pos.X := Col;
      ui.Pos.Y := Row;
      ui.Ori.X := LeftCol;
      ui.Ori.Y := TopRow;
      ui.Sel := Selection;
      UndoInfoList.Add(ui);

      DeleteBlock(x0,y0,x1,y1);

      ui := TUndoInfo.Create;
      ui.Proc := upInsert;
      ui.x0 := x0;
      ui.y0 := y0;
      ui.x1 := x0 +Length(Bases[0])-1;
      ui.y1 := y1;
      ui.Pos.X := x0;
      ui.Pos.Y := y0+1;
      ui.Ori.X := LeftCol;
      ui.Ori.Y := TopRow;
      ui.Sel := Selection;
      ui.goNext := true;

      InsertBlock(x0, y0, Bases);
      UndoInfoList.Add(ui);
      s.Left := ui.x0+Cols.count-1;
      s.Top  := ui.y0+1;
      s.Right := ui.x1+Cols.Count-1;
      s.Bottom := ui.y1+1;
    end;

    ResetColCount;
    //Selection := s;
    //Col := Selection.Right;
    //Row := Selection.Bottom;
  finally
    if not Assigned(FUpdateHeaderThread) then
      UpdateHeaderString(1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.DeleteBlock(x0,y0,x1,y1: integer);
var
  i: integer;
begin
  if (y0 > y1) or (y1 >= NoOfSeqs) then exit;
  if (x0 > x1) or (x0 > MaxNoOfSites) then exit;
  for i := y0 to y1 do
  begin
    System.Delete(SeqList[i].FSeqData, x0, x1-x0+1);
    if SeqList[i].MarkedSiteIndex > x1 then
      SeqList[i].MarkedSiteIndex := SeqList[i].MarkedSiteIndex -(x1-x0+1)
    else if SeqList[i].MarkedSiteIndex >= x0 then
      SeqList[i].MarkedSiteIndex := 0;
  end;
  FModified := true;
end;

procedure TAlignGrid.DeleteBasesAt(SeqIndex, SiteIndex, Count: integer);
var
  ui: TUndoInfo = nil;
begin
  if (SeqIndex >= SeqList.Count) or (SeqList[SeqIndex].NoOfSites < SiteIndex) then
    exit
  else if SeqList[SeqIndex].NoOfSites < SiteIndex+Count-1 then
    Count := SeqList[SeqIndex].NoOfSites-SiteIndex+1;

  if Col >= LeftCol+VisibleColCount then
    LeftCol := Col-VisibleColCount+1
  else if Col < LeftCol then
    LeftCol := Col;
  if Row >= TopRow+VisibleRowCount then
    TopRow := Row-VisibleRowCount+1
  else if Row < TopRow then
    TopRow := Row;

  ui := TUndoInfo.Create;
  with ui do
  begin
    Proc := upDelete;
    x0 := SiteIndex;
    y0 := SeqIndex;
    x1 := x0+Count-1;
    y1 := y0;
    data.Add(System.Copy(SeqList[SeqIndex].FSeqData, SiteIndex, Count));
    Pos.X := Col+FixedCols-1;
    if Row = 0 then
      Pos.Y := FixedRows
    else
      Pos.Y := Row+FixedRows-1;
    Ori.X := LeftCol;
    Ori.Y := TopRow;
    Sel := Selection;
  end;

  System.Delete(SeqList[SeqIndex].FSeqData, SiteIndex, Count);
  if SeqList[SeqIndex].MarkedSiteIndex >= SiteIndex+Count then
    SeqList[SeqIndex].MarkedSiteIndex := SeqList[SeqIndex].MarkedSiteIndex -Count
  else if SeqList[SeqIndex].MarkedSiteIndex >= SiteIndex then
    SeqList[SeqIndex].MarkedSiteIndex := 0;
  if FSkipHeaderUpdates then
  begin
    try
      UndoInfoList.ColumnEditedNotify := nil;
      UndoInfoList.Add(ui);
    finally
      UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
    end;
  end;

  FModified := true;
end;

procedure TAlignGrid.Delete;
var
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
  ui: TUndoInfo;
  i: integer;
  flag: boolean;
begin

  if not EditEnabled then
    if AlignEnabled then
      DeleteGaps
    else
      exit
  else
  begin
    try
      FSkipHeaderUpdates := True;
      UndoInfoList.ColumnEditedNotify := nil;
      if SequenceSelected then
        DeleteSequence
      else if BlockSelected then
      begin
        GetRangeOfSelectedSites(x0,x1);
        GetRangeOfSelectedSeqs(y0,y1);
        flag := false;
        for i := y0 to y1 do
          if SeqList[i].NoOfSites < x0 then
          begin
            flag := true;
            break;
          end;
        if flag then
        begin
          flag := false;
          for i := y0 to y1 do
          begin
            if SeqList[i].NoOfSites >= x0 then
            begin
              DeleteBasesAt(i, x0, x1-x0+1);
              UndoInfoList[UndoInfoList.Count-1].goNext := flag;
              flag := true;
            end;
          end;
        end
        else
        begin
          ui := TUndoInfo.Create;
          ui.Proc := upDelete;
          ui.x0 := x0;
          ui.y0 := y0;
          ui.x1 := x1;
          ui.y1 := y1;
          for i := y0 to y1 do
            ui.data.Add(System.Copy(SeqList[i].FSeqData, x0, x1-x0+1));
          ui.Pos.X := Col;
          ui.Pos.Y := Row;
          ui.Ori.X := LeftCol;
          ui.Ori.Y := TopRow;
          ui.Sel := Selection;
          DeleteBlock(x0,y0,x1,y1);
          UndoInfoList.Add(ui);
        end;
        ClearSelection;
        Col := x0+FixedCols-1;
        Row := y0+FixedRows;
        ResetColCount;
      end
      else
        DeleteBasesAt(Row-FixedRows, Col-FixedCols+1, 1);

      Invalidate;
    finally
      FSkipHeaderUpdates := False;
      UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
      UpdateHeaderString(Selection.Left - 1);
    end;
  end;
end;

procedure TAlignGrid.DeleteGaps;
var
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
  i,c: integer;
  str: AnsiString;
  gonext: boolean;
  PleaseWait: TPleaseWait = nil;
  updateTime: TDateTime;
  quickExit: Boolean = False;
begin
  if not AlignEnabled then exit;
  updateTime := now;
  try
    UndoInfoList.ColumnEditedNotify := nil;
    GetRangeOfSelectedSites(x0,x1);
    GetRangeOfSelectedSeqs(y0,y1);
    if BlockSelected or SequenceSelected then
    begin
      PleaseWait := TPleaseWait.Create(nil);
      PleaseWait.Caption := 'Please Wait';
      PleaseWait.Action := 'Deleting Gaps...';
      PleaseWait.SetShowCancel(False);
      PleaseWait.show;

      gonext := false;
      for i := y0 to y1 do
      begin
        if x0 > SeqList[i].NoOfSites then continue;
        str := System.Copy(SeqList[i].SeqData, x0, x1-x0+1);
        c := Length(str);
        while Pos('-', str) > 0 do
          System.Delete(str, Pos('-', str), 1);
        if Length(str) < c then
        begin
          DeleteBasesAt(i, x0, c);
          if FSkipHeaderUpdates then
            UndoInfoList[max(0, UndoInfoList.Count-1)].goNext := gonext;
          gonext := true;
          if Length(str) > 0 then
          begin
            InsertBasesAt(i, x0, str);
            UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
          end;
        end;
        if MilliSecondsBetween(Now, updateTime) > 200 then
        begin
          PleaseWait.Action := 'Deleting Gaps ' + IntToStr(i) + '/' + IntToStr(y1 + 1);
          updateTime := now;
          Application.ProcessMessages;
        end;
      end;
    end
    else if (SeqList[Row-FixedRows].NoOfSites > 0) and (Col < SeqList[Row-FixedRows].NoOfSites+FixedCols) then
    begin
      if SeqList[Row-FixedRows][Col-FixedCols+1] = '-' then
        DeleteBasesAt(Row-FixedRows, Col-FixedCols+1, 1)
      else
        quickExit := True;
    end
    else
      quickExit := True;

    if not QuickExit then
    begin
      ClearSelection;
      Col := x0+FixedCols-1;
      Row := y0+FixedRows;
      ResetColCount;
      Invalidate;
    end;
  finally
    if PleaseWait <> nil then
    begin
      PleaseWait.hide;
      PleaseWait.Free;
    end;
  end;
  UpdateHeaderString(x0);
  UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
end;

procedure TAlignGrid.ToUpper;
var
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
  i: integer;
  str, curSeqData: AnsiString;
  gonext: boolean;
  PleaseWait: TPleaseWait = nil;
  ui: TUndoInfo = nil;
begin
  if not AlignEnabled then exit;
  try
    UndoInfoList.ColumnEditedNotify := nil;
    GetRangeOfSelectedSites(x0,x1);
    GetRangeOfSelectedSeqs(y0,y1);
    if BlockSelected or SequenceSelected then
    begin
      PleaseWait := TPleaseWait.Create(nil);
      PleaseWait.Action := 'Capitalizing...';
      PleaseWait.SetShowCancel(False);
      PleaseWait.show;
      gonext := false;
      ui := TUndoInfo.Create;
      ui.Proc := upReplace;
      ui.x0 := x0;
      ui.y0 := y0;
      ui.x1 := x1;
      ui.y1 := y1;
      ui.Pos.X := Col;
      ui.Pos.Y := Row;
      ui.Ori.X := LeftCol;
      ui.Ori.Y := TopRow;
      ui.Sel := Selection;
      ui.goNext := gonext;
      for i := y0 to y1 do
      begin
        if x0 > SeqList[i].NoOfSites then continue;
        str := System.Copy(SeqList[i].SeqData, x0, x1-x0+1);
        curSeqData := SeqList[i].SeqData;
        System.Delete(curSeqData, x0, x1-x0+1); // delete
        System.Insert(UpperCase(str), curSeqData, x0);  // insert capitalized
        SeqList[i].SeqData := curSeqData;
        PleaseWait.Action := 'Capitalizing ' + IntToStr(i) + '/' + IntToStr(y1);
        ui.data.Add(str); // add the old uncapitalized string in case we need to undo.
      end;
      UndoInfoList.Add(ui);
    end
    else if (SeqList[Row-FixedRows].NoOfSites > 0) and (Col < SeqList[Row-FixedRows].NoOfSites+FixedCols) then
    begin
      SeqList[Row-FixedRows][Col-FixedCols+1] := AnsiChar(UpperCase(SeqList[Row-FixedRows][Col-FixedCols+1])[1]);
    end
    else
      exit;
    ClearSelection;
    Col := x0+FixedCols-1;
    Row := y0+FixedRows;
    ResetColCount;
    Invalidate;
  finally
    UpdateHeaderString(1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
    if PleaseWait <> nil then
    begin
      PleaseWait.hide;
      PleaseWait.Free;
    end;
  end;
end;

procedure TAlignGrid.MoveRight;
var
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
  i: integer;
  flag, gonext: boolean;
  gaps: TStringList;
  ui: TUndoInfo;
  s: TGridRect;
begin
  if not AlignEnabled then exit;
  if SequenceSelected then exit;
  GetRangeOfSelectedSites(x0,x1);
  GetRangeOfSelectedSeqs(y0,y1);
  flag := false;
  for i := y0 to y1 do
    if SeqList[i].NoOfSites >= x0 then
    begin
      flag := true;
      break;
    end;
  if not flag then exit;

  flag := false;
  for i := y0 to y1 do
    if (SeqList[i].NoOfSites <= x1) or (SeqList[i][x1+1] <> '-') then
    begin
      flag := true;
      break;
    end;

  try
    UndoInfoList.ColumnEditedNotify := nil;
    gonext := false;
    if flag then
    begin
      for i := y0 to y1 do
        if (SeqList[i].NoOfSites > x1) and (SeqList[i][x1+1] = '-') then
        begin
          DeleteBasesAt(i, x1+1, 1);
          if UndoInfoList.Count > 0 then
            UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
          gonext := true;
        end;
    end
    else
    begin
      ui := TUndoInfo.Create;
      ui.Proc := upDelete;
      ui.x0 := x1+1;
      ui.y0 := y0;
      ui.x1 := x1+1;
      ui.y1 := y1;
      ui.Pos.X := Col;
      ui.Pos.Y := Row;
      ui.Ori.X := LeftCol;
      ui.Ori.Y := TopRow;
      ui.Sel := Selection;
      ui.goNext := gonext;
      for i := y0 to y1 do
        ui.data.Add('-');
      UndoInfoList.Add(ui);
      DeleteBlock(x1+1,y0,x1+1,y1);
      gonext := true;
    end;
    flag := false;
    for i := y0 to y1 do
      if (SeqList[i].NoOfSites < x0)  then
      begin
        flag := true;
        break;
      end;

    if flag then
    begin
      for i := y0 to y1 do
        if (SeqList[i].NoOfSites >= x0)  then
        begin
          InsertBasesAt(i, x0, '-');
          UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
          gonext := true;
        end
    end
    else
    begin
      ui := TUndoInfo.Create;
      ui.Proc := upInsert;
      ui.x0 := x0;
      ui.y0 := y0;
      ui.x1 := x0;
      ui.y1 := y1;
      ui.Pos.X := Col;
      ui.Pos.Y := Row;
      ui.Ori.X := LeftCol;
      ui.Ori.Y := TopRow;
      ui.Sel := Selection;
      ui.goNext := gonext;
      UndoInfoList.Add(ui);

      gaps := TStringList.Create;
      for i := y0 to y1 do
        gaps.Add('-');
      InsertBlock(x0,y0,gaps);
      gaps.Free;
    end;

    s := Selection;
    s.Left := s.Left+1;
    s.Right := s.Right+1;
    ResetColCount;
    Col := s.Right;
    Selection := s;
  finally
    UpdateHeaderString(Selection.Left - 1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.MoveLeft;
var
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
  i: integer;
  flag, gonext: boolean;
  gaps: TStringList;
  ui: TUndoInfo;
  s: TGridRect;
begin
  if not AlignEnabled then exit;
  if SequenceSelected then exit;
  GetRangeOfSelectedSites(x0,x1);
  GetRangeOfSelectedSeqs(y0,y1);
  if x0 = 1 then exit;

  flag := true;
  for i := y0 to y1 do
    if (SeqList[i].NoOfSites = 0) or ((SeqList[i].NoOfSites >= x0-1) and (SeqList[i][x0-1] <> '-')) then
    begin
      flag := false;
      break;
    end;
  if not flag then exit;

  flag := false;
  for i := y0 to y1 do
    if (SeqList[i].NoOfSites <= x1)  then
    begin
      flag := true;
      break;
    end;
  try
    UndoInfoList.ColumnEditedNotify := nil;
    gonext := false;
    if flag then
    begin
      if SeqList[i].NoOfSites > x1  then
      begin
        InsertBasesAt(i, x1+1, '-');
        UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
        gonext := true;
      end;
    end
    else
    begin
      ui := TUndoInfo.Create;
      ui.Proc := upInsert;
      ui.x0 := x1+1;
      ui.y0 := y0;
      ui.x1 := x1+1;
      ui.y1 := y1;
      ui.Pos.X := Col;
      ui.Pos.Y := Row;
      ui.Ori.X := LeftCol;
      ui.Ori.Y := TopRow;
      ui.Sel := Selection;
      ui.goNext := gonext;
      UndoInfoList.Add(ui);
      gonext := true;

      gaps := TStringList.Create;
      for i := y0 to y1 do
        gaps.Add('-');
      InsertBlock(x1+1,y0,gaps);
      gaps.Free;
    end;
    ui := TUndoInfo.Create;
    ui.Proc := upDelete;
    ui.x0 := x0-1;
    ui.y0 := y0;
    ui.x1 := x0-1;
    ui.y1 := y1;
    ui.Pos.X := Col;
    ui.Pos.Y := Row;
    ui.Ori.X := LeftCol;
    ui.Ori.Y := TopRow;
    ui.Sel := Selection;
    ui.goNext := gonext;
    for i := y0 to y1 do
      ui.data.Add('-');
    DeleteBlock(x0-1,y0,x0-1,y1);
    s := Selection;
    s.Left := s.Left-1;
    s.Right := s.Right-1;
    ResetColCount;
    Col := s.Left;
    Selection := s;
    UndoInfoList.Add(ui);
    Invalidate;
  finally
    UpdateHeaderString(s.Left);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
end;

function TAlignGrid.Marked: boolean;
var
  i: integer;
begin
  result := false;
  if not Empty then
    for i := 0 to NoOfSeqs-1 do
      if SeqList[i].MarkedSiteIndex > 0 then
      begin
        result := true;
        break;
      end;
end;

procedure TAlignGrid.MarkSiteAt(SeqIndex, SiteIndex: integer);
begin
  SeqList[SeqIndex].MarkedSiteIndex := SiteIndex;
end;

function TAlignGrid.FindMark: boolean;
var
  lc: integer;
begin
  result := false;
  if SeqList[Row-FixedRows].MarkedSiteIndex = 0 then exit;
  Col := SeqList[Row-FixedRows].MarkedSiteIndex +FixedCols -1;
  if (Col < LeftCol) or (Col >= LeftCol+VisibleColCount) then
  begin
    lc := Col -(VisibleColCount div 2);
    if lc < FixedCols then
      lc := FixedCols
    else if lc > ColCount-VisibleColCount+1 then
      lc := ColCount-VisibleColCount+1;
    LeftCol := lc;
  end;
  result := true;
end;

procedure TAlignGrid.MarkSite;
var
  y0: Integer = -1;
  y1: integer = -1;
  x, i: integer;
  ui: TUndoInfo;
  gonext: boolean;
begin
  if Selection.Left <> Selection.Right then exit;
  x := Selection.Left;
  GetRangeOfSelectedSeqs(y0,y1);

  try
    UndoInfoList.ColumnEditedNotify := nil;
    gonext := false;
    for i := y0 to y1 do
    begin
      if x > SeqList[i].NoOfSites then continue;
      if SeqList[i][x] = '-' then continue;

      ui := TUndoInfo.Create;
      ui.Proc := upMark;
      ui.x0 := SeqList[i].MarkedSiteIndex;
      ui.y0 := i;
      ui.Pos.X := x;
      ui.Pos.Y := i+1;
      ui.Ori.X := LeftCol;
      ui.Ori.Y := TopRow;
      ui.Sel.Left := x;
      ui.Sel.Right := x;
      ui.Sel.Top := i+1;
      ui.Sel.Bottom := i+1;
      ui.goNext := gonext;
      gonext := true;
      UndoInfoList.Add(ui);
      if SeqList[i].MarkedSiteIndex = x-FixedCols+1 then
        MarkSiteAt(i, 0)
      else
        MarkSiteAt(i, x-FixedCols+1);
    end;
  finally
    UpdateHeaderString(1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  ClearSelection;
  Invalidate;
end;

procedure TAlignGrid.MarkSearchBox;
var
  i,x0,y0,y1,x: integer;
  ui: TUndoInfo;
  gonext: boolean;
begin
  if SearchBox = '' then exit;
  x0 := Col;
  y0 := 0;
  y1 := NoOfSeqs-1;
  try
    UndoInfoList.ColumnEditedNotify := nil;
    gonext := false;
    for i := y0 to y1 do
    begin
      x := FindFrom(i, x0);
      if x > 0 then
      begin
        ui := TUndoInfo.Create;
        ui.Proc := upMark;
        ui.x0 := SeqList[i].MarkedSiteIndex;
        ui.y0 := i;
        ui.Pos.X := Col;
        ui.Pos.Y := Row;
        ui.Ori.X := LeftCol;
        ui.Ori.Y := TopRow;
        ui.Sel := Selection;
        ui.goNext := gonext;
        gonext := true;
        UndoInfoList.Add(ui);

        MarkSiteAt(i, x{x0+Pos(SearchBox, str)-1});
      end;
    end;
  finally
    UpdateHeaderString(1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.UnMarkSite;
var
  ui: TUndoInfo;
  gonext: boolean;
  i: integer;
begin
  gonext := false;
  try
    UndoInfoList.ColumnEditedNotify := nil;
    for i := 0 to NoOfSeqs-1 do
      if SeqList[i].MarkedSiteIndex > 0 then
      begin
        ui := TUndoInfo.Create;
        ui.Proc := upMark;
        ui.x0 := SeqList[i].MarkedSiteIndex;
        ui.y0 := i;
        ui.Pos.X := Col;
        ui.Pos.Y := Row;
        ui.Ori.X := LeftCol;
        ui.Ori.Y := TopRow;
        ui.Sel := Selection;
        ui.goNext := gonext;
        UndoInfoList.Add(ui);
        gonext := true;

        MarkSiteAt(i, 0);
      end;
  finally
    UpdateHeaderString(1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.AlignMakedSites;
var
  i,j,max : integer;
  gonext: boolean;
begin
  if not AlignEnabled then exit;
  try
    UndoInfoList.ColumnEditedNotify := nil;
    j := 0;
    for i := 0 to NoOfSeqs-1 do
      if SeqList[i].MarkedSiteIndex > 0 then
      begin
        inc(j);
        if j > 1 then break;
      end;
    if j <= 1 then exit;
    max := 1;
    for i := 0 to NoOfSeqs-1 do
      if SeqList[i].MarkedSiteIndex > 0 then
      begin
        j := SeqList[i].MarkedSiteIndex;
        while (j > 1) and (SeqList[i][j-1] = '-') do
          dec(j);
        if j > max then
          max := j;
      end;
    gonext := false;
    for i := 0 to NoOfSeqs-1 do
      if (SeqList[i].MarkedSiteIndex > 0) and (SeqList[i].MarkedSiteIndex <> max) then
      begin
        if max < SeqList[i].MarkedSiteIndex then
          DeleteBasesAt(i, max, SeqList[i].MarkedSiteIndex-max)
        else
          InsertBasesAt(i, SeqList[i].MarkedSiteIndex, StringOfChar('-', max-SeqList[i].MarkedSiteIndex));
        UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
        gonext := true;
      end;
    ClearSelection;
    ResetColCount;
    Col := max;
  finally
    UpdateHeaderString(Selection.Left - 1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.SetSearchBox(seq: AnsiString);
begin
  FSearchBox := UpperCase(seq);
  while Pos(' ', FSearchBox) > 0 do
    System.Delete(FSearchBox, Pos(' ', FSearchBox), 1);
end;

function TAlignGrid.MatchSearchBox(box: AnsiString):boolean;
var
  i: integer;
begin
  result := false;
  box := uppercase(box);
  if length(box) >= length(SearchBox) then
    if IsDNA then
      for i := 1 to length(SearchBox) do
      begin
        case SearchBox[i] of
          'A': result := (box[i] = 'A');
          'T': result := (box[i] = 'T') or (box[i] = 'U');
          'U': result := (box[i] = 'U') or (box[i] = 'T');
          'C': result := (box[i] = 'C');
          'G': result := (box[i] = 'G');
          'R': result := (box[i] = 'R') or (box[i] = 'A') or (box[i] = 'G');
          'Y': result := (box[i] = 'Y') or (box[i] = 'T') or (box[i] = 'C') or (box[i] = 'U');
          'M': result := (box[i] = 'M') or (box[i] = 'A') or (box[i] = 'C');
          'K': result := (box[i] = 'K') or (box[i] = 'T') or (box[i] = 'G') or (box[i] = 'U');
          'S': result := (box[i] = 'S') or (box[i] = 'C') or (box[i] = 'G');
          'W': result := (box[i] = 'W') or (box[i] = 'A') or (box[i] = 'T') or (box[i] = 'U');
          'B': result := (box[i] = 'B') or (box[i] = 'T') or (box[i] = 'U') or (box[i] = 'C') or (box[i] = 'G') or (box[i] = 'Y') or (box[i] = 'K') or (box[i] = 'S');
          'V': result := (box[i] = 'V') or (box[i] = 'A') or (box[i] = 'C') or (box[i] = 'G')                   or (box[i] = 'R') or (box[i] = 'M') or (box[i] = 'S');
          'D': result := (box[i] = 'D') or (box[i] = 'A') or (box[i] = 'T') or (box[i] = 'U') or (box[i] = 'G') or (box[i] = 'R') or (box[i] = 'K') or (box[i] = 'W');
          'H': result := (box[i] = 'H') or (box[i] = 'A') or (box[i] = 'T') or (box[i] = 'U') or (box[i] = 'C') or (box[i] = 'Y') or (box[i] = 'M') or (box[i] = 'W');
          'N': result := true;
          '-': result := box[i] = '-'
        else
          result := box[i] = SearchBox[i];
        end;
        if not result then break;
      end
    else
      for i := 1 to length(SearchBox) do
      begin
        if SearchBox[i] = 'X' then
          result := true
        else
          result := box[i] = SearchBox[i];
        if not result then break;
      end
  else
    result := false;
end;

function TAlignGrid.SiteInSearchBox(seqindex, siteindex: integer):boolean;
var
  i,j: integer;
  str: AnsiString;
begin
  if Pos('-', SearchBox) > 0 then
  begin
    result := false;
    if siteindex-length(SearchBox) < 0 then
    begin
      j := length(SearchBox) +siteindex -1;
      str := system.copy(SeqList[seqindex].SeqData, 1, j);
    end
    else
      str := system.copy(SeqList[seqindex].SeqData, siteindex-length(SearchBox)+1, length(SearchBox)*2-1);
    for i := 1 to length(str)-length(SearchBox)+1 do
      if MatchSearchBox(system.Copy(str, i, length(SearchBox))) then
      begin
        result := true;
        break;
      end;
    exit;
  end;

  str := '';
  i := siteindex;
  j := 0;
  while (i > 0) and (j < length(SearchBox)) do
  begin
    if SeqList[seqindex].SeqData[i] <> '-' then
    begin
      str := SeqList[seqindex].SeqData[i] +str;
      inc(j);
    end;
    dec(i);
  end;
  if (SeqList[seqindex].SeqData[siteindex] = '-') and (length(str) = length(SearchBox)) then
  begin
    result := false;
    exit;
  end;
  i := siteindex+1;
  j := 0;
  while (i <= SeqList[seqindex].NoOfSites) and (j < length(SearchBox)-1) do
  begin
    if SeqList[seqindex].SeqData[i] <> '-' then
    begin
      str := str +SeqList[seqindex].SeqData[i];
      inc(j);
    end;
    inc(i);
  end;
  result := false;
  if length(str) >= length(SearchBox) then
    for i := 1 to length(str)-length(SearchBox)+1 do
      if MatchSearchBox(system.Copy(str, i, length(SearchBox))) then
      begin
        result := true;
        break;
      end;
end;

function TAlignGrid.FindFrom(seqindex, siteindex: integer):integer;
var
  i,j,k : integer;
  str: AnsiString;
begin
  result := 0;
  if SeqList[seqindex].NoOfSites-siteindex+1 < length(SearchBox)  then exit;

  SetLength(str, length(SearchBox));
  for i := siteindex to SeqList[seqindex].NoOfSites-length(SearchBox)+1 do
  begin
    if SeqList[seqindex].SeqData[i] = '-' then continue;
    j := i;
    k := 0;
    while (j <= SeqList[seqindex].NoOfSites) and (k < length(SearchBox)) do
    begin
      if SeqList[seqindex].SeqData[j] <> '-' then
      begin
        inc(k);
        str[k] := SeqList[seqindex].SeqData[j];
      end;
      inc(j);
    end;
    if MatchSearchBox(str) then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TAlignGrid.FindBackFrom(seqindex, siteindex: integer):integer;
var
  i,j,k : integer;
  str: AnsiString;
begin
  result := 0;
  if siteindex <= 0 then exit;

  SetLength(str, length(SearchBox));
  for i := siteindex downto 1 do
  begin
    if SeqList[seqindex].SeqData[i] = '-' then continue;
    j := i;
    k := 0;
    while (j <= SeqList[seqindex].NoOfSites) and (k < length(SearchBox)) do
    begin
      if SeqList[seqindex].SeqData[j] <> '-' then
      begin
        inc(k);
        str[k] := SeqList[seqindex].SeqData[j];
      end;
      inc(j);
    end;
    if MatchSearchBox(str) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TAlignGrid.FindFromWithGap(seqindex, siteindex: integer):integer;
var
  i : integer;
  str: AnsiString;
begin
  result := 0;
  if SeqList[seqindex].NoOfSites-siteindex+1 < length(SearchBox)  then exit;

  for i := siteindex to SeqList[seqindex].NoOfSites-length(SearchBox)+1 do
  begin
    str := system.copy(SeqList[seqindex].SeqData, i, length(SearchBox));
    if MatchSearchBox(str) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TAlignGrid.FindBackFromWithGap(seqindex, siteindex: integer):integer;
var
  i : integer;
  str: AnsiString;
begin
  result := 0;
  if siteindex <= 0 then exit;
  for i := siteindex downto 1 do
  begin
    str := system.copy(SeqList[seqindex].SeqData, i, length(SearchBox));
    if MatchSearchBox(str) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TAlignGrid.Find: boolean;
var
  x,i,n: integer;
  s: TGridRect;
begin
  result := false;
  if SearchBox = '' then exit;

  if Pos('-', SearchBox) > 0 then
    x := FindFromWithGap(Row-FixedRows, 1)
  else
    x := FindFrom(Row-FixedRows, 1);
  if x > 0 then
  begin
    s.Left   := x+FixedCols-1; // Set cursor to start of found motif.
    if Pos('-', SearchBox) > 0 then
      s.Right := s.Left +Length(SearchBox) -1
    else
    begin
      i := s.Left;
      n := 1;
      while n < Length(SearchBox) do
      begin
        if Sequence[Row-FixedRows].SeqData[i] <> '-' then
          inc(n);
        inc(i);
      end;
      s.Right  := i;
    end;
    s.Top    := Row;
    s.Bottom := Row;
    Selection := s;
  end;
  result := x > 0;
  SetCursorCenter;
  Invalidate;
end;

function TAlignGrid.FindNext: boolean;
var
  x,i,n: integer;
  s: TGridRect;
begin
  result := false;
  if SearchBox = '' then exit;

  if Pos('-', SearchBox) > 0 then
    x := FindFromWithGap(Row-FixedRows, Selection.Left-FixedCols+2)
  else
    x := FindFrom(Row-FixedRows, Selection.Left-FixedCols+2);
  if x > 0 then
  begin
    s.Left   := x+FixedCols-1;
    if Pos('-', SearchBox) > 0 then
      s.Right := s.Left +Length(SearchBox) -1
    else
    begin
      i := s.Left;
      n := 1;
      while n < Length(SearchBox) do
      begin
        if Sequence[Row-FixedRows].SeqData[i] <> '-' then
          inc(n);
        inc(i);
      end;
      s.Right  := i;
    end;
    s.Top    := Row;
    s.Bottom := Row;
    Selection := s;
  end;
  result := x > 0;
  SetCursorCenter;
  Invalidate;
end;

function TAlignGrid.FindPrev: boolean;
var
  x,i,n: integer;
  s: TGridRect;
begin
  result := false;
  if SearchBox = '' then exit;

  if Pos('-', SearchBox) > 0 then
    x := FindBackFromWithGap(Row-FixedRows, Selection.Left-FixedCols)
  else
    x := FindBackFrom(Row-FixedRows, Selection.Left-FixedCols);
  if x > 0 then
  begin
    s.Left   := x+FixedCols-1;
    if Pos('-', SearchBox) > 0 then
      s.Right := s.Left +Length(SearchBox) -1
    else
    begin
      i := s.Left;
      n := 1;
      while n < Length(SearchBox) do
      begin
        if Sequence[Row-FixedRows].SeqData[i] <> '-' then
          inc(n);
        inc(i);
      end;
      s.Right  := i;
    end;
    s.Top    := Row;
    s.Bottom := Row;
    Selection := s;
  end;
  result := x > 0;
  SetCursorCenter;
  Invalidate;
end;

procedure TAlignGrid.DeleteSequenceAT(index: integer);
begin
  SeqList.Delete(index);
  FModified := true;
end;

procedure TAlignGrid.DeleteSequence;
var
  y0: Integer = -1;
  y1: integer = -1;
  i: integer;
  ui: TUndoInfo;
  gonext: boolean;
  s: TGridRect;
begin
  if not SequenceSelected then exit;
  GetRangeOfSelectedSeqs(y0,y1);
  s := Selection;
  ClearSelection;
  Col := LeftCol;
  try
    UndoInfoList.ColumnEditedNotify := nil;
    gonext := false;
    for i := y1 downto y0 do
    begin
      ui := TUndoInfo.Create;
      ui.Proc := upDelSeq;
      ui.x0 := ColWidths[0];
      ui.y0 := i;
      ui.x1 := SeqList[i].MarkedSiteIndex;
      ui.Pos.X := FixedCols;
      ui.Pos.Y := i+FixedRows;
      ui.Ori.X := LeftCol;
      ui.Ori.Y := TopRow;
      ui.Sel := s;
      ui.goNext := gonext;
      gonext := true;
      ui.data.Add(SeqList[i].SeqName);
      ui.data.Add(SeqList[i].SeqInfo);
      ui.data.Add(SeqList[i].UIDS);
      ui.data.Add(SeqList[i].SeqData);
      DeleteSequenceAt(i);
      UndoInfoList.Add(ui);
    end;

    ResetColWidth;
    ResetRowCount;
    ResetColCount;
    if RowCount = y0+FixedRows then
      Row := y0+FixedRows-1
    else
      Row := y0+FixedRows;
  finally
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.InsertSequenceAT(index: integer; seq: TSequence);
begin
  try
    if seq = nil then
      seq := TSequence.Create;
    if index = NoOfSeqs then
      SeqList.Add(seq)
    else
      SeqList.Insert(index, seq);
  finally
    if not Assigned(FUpdateHeaderThread) then
      UpdateHeaderString(1);
    FModified := true;
  end;
end;

procedure TAlignGrid.InsertSequence(seq: TSequence);
var
  ui: TUndoInfo;
  s: TGridRect;
begin
  if NoOfSeqs = 0 then
  begin
    AppendSequence(seq);
    exit;
  end;

  FilterSeqName(seq);

  ui := TUndoInfo.Create;
  ui.Proc := upAddSeq;
  ui.x0 := ColWidths[0];
  ui.y0 := Row-FixedRows;
  ui.Pos.X := Col;
  ui.Pos.Y := Row;
  ui.Ori.X := LeftCol;
  ui.Ori.Y := TopRow;
  ui.Sel := Selection;

  InsertSequenceAt(Row-FixedRows, seq);

  if NoOfSeqs = 1 then ResetSize;

  if ColWidths[0] < DefaultColWidth*Length(seq.SeqName) then
    if DefaultColWidth*Length(seq.SeqName) > width-Canvas.TextWidth('W')-4 then
      ColWidths[0] := width -Canvas.TextWidth('W')-4      // To keep space for at least a single character for sequences
    else
      ColWidths[0] := DefaultColWidth*Length(seq.SeqName);

  ResetRowCount;
  ResetColCount;

  Col := FixedCols;
  s.Left   := 0;
  s.Right  := ColCount-1;
  s.Top    := Row-FixedRows+2;
  s.Bottom := Row-FixedRows+2;
  Selection := s;
  UndoInfoList.Add(ui);
  Invalidate;
end;

procedure TAlignGrid.InsertSequenceList(seqs: TSequenceList);
var
  ui: TUndoInfo;
  i: integer;
  gonext: boolean;
  s: TGridRect;
begin
  if seqs.Count = 0 then exit;
  if NoOfSeqs = 0 then
  begin
    AppendSequenceList(seqs);
    exit;
  end;

  gonext := false;
  for i := seqs.Count-1 downto 0 do
  begin
    FilterSeqName(seqs[i]);

    ui := TUndoInfo.Create;
    ui.Proc := upAddSeq;
    ui.x0 := ColWidths[0];
    ui.y0 := Row-FixedRows;
    ui.Pos.X := Col;
    ui.Pos.Y := Row;
    ui.Ori.X := LeftCol;
    ui.Ori.Y := TopRow;
    ui.Sel := Selection;
    ui.goNext := gonext;
    gonext := true;

    InsertSequenceAt(Row-FixedRows, seqs[i]);

    ResetRowCount;
    ResetColCount;
//    Row := Row+1;
    Col := FixedCols;
    seqs[i] := nil;
  end;

  if NoOfSeqs = 1 then
    ResetSize
  else
    ResetColWidth;

  s.Left   := 0;
  s.Right  := ColCount-1;
  s.Top    := Row;
  s.Bottom := Row+seqs.Count-1;
  Selection := s;
  try
    UndoInfoList.ColumnEditedNotify := nil;
    UndoInfoList.Add(ui);
  finally
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.InsertBlankSequence;
var
  seq: TSequence;
  ui: TUndoInfo;
  i: integer;
begin
  if not EditEnabled then exit;
  if NoOfSeqs = 0 then
  begin
    AppendBlankSequence;
    exit;
  end;

  seq := TSequence.Create;
  i := NoOfSeqs+1;
  repeat
    seq.SeqName := 'Sequence '+IntToStr(i);
    inc(i);
  until CheckSeqName(seq.SeqName) = -1;

  ui := TUndoInfo.Create;
  ui.Proc := upAddSeq;
  ui.x0 := ColWidths[0];
  ui.y0 := Row-FixedRows+1;
  ui.Pos.X := Col;
  ui.Pos.Y := Row;
  ui.Ori.X := LeftCol;
  ui.Ori.Y := TopRow;
  ui.Sel := Selection;

  InsertSequenceAt(Row-FixedRows+1, seq);

  if ColWidths[0] < DefaultColWidth*Length(seq.SeqName) then
    if DefaultColWidth*Length(seq.SeqName) > width-Canvas.TextWidth('W')-4 then
      ColWidths[0] := width -Canvas.TextWidth('W')-4      // To keep space for at least a single character for sequences
    else
      ColWidths[0] := DefaultColWidth*Length(seq.SeqName);

  ResetRowCount;
  ResetColCount;
  Row := Row+1;
  Col := FixedCols;
  UndoInfoList.Add(ui);
  Invalidate;

  if Visible then
    SetFocus;
//  EditSeqName(NoOfSeqs-1);
end;

procedure TAlignGrid.ParseFASTA(sl: TStringList; seqs: TSequenceList; check: boolean);  // sl is input, seqs are parsed sequences out.
var
  seq: TSequence = nil;
  str: AnsiString = '';
  aInfo: AnsiString = '';
  i,j: integer;
  mr: word;
  flag, ignoreall: boolean;
begin
  ignoreall := not check;
  mr := mrIgnore;
  try
    i := 0;
    while i < sl.Count do
      if (Length(sl[i]) > 0) and (sl[i][1] = '>') then
      begin
        seq := TSequence.Create;
        str := Trim(System.Copy(sl[i], 2, Length(sl[i])-1));

        if Pos(' ', str) > 0 then
        begin
          aInfo := System.Copy(str, Pos(' ', str), Length(str) - Pos(' ', str));
          aInfo := Trim(aInfo);
          str := System.Copy(str, 1, Pos(' ', str) - 1);
        end
        else
          aInfo := EmptyStr;

        while Pos('_', str) > 0 do
          str[Pos('_', str)] := ' ';

        seq.SeqName := str;
        if aInfo <> EmptyStr then
          seq.SeqInfo := aInfo;
        str := EmptyStr;
        aInfo := EmptyStr;
        Inc(i);
        while  i < sl.Count do
        begin
          if Length(sl[i]) > 0 then
            if sl[i][1] = '>' then
              break
            else
              str := str +Trim(sl[i]);
          Inc(i);
        end;
        while Pos(' ', str) > 0 do
          System.Delete(str, Pos(' ', str), 1);
        flag := true;
        if Length(str) = 0 then
          flag := false
        else if not ignoreall then
          for j := 1 to Length(str) do
          begin
            mr := mrIgnore;
            if str[j] = '-' then
              continue
            else if IsDNA then
              if (upcase(str[j]) in DNASites) then
                continue
              else if upcase(str[j]) in AASites then
                mr := MessageDlg('You may insert protein data in DNA data. Continue?', mtWarning, [mbCancel, mbIgnore], 0)
              else
                mr := MessageDlg('Invalid character '''+str[j]+''' found. Continue?', mtWarning, [mbCancel, mbIgnore], 0)
            else if not (upcase(str[j]) in AASites) then
              mr := MessageDlg('Invalid character '''+str[j]+''' found. Continue?', mtWarning, [mbCancel, mbIgnore], 0)
            else
              continue;

            if mr = mrIgnore then
              ignoreall := true
            else if mr = mrCancel then
              flag := false;
            break;
          end;

        if mr = mrCancel then
        begin
          seqs.Clear;
          break;
        end
        else if not flag then
        begin
          seq.Free;
          seq := nil;
        end;
        if seq <> nil then seq.SeqData := str;
        if seq <> nil then seqs.Add(seq);
        if seq <> nil then seq := nil;
      end
      else
        Inc(i);
  finally
    if seq <> nil then seq.Free;
  end;
end;

procedure TAlignGrid.InsertSequenceFromClipBoard;

var
  sl: TStringList;
  seqs: TSequenceList;
  seq: TSequence;
  i: integer;

  function CheckSeqs: boolean;
  var
    i: integer;
  begin
    result := true;
    for i := 0 to (sl.Count div 5)-1 do
      if IsDNA and (sl[i*5+3] = '0') then
      begin
        if MessageDlg('You are inserting protein data in DNA data. Continue?', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
          result := false;
        break;
      end
      else if not IsDNA and (sl[i*5+3] = '1') then
      begin
        if MessageDlg('You are inserting DNA data in protein data. Continue?', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
          result := false;
        break;
      end;
  end;

begin
  if not ClipBoard.HasFormat(CF_TEXT) then exit;
  seq := nil;
  sl := TStringList.Create;
  seqs := TSequenceList.Create;

  sl.Text := ClipBoard.AsText;
  if sl.Count < 5 then exit;

  try
    for i := 0 to (sl.Count div 5)-1 do
    begin
      seq := TSequence.Create;
      seq.SeqName := sl[i*5];
      seq.SeqInfo := sl[i*5+1];
      seq.UIDS    := sl[i*5+2];
      seq.SeqData := sl[i*5+4];
      seqs.Add(seq);
      seq := nil;
    end;
    if (seqs.Count > 0) and CheckSeqs then
      InsertSequenceList(seqs);
  finally
    seqs.Free;
    sl.Free;
    if seq <> nil then
      seq.Free;
  end;
end;

procedure TAlignGrid.InsertFASTASequenceFromClipBoard;
var
  sl: TStringList;
  seqs: TSequenceList;
begin
  if not ClipBoard.HasFormat(CF_TEXT) then exit;
  sl := TStringList.Create;
  seqs := TSequenceList.Create;

  sl.Text := ClipBoard.AsText;
  if sl.Count = 0 then exit;

  try
    ParseFASTA(sl, seqs, true);
    if seqs.Count > 0 then
      InsertSequenceList(seqs);
  finally
    seqs.Free;
    sl.Free;
  end;
end;

procedure TAlignGrid.AppendSequence(seq: TSequence);
var
  ui: TUndoInfo;
  s: TGridRect;
begin
  if seq = nil then exit;
  FilterSeqName(seq);

  try
    if FSkipHeaderUpdates then
      UndoInfoList.ColumnEditedNotify := nil;
    ui := TUndoInfo.Create;
    ui.Proc := upAddSeq;
    ui.x0 := ColWidths[0];
    ui.y0 := NoOfSeqs;
    ui.Pos.X := Col;
    ui.Pos.Y := Row;
    ui.Ori.X := LeftCol;
    ui.Ori.Y := TopRow;
    ui.Sel := Selection;
    ui.goNext := false;

    InsertSequenceAt(NoOfSeqs, seq);

    ResetRowCount;
    ResetColCount;
    Row := NoOfSeqs+FixedRows-1;
    Col := FixedCols;


    if NoOfSeqs = 1 then
      ResetSize
    else
      ResetColWidth;

    s.Left   := 0;
    s.Right  := ColCount-1;
    s.Top    := RowCount-1;
    s.Bottom := RowCount-1;
    Selection := s;
    UndoInfoList.Add(ui);
    Invalidate;
  finally
    if FSkipHeaderUpdates then
      UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
end;

procedure TAlignGrid.AppendSequenceList(seqs: TSequenceList);
var
  ui: TUndoInfo;
  i: integer;
  gonext: boolean;
  s: TGridRect;
begin
  if seqs.Count = 0 then exit;
  gonext := false;
  for i := 0 to seqs.Count-1 do
  begin
    FilterSeqName(seqs[i]);

    ui := TUndoInfo.Create;
    ui.Proc := upAddSeq;
    ui.x0 := ColWidths[0];
    ui.y0 := NoOfSeqs;
    ui.Pos.X := Col;
    ui.Pos.Y := Row;
    ui.Ori.X := LeftCol;
    ui.Ori.Y := TopRow;
    ui.Sel := Selection;
    ui.goNext := gonext;
    gonext := true;

    InsertSequenceAt(NoOfSeqs, seqs[i]);

    ResetRowCount;
    ResetColCount;
    Row := NoOfSeqs+FixedRows-1;
    Col := FixedCols;
    seqs[i] := nil;
  end;

  if NoOfSeqs = 1 then
    ResetSize
  else
    ResetColWidth;

  s.Left   := 0;
  s.Right  := ColCount-1;
  s.Top    := RowCount-seqs.Count-FixedRows+1;
  s.Bottom := RowCount-FixedRows;
  Selection := s;
  UndoInfoList.Add(ui);
  Invalidate;
end;

procedure TAlignGrid.AppendBlankSequence;
var
  seq: TSequence;
  ui: TUndoInfo;
  i: integer;
begin
  if not EditEnabled then exit;
  seq := TSequence.Create;
  i := NoOfSeqs+1;
  repeat
    seq.SeqName := 'Sequence '+IntToStr(i);
    inc(i);
  until CheckSeqName(seq.SeqName) = -1;

  ui := TUndoInfo.Create;
  ui.Proc := upAddSeq;
  ui.x0 := ColWidths[0];
  ui.y0 := NoOfSeqs;
  ui.Pos.X := Col;
  ui.Pos.Y := Row;
  ui.Ori.X := LeftCol;
  ui.Ori.Y := TopRow;
  ui.Sel := Selection;

  InsertSequenceAt(NoOfSeqs, seq);

  if NoOfSeqs = 1 then ResetSize;

  if ColWidths[0] < DefaultColWidth*Length(seq.SeqName) then
    if  DefaultColWidth*Length(seq.SeqName) > width-Canvas.TextWidth('W')-4 then
      ColWidths[0] := width -Canvas.TextWidth('W')-4      // To keep space for at least a single character for sequences
    else
      ColWidths[0] := DefaultColWidth*Length(seq.SeqName);

  ResetRowCount;
  ResetColCount;
  Row := NoOfSeqs+FixedRows-1;
  Col := FixedCols;
  UndoInfoList.Add(ui);
  if not Enabled then
    Enabled := True;
  if Visible then
    SetFocus;
//  EditSeqName(NoOfSeqs-1);
end;

procedure TAlignGrid.AppendSequenceFromClipBoard;
var
  sl: TStringList;
  seqs: TSequenceList;
begin
  if not ClipBoard.HasFormat(CF_TEXT) then exit;
  sl := TStringList.Create;
  seqs := TSequenceList.Create;

  sl.Text := ClipBoard.AsText;
  if sl.Count = 0 then exit;
  try
    ParseFASTA(sl, seqs, true);
    AppendSequenceList(seqs);
  finally
    seqs.Free;
    sl.Free;
  end;
end;

procedure TAlignGrid.ReverseCompliment;
var
  str: AnsiString;
  y0: Integer = -1;
  y1: Integer = -1;
  i,lc: integer;
  gonext: boolean;
  pw: TPleaseWait = nil;
  updateTime: TDateTime;
begin
  if not EditEnabled then exit;
  if not IsDNA then exit;
  if not (BlockSelected or SequenceSelected) then exit;
  try
    FSkipHeaderUpdates := True;
    Enabled := False;
    UndoInfoList.ColumnEditedNotify := nil;
    updateTime := Now;
    pw := TPleaseWait.Create(nil);
    pw.Caption := 'Please Wait';
    pw.Action := 'Reverse complimenting selection';
    pw.SetShowCancel(False);
    pw.Show;
    lc := LeftCol;
    GetRangeOfSelectedSeqs(y0, y1);
    gonext := false;
    for i := y0 to y1 do
    begin
      str := SeqList[i].ReverseCompliment;
      DeleteBasesAt(i, 1, SeqList[i].NoOfSites);
      if UndoInfoList.Count > 0 then
        UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
      gonext := true;
      InsertBasesAt(i, 1, str);
      if UndoInfoList.Count > 0 then
        UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
      if MilliSecondsBetween(Now, updateTime) > 200 then
      begin
        pw.Action := Format('Reversing complimenting selection %d%%', [Round((i - y0)/(y1 - y0)*100)]);
        Application.ProcessMessages;
      end;
    end;
    LeftCol := lc;
  finally
    FSkipHeaderUpdates := False;
    Enabled := True;
    if Assigned(pw) then
      pw.Free;
    UpdateHeaderString(Selection.Left - 1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.ReverseSelection;
var
  str: AnsiString;
  i,lc: integer;
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
  gonext: boolean;
  pw: TPleaseWait = nil;
  updateTime: TDateTime;
begin
  if not EditEnabled then exit;
  if not IsDNA then exit;
  if not (BlockSelected or SequenceSelected) then exit;
  try
    FSkipHeaderUpdates := True;
    UndoInfoList.ColumnEditedNotify := nil;
    Enabled := False;
    updateTime := Now;
    pw := TPleaseWait.Create(nil);
    pw.Caption := 'Please Wait';
    pw.Action := 'Reversing selection';
    pw.SetShowCancel(False);
    pw.Show;
    lc := LeftCol;
    GetRangeOfSelectedSites(x0, x1);
    GetRangeOfSelectedSeqs(y0, y1);
    gonext := false;
    for i := y0 to y1 do
    begin
      str := SeqList[i].Reverse(x0, x1);
      DeleteBasesAt(i, x0, x1-x0+1);
      UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
      gonext := true;
      InsertBasesAt(i, x0, str);
      UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
      if MilliSecondsBetween(Now, updateTime) > 200 then
      begin
        pw.Action := Format('Reversing selection %d%%', [Round((i - y0)/(y1 - y0)*100)]);
        Application.ProcessMessages;
      end;
    end;
    LeftCol := lc;
  finally
    FSkipHeaderUpdates := False;
    Enabled := True;
    if Assigned(pw) then
      pw.Free;
    UpdateHeaderString(Selection.Left - 1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
  Invalidate;
end;

procedure TAlignGrid.ComplimentSelection;
var
  str: AnsiString;
  i,lc: integer;
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
  gonext: boolean;
  pw: TPleaseWait = nil;
  updateTime: TDateTime;
begin
  if not EditEnabled then exit;
  if not IsDNA then exit;
  if not (BlockSelected or SequenceSelected) then exit;
  try
    FSkipHeaderUpdates := True;
    UndoInfoList.ColumnEditedNotify := nil;
    Enabled := False;
    updateTime := Now;
    pw := TPleaseWait.Create(nil);
    pw.Caption := 'Please Wait';
    pw.Action := 'Complimenting selection';
    pw.SetShowCancel(False);
    pw.Show;
    lc := LeftCol;
    GetRangeOfSelectedSites(x0, x1);
    GetRangeOfSelectedSeqs(y0, y1);
    gonext := false;
    for i := y0 to y1 do
    begin
      str := SeqList[i].Compliment(x0, x1);
      DeleteBasesAt(i, x0, x1-x0+1);
      UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
      gonext := true;
      InsertBasesAt(i, x0, str);
      UndoInfoList[UndoInfoList.Count-1].goNext := gonext;
      if MilliSecondsBetween(Now, updateTime) > 200 then
      begin
        pw.Action := Format('Complimenting selection %d%%', [Round((i - y0)/(y1 - y0)*100)]);
        Application.ProcessMessages;
      end;
    end;
    LeftCol := lc;
    Invalidate;
  finally
    FSkipHeaderUpdates := False;
    Enabled := True;
    if Assigned(pw) then
      pw.Free;
    UpdateHeaderString(Selection.Left - 1);
    UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
  end;
end;

function TAlignGrid.UndoEnabled: boolean;
begin
  result := (UndoInfoList.Count > 0);
end;

procedure TAlignGrid.Undo;
var
  ui: TUndoInfo;
  seq: TSequence;
  flag: boolean;
  pw: TPleaseWait = nil;
  operations: Integer = 0;
  updateTime: TDateTime;
begin
  if EditBox.Focused then
  begin
    EditBox.Undo;
    exit;
  end;

  try
    if UndoInfoList.Count > 50 then
    begin
      updateTime := Now;
      pw := TPleaseWait.Create(nil);
      pw.Action := 'Undoing last operation';
      pw.SetShowCancel(False);
      pw.Show;
    end;

    while UndoInfoList.Count > 0 do
    begin
      inc(operations);
      ui := UndoInfoList[UndoInfoList.Count-1];
      case ui.Proc of
        upSeqMove:
          if ui.data.Count = 0 then
            if ui.y0 <> ui.y1 then
            begin
              SeqList.Move(ui.y1, ui.y0);
              Ori.X := ui.Pos.X;
              Ori.Y := ui.Pos.Y;
              Cols.Col[0].SortOrder := TColumnsSortOrder(ui.x0);
            end;
        upReplace:
          begin
            // delete the newer text
            DeleteBlock(ui.x0, ui.y0, ui.x1, ui.y1);
            // insert the old saved text
            InsertBlock(ui.x0, ui.y0, ui.data);
          end;
        upInsert:
          begin
            DeleteBlock(ui.x0, ui.y0, ui.x1, ui.y1);
            ResetColCount;
            ResetRowCount;
          end;
        upDelete:
          begin
            InsertBlock(ui.x0, ui.y0, ui.data);
            ResetColCount;
            ResetRowCount;
          end;
        upMark:
          MarkSiteAt(ui.y0, ui.x0);
        upAddSeq:
          begin
            DeleteSequenceAt(ui.y0);
            ColWidths[0] := ui.x0;
            ResetColCount;
            ResetRowCount;
          end;
        upDelSeq:
          begin
            seq := TSequence.Create;
            seq.SeqName := ui.data[0];
            seq.SeqInfo := ui.data[1];
            seq.UIDS    := ui.data[2];
            seq.SeqData := ui.data[3];
            seq.MarkedSiteIndex := ui.x1;
            InsertSequenceAt(ui.y0, seq);
            ColWidths[0] := ui.x0;
            ResetColCount;
            ResetRowCount;
          end;
        upSeqName:
          begin
            SeqList[ui.x0].SeqName := ui.data[0];
            ResetColWidth;
          end;
        upGrpName:
          begin
            SeqList[ui.x0].GroupName := ui.data[0];
            ResetColWidth;
          end;
      end;
      if Assigned(pw) and (MillisecondsBetween(Now, updateTime) > 200) then
      begin
        pw.Action := Format('Undoing last operation %.0n', [operations*1.0]);
        updateTime := Now;
        Application.ProcessMessages;
      end;
      if not ui.goNext then
      begin
        if ui.Pos.X >= ColCount then
          Col := ColCount-1
        else
          Col := ui.Pos.X;
        if ui.Pos.Y >= RowCount then
          Row := RowCount-1
        else
          Row := ui.Pos.Y;
        Selection := ui.Sel;
        LeftCol := ui.Ori.X;
        TopRow  := ui.Ori.Y;
      end;
      flag := ui.goNext;
      UndoInfoList.Delete(UndoInfoList.Count-1);
      if not flag then break;
    end;
  finally
    if not Assigned(FUpdateHeaderThread) then
      UpdateHeaderString(1);
    if Assigned(pw) then
      pw.Free;
  end;

  Invalidate;
  if UndoInfoList.Count = 0 then
    FModified := false;
end;

function TAlignGrid.CopyEnabled: boolean;
var
  i: integer;
begin
  result := false;
  if NoOfSeqs = 0 then exit;

  if SequenceSelected then
  begin
    result := true;
    exit;
  end;

  for i := Selection.Top to Selection.Bottom do
  begin
    if (i <= 0) or (i > SeqList.Count) then continue;
    if Selection.Left-FixedCols < SeqList[i-1].NoOfSites then
    begin
      result := true;
      break;
    end;
  end;
end;

function TAlignGrid.DeleteEnabled: boolean;
begin
  result := EditEnabled;
  if result then
    result := CopyEnabled;
end;

function TAlignGrid.DelGapEnabled: boolean;
var
  i,j: integer;
begin
  result := false;
  if NoOfSeqs = 0 then exit;

  result := AlignEnabled;
  if result then
  begin
    result := false;

    for i := Selection.Top to Selection.Bottom do
    begin
      if i <= 0 then continue;
      for j := Selection.Left-FixedCols+1 to Selection.Right-FixedCols+1 do
      begin
        if j = 0 then continue;
        if (j <= SeqList[i-1].NoOfSites) and (j >= 0) then
          if SeqList[i-1].SeqData[j] = '-' then
          begin
            result := true;
            break;
          end;
      end;
      if result then break;
    end;
  end;
end;

function TAlignGrid.PasteEnabled: boolean;
begin
  result := EditEnabled;
  {$IFDEF DARWIN}
  if Result then
    Result := (ClipBoard.AsText <> EmptyStr);
  {$ELSE}
  if result then
    result := ClipBoard.HasFormat(CF_TEXT);
  {$ENDIF}

  try  // Bug fix/prevention, Users have reported occasional problems when another program has locked the clipboard.  If we try to access the clipboard while locked we get an error, this hides that problem from the users.
    if result and (NoOfSeqs = 0) and (ClipBoard.AsText <> EmptyStr) then
      result := (ClipBoard.AsText[1] = '#') or (ClipBoard.AsText[1] = '>');
  Except on E: Exception do
    result := false;
  end;
end;

procedure TAlignGrid.Copy;
var
  sl: TStringList;
  buffer: PChar;
  i: integer;
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
begin
  if EditBox.Focused then
    EditBox.CopyToClipboard
  else
  begin
    GetRangeOfSelectedSeqs(y0,y1);
    GetRangeOfSelectedSites(x0,x1);
    sl := TStringList.Create;
    if SequenceSelected then
      for i := y0 to y1 do
      begin
        sl.Add('#'+SeqList[i].SeqName);
        sl.Add(SeqList[i].SeqInfo);
        sl.Add(SeqList[i].UIDS);
        if IsDNA then
          sl.Add('1')
        else
          sl.Add('0');
        sl.Add(SeqList[i].SeqData);
      end
    else if BlockSelected then
      for i := y0 to y1 do
        sl.Add(System.Copy(SeqList[i].SeqData, x0, x1-x0+1))
    else if SeqList[Row-FixedRows].NoOfSites > Col-FixedCols then
      sl.Add(SeqList[Row-FixedRows].SeqData[Col-FixedCols+1]);
    buffer := PChar(sl.GetText);
    ClipBoard.SetTextBuf(buffer);

    sl.Free;
  end
end;

procedure TAlignGrid.Cut;
begin
  if not EditEnabled then exit;
  if EditBox.Focused then
    EditBox.CutToClipboard
  else
  begin
    Copy;
    Delete;
  end;
end;

procedure TAlignGrid.Paste;

  function check(sl: TStringList): boolean;
  var
    i,j: integer;
  begin
    result := true;
    for i := 0 to sl.Count-1 do
    begin
      for j := 1 to length(sl[i]) do
        if sl[i][j] = '-' then
          continue
        else if (IsDNA and not (upcase(sl[i][j]) in DNASites)) or (not IsDNA and not (upcase(sl[i][j]) in AASites)) then
        begin
          if MessageDlg('Invalid character found. Continue?', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
            result := false;
          break;
        end;
      if not result then break;
    end;
  end;

var
  sl: TStringList;
//  pos: TPoint;
  i: integer;
begin
  sl := nil;
  if not EditEnabled then exit;
  {$IFNDEF DARWIN}
  if not ClipBoard.HasFormat(CF_TEXT) then exit;
  {$ELSE}
  if (ClipBoard.AsText = EmptyStr) then exit;
  {$ENDIF}
  try
    if EditBox.Focused then
      EditBox.PasteFromClipboard
    else
    begin
      sl := TStringList.Create;
      sl.DelimitedText := ClipBoard.AsText;
      if sl.Count = 0 then exit;

      if sl[0][1] = '#' then
        InsertSequenceFromClipBoard
      else if (sl[0][1] = '>') {and
         (MessageDlg('Pasting FASTA sequence(s)?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) = mrYes)} then
        InsertFASTASequenceFromClipBoard
      else if (NoOfSeqs > 0) and check(sl) then
      begin
        if not SequenceSelected then
          if BlockSelected then
          begin
//            pos.X := Selection.Left;
//            pos.Y := Selection.Top;
            Delete;
//            Col := pos.X;
//            Row := pos.Y;
            i := UndoInfoList.Count;
            Insert(sl);
            UndoInfoList[i].goNext := true;
          end
          else
            Insert(sl)
        else if (Selection.Left = Selection.Right) and (Selection.Top = Selection.Bottom) then
          Insert(sl);
      end;
    end;
  finally
    if sl <> nil then sl.Free;
  end;
end;

procedure TAlignGrid.GetSelectedData(data: TSequenceList);
var
  i: Integer;
  x0: Integer = -1;
  y0: Integer = -1;
  x1: Integer = -1;
  y1: integer = -1;
  seq: TSequence = nil;
begin
  if Empty then exit;
  data.Clear;
  GetRangeOfSelectedSites(x0,x1);
  GetRangeOfSelectedSeqs(y0,y1);
  for i := y0 to y1 do
  begin
    seq := TSequence.Create;
    seq.SeqName := SeqList[i].SeqName;
    seq.GroupName := SeqList[i].GroupName;
    seq.SeqInfo := SeqList[i].SeqInfo;
    seq.UIDS := SeqList[i].UIDS;
    seq.AccessionNum := SeqList[i].AccessionNum;
    seq.SeqData := System.Copy(SeqList[i].SeqData, x0, x1-x0+1);
    data.Add(seq);
  end;
  data.IsDNA := SeqList.IsDNA;
end;

procedure TAlignGrid.GetAllData(var data: TSequenceList);
begin
  data.Clear;
  if Empty then exit;
  data.Assign(SeqList);
end;

function TAlignGrid.CheckSeqName(AName: AnsiString):integer;
var
  i: integer;
begin
  result := -1;
  if NoOfSeqs = 0 then exit;
  for i := 0 to NoOfSeqs-1 do
    if AName = SeqList[i].SeqName then
    begin
      result := i;
      break;
    end;
end;

function TAlignGrid.CheckMEGASeqName(AName: AnsiString): integer;
var
  i: integer;
begin
  result := 0;
  if Length(AName) > 0 then
    if not (AName[1] in ValidOtuNameStartSet) then
      result := 1
    else
      for i := 2 to length(Aname) do
        if not ((AName[i]=' ') or (AName[i] in ValidOtuNameContinueSet)) then
        begin
          result := i;
          break;
        end;
  if result = 0 then
    if Length(AName) > EditBox.MaxLength then
      result := EditBox.MaxLength;
end;

procedure TAlignGrid.FilterSeqName(seq: TSequence);
var
 i: integer;
 str: AnsiString;
begin
  if seq.FSeqName = '' then
  begin
    i := 0;
    repeat
      inc(i);
      str := 'Sequence '+IntToStr(i);
    until CheckSeqName(str) = -1;
    seq.FSeqName := str;
    exit;
  end;

  i := CheckMEGASeqName(seq.FSeqName);
  while i > 0 do
  begin
    if i = EditBox.MaxLength+1 then
      seq.FSeqName := system.copy(seq.FSeqName, 1, EditBox.MaxLength-3)+'...'
    else
      System.Delete(seq.FSeqName, i, 1);
    i := CheckMEGASeqName(seq.FSeqName);
  end;
  if CheckSeqName(seq.FSeqName) > -1 then
  begin
    i := 1;
    repeat
      inc(i);
      str := '('+IntToStr(i)+')';
    until CheckSeqName(system.copy(seq.FSeqName, 1, EditBox.MaxLength-length(str)) +str) = -1;
    seq.FSeqName := system.copy(seq.FSeqName, 1, EditBox.MaxLength-length(str)) +str;
  end;
end;

function TAlignGrid.IsTaxaNameCol(aCol: LongInt): Boolean;
begin
  Result := ((aCol = 0) and (Cols.Col[0].Visible));
end;

function TAlignGrid.IsGroupNameCol(aCol: LongInt): Boolean;
begin
  Result := False;
  case aCol of
    0:
      begin
        Result := ((Cols.Col[0].Visible = False) and (Cols.Col[1].Visible));
      end;
    1:
      begin
        Result := ((Cols.Col[0].Visible = True) and (Cols.Col[1].Visible));
      end;
  end;
end;

function TAlignGrid.IsFixedColumn(aCol: LongInt): Boolean;
begin
  Result := (IsGroupNameCol(aCol) or IsTaxaNameCol(aCol));
end;

function TAlignGrid.CheckFixedColWidth(aCol: LongInt): Boolean;
begin
  Result := Cols.Col[aCol].Visible;
  if aCol <= Cols.Count - 1 then
    if Cols.Col[aCol].Visible then
    begin
      if ColWidths[aCol] <= DefaultColWidth then
        ColWidths[aCol] := Cols.Col[aCol].RestoreWidth;
    end
    else
    begin
      if (ColWidths[aCol] <> 1) and (aCol < Cols.VisibleCount) then
      begin
        Cols.Col[aCol].RestoreWidth := ColWidths[aCol];
        ColWidths[aCol] := 1;
      end;
    end;
  FixedCols := Cols.VisibleCount;
end;

procedure TAlignGrid.UpdateHeaderString(aCol: Int64);
var
  startSite: Int64;
begin
  if not Assigned(HeaderCS) then
    Exit;
  if (aCol > MaxNoOfSites) or (not EditEnabled) then
    Exit;
  //Assert(FUpdateHeaderThread = nil, 'Thread collision updating header string');
  if Assigned(FUpdateHeaderThread) then
  begin
    try
      HeaderCS.Acquire;
      FHeaderStringInvalidFrom := aCol;
      FHeaderStringInvalid := True;
    finally
      HeaderCS.Release;
    end;
    Exit;
  end;
  try
    HeaderCS.Acquire;
    if Trim(FHeaderString) = EmptyStr then
      startSite := 1
    else
      startSite := Max(aCol, 1);
    if Length(FHeaderString) <= MaxNoOfSites then
      SetLength(FHeaderString, MaxNoOfSites + 1);
    FUpdateHeaderThread := TUpdateHeaderStringThread.Create(FHeaderString, SeqList, FixedRowStyle, startSite);
    FUpdateHeaderThread.OnTerminate := UpdateHeaderThreadDone;
    FUpdateHeaderThread.Start;
  finally
    HeaderCS.Release;
  end;
end;

procedure TAlignGrid.UpdateConsensusSequence;
var
  i, j: Integer;
  aCol, aRow: LongInt;
begin
  SetLength(FBaseCounts, MaxNoOfSites);
  for i := 0 to MaxNoOfSites - 1 do
  begin
    if IsDNA then
    begin
      SetLength(FBaseCounts[i], 6);
      for j := 0 to 5 do
        FBaseCounts[i][j] := 0;
    end
    else
    begin
      SetLength(FBaseCounts[i], 22);
      for j := 0 to 21 do
        FBaseCounts[i][j] := 0;
    end;
  end;

  for aCol := 0 to MaxNoOfSites - 1 do
  begin
    for aRow := 0 to NoOfSeqs - 1 do
    begin
      if SeqList[aRow].NoOfSites < (aCol + 1) then
        continue;
      if IsDNA then
      begin
        case upcase(SeqList[aRow].SeqData[aCol + 1]) of
          'A': inc(FBaseCounts[aCol][1]);
          'T': inc(FBaseCounts[aCol][2]);
          'U': inc(FBaseCounts[aCol][2]);
          'C': inc(FBaseCounts[aCol][3]);
          'G': inc(FBaseCounts[aCol][4]);
          '-': inc(FBaseCounts[aCol][5]);
        end;
      end
      else
      begin
        case upcase(SeqList[aRow].SeqData[aCol + 1]) of
          'A': inc(FBaseCounts[aCol][1]);
          'C': inc(FBaseCounts[aCol][2]);
          'D': inc(FBaseCounts[aCol][3]);
          'E': inc(FBaseCounts[aCol][4]);
          'F': inc(FBaseCounts[aCol][5]);
          'G': inc(FBaseCounts[aCol][6]);
          'H': inc(FBaseCounts[aCol][7]);
          'I': inc(FBaseCounts[aCol][8]);
          'K': inc(FBaseCounts[aCol][9]);
          'L': inc(FBaseCounts[aCol][10]);
          'M': inc(FBaseCounts[aCol][11]);
          'N': inc(FBaseCounts[aCol][12]);
          'P': inc(FBaseCounts[aCol][13]);
          'Q': inc(FBaseCounts[aCol][14]);
          'R': inc(FBaseCounts[aCol][15]);
          'S': inc(FBaseCounts[aCol][16]);
          'T': inc(FBaseCounts[aCol][17]);
          'V': inc(FBaseCounts[aCol][18]);
          'W': inc(FBaseCounts[aCol][19]);
          'Y': inc(FBaseCounts[aCol][20]);
          '-': inc(FBaseCounts[aCol][21]);
        end;
      end;
    end;

    //if IsDNA then
    //begin
    //  for i := 1 to 5 do
    //    if round(FBaseCounts[aCol][i]/NoOfSeqs*100) >= ConsensusValue then
    //    begin
    //      imax := i;
    //      break;
    //    end;
    //  case imax of
    //    1: FConsensusSequence[aCol + 1] := 'A';
    //    2: FConsensusSequence[aCol + 1] := 'T';
    //    3: FConsensusSequence[aCol + 1] := 'C';
    //    4: FConsensusSequence[aCol + 1] := 'G';
    //    5: FConsensusSequence[aCol + 1] := '-';
    //  end;
    //end
    //else
    //begin
    //  for i := 1 to 21 do
    //    if round(FBaseCounts[aCol][i]/NoOfSeqs*100) >= ConsensusValue then
    //    begin
    //      imax := i;
    //      break;
    //    end;
    //  case imax of
    //    1: FConsensusSequence[aCol + 1] := 'A';
    //    11: FConsensusSequence[aCol + 1] := 'M';
    //    10: FConsensusSequence[aCol + 1] := 'L';
    //    18: FConsensusSequence[aCol + 1] := 'V';
    //    8: FConsensusSequence[aCol + 1] := 'I';
    //    5: FConsensusSequence[aCol + 1] := 'F';
    //    2: FConsensusSequence[aCol + 1] := 'C';
    //    7: FConsensusSequence[aCol + 1] := 'H';
    //    4: FConsensusSequence[aCol + 1] := 'E';
    //    3: FConsensusSequence[aCol + 1] := 'D';
    //    9: FConsensusSequence[aCol + 1] := 'K';
    //    15: FConsensusSequence[aCol + 1] := 'R';
    //    6: FConsensusSequence[aCol + 1] := 'G';
    //    13: FConsensusSequence[aCol + 1] := 'P';
    //    20: FConsensusSequence[aCol + 1] := 'Y';
    //    17: FConsensusSequence[aCol + 1] := 'T';
    //    12: FConsensusSequence[aCol + 1] := 'N';
    //    16: FConsensusSequence[aCol + 1] := 'S';
    //    14: FConsensusSequence[aCol + 1] := 'Q';
    //    19: FConsensusSequence[aCol + 1] := 'W';
    //    21: FConsensusSequence[aCol + 1] := '-';
    //  end;
    //end;
  end;

  //imax := 0;
  //for i := 1 to 21 do
  //  count[i] := 0;
  //if IsDNA then
  //begin
  //  for i := 0 to NoOfSeqs-1 do
  //  begin
  //    if SeqList[i].NoOfSites <= ACol-FixedCols+1 then
  //      continue;
  //    case upcase(SeqList[i].SeqData[ACol-FixedCols+1]) of
  //      'A': inc(count[1]);
  //      'T': inc(count[2]);
  //      'U': inc(count[2]);
  //      'C': inc(count[3]);
  //      'G': inc(count[4]);
  //      '-': inc(count[5]);
  //    end;
  //  end;
  //  for i := 1 to 5 do
  //    if round(count[i]/NoOfSeqs*100) >= ConsensusValue then
  //    begin
  //      imax := i;
  //      break;
  //    end;
  //end
  //else
  //begin
  //  for i := 0 to NoOfSeqs-1 do
  //  begin
  //    if SeqList[i].NoOfSites <= ACol-FixedCols+1 then
  //      continue;
  //    case upcase(SeqList[i].SeqData[ACol-FixedCols+1]) of
  //      'A': inc(count[1]);
  //      'C': inc(count[2]);
  //      'D': inc(count[3]);
  //      'E': inc(count[4]);
  //      'F': inc(count[5]);
  //      'G': inc(count[6]);
  //      'H': inc(count[7]);
  //      'I': inc(count[8]);
  //      'K': inc(count[9]);
  //      'L': inc(count[10]);
  //      'M': inc(count[11]);
  //      'N': inc(count[12]);
  //      'P': inc(count[13]);
  //      'Q': inc(count[14]);
  //      'R': inc(count[15]);
  //      'S': inc(count[16]);
  //      'T': inc(count[17]);
  //      'V': inc(count[18]);
  //      'W': inc(count[19]);
  //      'Y': inc(count[20]);
  //      '-': inc(count[21]);
  //    end;
  //  end;
  //  for i := 1 to 21 do
  //    if round(count[i]/NoOfSeqs*100) >= ConsensusValue then
  //    begin
  //      imax := i;
  //      break;
  //    end;
  //end;
  //
  //if IsDNA then
  //  case UpCase(SeqList[ARow-FixedRows][ACol-FixedCols+1]) of
  //    'A' : if imax = 1 then flag := true;
  //    'T' : if imax = 2 then flag := true;
  //    'U' : if imax = 2 then flag := true;
  //    'C' : if imax = 3 then flag := true;
  //    'G' : if imax = 4 then flag := true;
  //    '-' : if imax = 5 then flag := true;
  //  end
  //else
  //  case UpCase(SeqList[ARow-FixedRows][ACol-FixedCols+1]) of
  //    'A' : if imax =  1 then flag := true;
  //    'M' : if imax = 11 then flag := true;
  //    'L' : if imax = 10 then flag := true;
  //    'V' : if imax = 18 then flag := true;
  //    'I' : if imax =  8 then flag := true;
  //    'F' : if imax =  5 then flag := true;
  //    'C' : if imax =  2 then flag := true;
  //    'H' : if imax =  7 then flag := true;
  //    'E' : if imax =  4 then flag := true;
  //    'D' : if imax =  3 then flag := true;
  //    'K' : if imax =  9 then flag := true;
  //    'R' : if imax = 15 then flag := true;
  //    'G' : if imax =  6 then flag := true;
  //    'P' : if imax = 13 then flag := true;
  //    'Y' : if imax = 20 then flag := true;
  //    'T' : if imax = 17 then flag := true;
  //    'N' : if imax = 12 then flag := true;
  //    'S' : if imax = 16 then flag := true;
  //    'Q' : if imax = 14 then flag := true;
  //    'W' : if imax = 19 then flag := true;
  //    '-' : if imax = 21 then flag := true;
  //  end;
end;

function TAlignGrid.IsConsensusBase(aBase: AnsiChar; aCol: LongInt): Boolean;
var
  numOfThisBaseAtSite: Integer;
begin
  Result := False;
  if IsDNA then
  begin
    case upcase(aBase) of
      'A': numOfThisBaseAtSite := FBaseCounts[aCol][1];
      'T': numOfThisBaseAtSite := FBaseCounts[aCol][2];
      'U': numOfThisBaseAtSite := FBaseCounts[aCol][2];
      'C': numOfThisBaseAtSite := FBaseCounts[aCol][3];
      'G': numOfThisBaseAtSite := FBaseCounts[aCol][4];
      '-': numOfThisBaseAtSite := FBaseCounts[aCol][5];
    end;
  end
  else
  begin
     case upcase(aBase) of
       'A': numOfThisBaseAtSite := FBaseCounts[aCol][1];
       'C': numOfThisBaseAtSite := FBaseCounts[aCol][2];
       'D': numOfThisBaseAtSite := FBaseCounts[aCol][3];
       'E': numOfThisBaseAtSite := FBaseCounts[aCol][4];
       'F': numOfThisBaseAtSite := FBaseCounts[aCol][5];
       'G': numOfThisBaseAtSite := FBaseCounts[aCol][6];
       'H': numOfThisBaseAtSite := FBaseCounts[aCol][7];
       'I': numOfThisBaseAtSite := FBaseCounts[aCol][8];
       'K': numOfThisBaseAtSite := FBaseCounts[aCol][9];
       'L': numOfThisBaseAtSite := FBaseCounts[aCol][10];
       'M': numOfThisBaseAtSite := FBaseCounts[aCol][11];
       'N': numOfThisBaseAtSite := FBaseCounts[aCol][12];
       'P': numOfThisBaseAtSite := FBaseCounts[aCol][13];
       'Q': numOfThisBaseAtSite := FBaseCounts[aCol][14];
       'R': numOfThisBaseAtSite := FBaseCounts[aCol][15];
       'S': numOfThisBaseAtSite := FBaseCounts[aCol][16];
       'T': numOfThisBaseAtSite := FBaseCounts[aCol][17];
       'V': numOfThisBaseAtSite := FBaseCounts[aCol][18];
       'W': numOfThisBaseAtSite := FBaseCounts[aCol][19];
       'Y': numOfThisBaseAtSite := FBaseCounts[aCol][20];
       '-': numOfThisBaseAtSite := FBaseCounts[aCol][21];
     end;
  end;
  Result := (Round(numOfThisBaseAtSite/NoOfSeqs*100) >= ConsensusValue);
end;

function TAlignGrid.IsSameNucleotide(base1, base2: Char): Boolean;
begin
  Result := False;
  if not (Pos(upcase(base1), 'AGCTU') > 0) then
    Exit;
  if not (Pos(upcase(base2), 'AGCTU') > 0) then
    Exit;
  if upcase(base1) = upcase(base2) then
    Result := True
  else
  begin
    if (upcase(base1) = 'U') or (upcase(base1) = 'T') then
      Result := (upcase(base2) = 'U') or (upcase(base2) = 'T')
  end;
end;

procedure TAlignGrid.UpdateHeaderThreadDone(aThread: TObject);
var
  t: TUpdateHeaderStringThread = nil;
  i: Integer;
  numChars: Integer;
begin
  if not Assigned(HeaderCS) then
    Exit;
  if (not Assigned(FUpdateHeaderThread)) then
    Exit;
  t := TUpdateHeaderStringThread(aThread);
  if FUpdateHeaderThread = aThread then
  begin
    try
      HeaderCS.Acquire;
      FUpdateHeaderThread := nil;
      if t.IsSuccess and (not t.IsCancelled) then
      begin
        numChars := Min(Length(FHeaderString), Length(t.HeaderString));
        if numChars > 0 then
          for i := 1 to numChars do
            FHeaderString[i] := t.HeaderString[i];
      end;
      {$IFDEF MSWINDOWS}
      t.Free;
      {$ELSE}
      t.HeaderString := EmptyStr;
      t.FreeOnTerminate := True;
      t.Terminate;
      {$ENDIF}
      InvalidateRow(0);
    finally
        HeaderCS.Release;
    end;
  end
  else
  begin
    Assert(False, 'unexpected thread in OnThreadDone procecure');
  end;
end;

function TAlignGrid.IsVisibleCell(aCol, aRow: Integer): Boolean;
begin
  if IsFixedColumn(aCol) or (aRow = 0) then
  begin
    Result := True;
    Exit;
  end;
  Result := IsCellVisible(aCol, aRow);
end;

procedure TAlignGrid.ChangeSeqName(index: integer; newname: AnsiString);
var
  ui: TUndoInfo;
begin
  if newname = '' then Exit; // Don't allow the user to give an empty name, this causes errors on export and there's no real reason why there should be a taxa w/o any name
  ui := TUndoInfo.Create;
  ui.Proc := upSeqName;
  ui.x0 := index;
  ui.Pos.X := Col;
  ui.Pos.Y := Row;
  ui.Ori.X := LeftCol;
  ui.Ori.Y := TopRow;
  ui.Sel := Selection;
  ui.data.Add(SeqList[index].SeqName);
  UndoInfoList.Add(ui);

  SeqList[index].SeqName := newname;

  ResetColWidth;

  FModified := true;
end;

function TAlignGrid.CanAlignSelectedData: Boolean;
var
  aList: TSequenceList;
begin
  aList := TSequenceList.Create;
  GetSelectedData(aList);
  Result := aList.HasAtLeast2SeqsWithSitesWithBases;
end;

procedure TAlignGrid.ChangeGrpName(index: integer; newname: AnsiString);
var
  ui: TUndoInfo;
begin
  ui := TUndoInfo.Create;
  ui.Proc := upGrpName;
  ui.x0 := index;
  ui.Pos.X := Col;
  ui.Pos.Y := Row;
  ui.Ori.X := LeftCol;
  ui.Ori.Y := TopRow;
  ui.Sel := Selection;
  ui.data.Add(SeqList[index].GroupName);
  UndoInfoList.Add(ui);

  SeqList[index].GroupName := newname;

  ResetColWidth;

  FModified := true;
end;

procedure TAlignGrid.EditSeqName(SeqIndex: integer);
var
  ARect: TRect;
begin
  if not EditEnabled then exit;
  if (SeqIndex < 0) or (SeqIndex >= NoOfSeqs) then exit;
  ClearSelection;
  ARect := CellRect(0, SeqIndex+1);   // the cell where the inplace edit shows
  EditBox.OnExit := EditBoxOnExit;
  EditBox.OnKeyDown := EditBoxOnKeyDown;
  EditBox.Left := ARect.Left;
  EditBox.Top := Arect.Top;
  EditBox.Width := Arect.Right -ARect.Left;
  EditBox.Height := RowHeights[Row] + 6;
  EditBox.Text := SeqList[SeqIndex].SeqName;
  EditBox.Show;
  EditBox.SetFocus;
end;

procedure TAlignGrid.EditGrpName(SeqIndex: integer);
var
  ARect: TRect;
begin
  if not EditEnabled then exit;
  if (SeqIndex < 0) or (SeqIndex >= NoOfSeqs) then exit;
  ClearSelection;
  ARect := CellRect(1, SeqIndex+1);   // the cell where the inplace edit shows
  EditBox.OnExit := EditBoxOnGroupExit;
  EditBox.OnKeyDown := EditBoxOnGroupKeyDown;
  EditBox.Left := ARect.Left;
  EditBox.Top := Arect.Top;
  EditBox.Width := Arect.Right -ARect.Left;
  EditBox.Height := RowHeights[Row] + 6;
  EditBox.Text := SeqList[SeqIndex].GroupName;
  EditBox.Show;
  EditBox.SetFocus;
end;


procedure TAlignGrid.EditBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    EditBoxOnExit(Sender)
  else
    inherited;
end;

procedure TAlignGrid.EditBoxOnGroupKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    EditBoxOnGroupExit(Sender)
  else
    inherited;
end;

procedure TAlignGrid.EditBoxOnExit(Sender : TObject);
var
  i: integer;
begin
  try
    if Trim(EditBox.Text) <> SeqList[MouseCoord(EditBox.Left+1, EditBox.Top+1).y-1].SeqName then
    begin
      i := CheckMEGASeqName(EditBox.Text);
      if i > 0 then
      begin
        if i = 1 then
          MessageDlg(''''+EditBox.Text[1]+''' is forbidden for the first character in the MEGA format.', mtError, [mbOK], 0)
        else
          MessageDlg('A forbidden character '''+EditBox.Text[i]+''' is found.', mtError, [mbOK], 0);
        EditBox.SelStart := i-1;
        EditBox.SelLength := 1;
        try
          EditBox.Visible := True;
          EditBox.Enabled := True;
          EditBox.SetFocus;
        except
          on E:Exception do
            {$IFDEF DEBUG}
            ShowMessage('Error in TAlignGrid: ' + E.Message);
            {$ENDIF}
          // some times there is a mysterious 'cannot focus a disabled or invisible window error'
        end;
        exit;
      end;
      i := CheckSeqName(EditBox.Text);
      if i = -1 then
        ChangeSeqName(MouseCoord(EditBox.Left+1, EditBox.Top+1).y-1, Trim(EditBox.Text))
      else
      begin
        MessageDlg('The sequence name already exists. An unique name is required.', mtError, [mbOK], 0);
        EditBox.Visible := True;
        EditBox.SelectAll;
        try
          EditBox.SetFocus;
        except
          on E:Exception do
          {$IFDEF DEBUG}
          ShowMessage('Error in TAlignGrid: ' + E.Message);
          {$ENDIF}
          // some times there is a mysterious 'cannot focus a disabled or invisible window error'
        end;
        exit;
      end;

    end;

    EditBox.Hide;

    Col := LeftCol;
    SetFocus;
  except
    {$IFDEF DEBUG}
     on E:Exception do
       ShowMessage('A crash bug exists in MAlignGrid: ' + E.Message);
    {$ENDIF}
  end;
end;

procedure TAlignGrid.EditBoxOnGroupExit(Sender : TObject);
var
  i: integer;
begin
  if Trim(EditBox.Text) <> SeqList[MouseCoord(EditBox.Left+1, EditBox.Top+1).y-1].GroupName then
  begin
    i := CheckMEGASeqName(EditBox.Text);
    if i > 0 then
    begin
      if i = 1 then
        MessageDlg(''''+EditBox.Text[1]+''' is forbidden for the first character in the MEGA format.', mtError, [mbOK], 0)
      else
        MessageDlg('A forbidden character '''+EditBox.Text[i]+''' is found.', mtError, [mbOK], 0);
      EditBox.SelStart := i-1;
      EditBox.SelLength := 1;
      try
        EditBox.Visible := True;
        EditBox.Enabled := True;
        EditBox.SetFocus;
      except
        on E:Exception do
        {$IFDEF DEBUG}
        ShowMessage('Error in TAlignGrid: ' + E.Message);
        {$ENDIF}
        // some times there is an error 'cannot focus on a disabled or invisible window'
      end;
      exit;
    end;
    ChangeGrpName(MouseCoord(EditBox.Left+1, EditBox.Top+1).y-1, Trim(EditBox.Text))
  end;
  EditBox.Hide;
  Col := LeftCol;
  SetFocus;
end;

procedure TAlignGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  s: TGridRect;
  x0,y0: integer;
begin
  if Assigned(ClickNotify) then
    ClickNotify(Self);
  if Empty then exit;
  if EditBox.Visible then
  begin
    EditBox.Hide; // Same as calling exit, but allows you to call the proper onExit procedure (groups).
    if EditBox.Visible then
    begin
      inherited MouseDown(Button, Shift, X, Y);
      exit;
    end;
  end;

  if Y > GridHeight then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    exit;
  end;
  x0 := MouseCoord(X, Y).X;
  y0 := MouseCoord(X, Y).Y;
  if (Button = mbLeft) and (ssCtrl in Shift) then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    if x0 > 0 then
      MarkSite;
  end
  else if (Button = mbLeft) and (ssShift in Shift) then
  begin
    if (x0 <= Cols.Count-1) and (x0 >= 0) and (y0 > 0) and (y0 <= NoOfSeqs) then // if it is part of a column
      if SequenceSelected and (Ori.Y > 0) and (goRangeSelect in Options) then
      begin
        s := Selection;
        if y0 < Ori.Y then
        begin
          s.Top    := y0;
          s.Bottom := Ori.Y;
        end
        else
        begin
          s.Top    := Ori.Y;
          s.Bottom := y0;
        end;
        Selection := s;
      end
      else
        SelectSequenceAt(y0-1)
    else if (y0 = 0) and (y0 <= MaxNoOfSites) and (x0 > 0) and (Ori.X > 0) then
      if SiteSelected then
      begin
        s := Selection;
        if x0 < Ori.X then
        begin
          s.Left  := x0;
          s.Right := Ori.X;
        end
        else
        begin
          s.Left  := Ori.X;
          s.Right := x0;
        end;
        Selection := s;
      end
      else
        SelectSiteAt(x0);
    inherited MouseDown(Button, Shift, X, Y);
  end
  else if (Button = mbLeft) and (ssDouble in Shift) then
  begin
    if (x0 = 0) and (y0 > 0) and (y0 <= NoOfSeqs) and EditEnabled then
      EditSeqName(y0-1);
    if (x0 = 1) and (y0 > 0) and (y0 <= NoOfSeqs) and EditEnabled then
      EditGrpName(y0-1);
  end
  else if Button = mbLeft then
  begin
    if (y0 = 0) and (x0 <= MaxNoOfSites) and (not IsFixedColumn(x0)) then
      SelectSiteAt(x0)
    else if (y0 = 0) and (x0 = MaxNoOfSites + 1) then { bug fix - adding the groups column made it not possible to select the last column by clicking on the header}
      SelectSiteAt(x0)
    else if (x0 < Cols.Count) and (y0 >= FixedRows) then
    begin
      if SequenceSelected and (Selection.Top <= y0) and (Selection.Bottom >= y0) then
      begin
        s.Top    := y0;
        s.Bottom := y0;
        s.Left   := LeftCol;
        s.Right  := LeftCol;
        Selection := s;
        Ori.X := LeftCol;
        Ori.Y := y0;
      end
      else
        SelectSequenceAt(y0-FixedRows);
    end
    else if y0 >= FixedRows then
      Ori.Y := y0;
    inherited MouseDown(Button, Shift, X, Y);
  end
  else if (x0 >= 0) and (y0 >= 0) then
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TAlignGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  s: TGridRect;
  x0, y0: integer;
begin
  if Y > GridHeight then exit;
  s := Selection;
  inherited;
  if X < ColWidths[0] then
    Selection := s;
  if (Shift = [ssLeft]) and (goRangeSelect in Options) and SiteSelected then
  begin
    x0 := MouseCoord(X, Y).X;
    y0 := MouseCoord(X, Y).Y;
    if (y0 > 0) and (Ori.Y > 0) then
      Exit;
    if ((ScrollBars = ssHorizontal) or (ScrollBars = ssAutoBoth) or (ScrollBars = ssBoth)) then
      if (X >= ClientWidth) then
      begin
        if (LeftCol+VisibleColCount-1 <= MaxNoOfSites) then
          LeftCol := LeftCol +1;
      end
      else if (x0 < LeftCol) and (LeftCol > Cols.Count) then
      begin
        LeftCol := LeftCol -1;
        x0 := LeftCol;
      end;

    if (x0-FixedCols >= 0) and (x0-FixedCols <= MaxNoOfSites) then
    begin
      s := Selection;
      if x0 < Ori.X then
      begin
        s.Left  := x0;
        s.Right := Ori.X;
      end
      else
      begin
        s.Left  := Ori.X;
        s.Right := x0;
      end;
      Selection := s;
    end;
  end;
end;

procedure TAlignGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  s: TGridRect;
  x,y: integer;
begin
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
  if Empty then exit;

  if Shift = [ssAlt] then
  begin
    case Key of
      VK_LEFT :  MoveLeft;
      VK_RIGHT:  MoveRight;
      VK_UP   :  MoveSequenceUp;
      VK_DOWN :  MoveSequenceDown;
    else
      inherited;
    end;
  end
  else if ssShift in Shift then
    inherited
  else if Shift = [ssCtrl] then
  begin
    case Key of
      VK_DELETE: DeleteGaps;
      VK_LEFT:
        if SiteSelected then
        begin
          if Selection.Left > 1 then
          begin
            s := Selection;
            if s.Left-VisibleColCount < 1 then
              s.Left  := 1
            else
              s.Left  := s.Left -VisibleColCount;
            s.Right := s.Left;
            Selection := s;
          end
        end
        else
          inherited;
      VK_RIGHT:
        if SiteSelected then
        begin
          if Selection.Right < MaxNoOfSites then
          begin
            s := Selection;
            if s.Left+VisibleColCount > MaxNoOfSites then
              s.Left  := MaxNoOfSites
            else
              s.Left  := s.Left +VisibleColCount;
            s.Right := s.Left;
            Selection := s;
          end
        end
        else
          inherited;
      VK_UP:
        if SequenceSelected then
        begin
          if Selection.Top > 1 then
          begin
            s := Selection;
            if s.Top-VisibleRowCount < 1 then
              s.Top := 1
            else
              s.Top  := s.Top -VisibleRowCount;
            s.Bottom := s.Top;
            Selection := s;
          end
        end
        else
          inherited;
      VK_DOWN:
        if SequenceSelected then
        begin
          if Selection.Bottom < NoOfSeqs then
          begin
            s := Selection;
            if s.Top+VisibleRowCount > NoOfSeqs then
              s.Top    := NoOfSeqs
            else
              s.Top  := s.Top   +VisibleRowCount;
            s.Bottom := s.Top;
            Selection := s;
          end
        end
        else
          inherited;
    else
      inherited;
    end;
    if ((Key = VK_LEFT) or (Key = VK_RIGHT)) and SiteSelected then
    begin
      x := LeftCol;
      while Selection.Left < x do
        if x <= VisibleColCount then
        begin
          x := 1;
          break;
        end
        else
          x := x -VisibleColCount;
      while Selection.Left >= x+VisibleColCount do
        if x+2*VisibleColCount >= MaxNoOfSites then
        begin
          x := MaxNoOfSites -VisibleColCount +1;
          break;
        end
        else
          x := x +VisibleColCount;
      LeftCol := x;
    end
    else if ((Key = VK_UP) or (Key = VK_DOWN)) and SequenceSelected then
    begin
      y := TopRow;
      while Selection.Top < y do
        if y <= VisibleRowCount then
        begin
          y := 1;
          break;
        end
        else
          y := y -VisibleRowCount;
      while Selection.Top >= y+VisibleRowCount do
        if y+2*VisibleRowCount >= NoOfSeqs then
        begin
          y := NoOfSeqs -VisibleRowCount +1;
          break;
        end
        else
          y := y +VisibleRowCount;
      TopRow := y;
    end;

  end
  else if Shift = [] then
  begin
    case Key of
      VK_LEFT:
        if SiteSelected then
        begin
          if Selection.Left > 1 then
          begin
            s := Selection;
            s.Left := s.Left-1;
            s.Right := s.Right-1;
            Selection := s;
            if Selection.Left < LeftCol then
              LeftCol := Selection.Left;
          end
        end
        else if SequenceSelected then
          Col := LeftCol+VisibleColCount-1
        else
          inherited;
      VK_RIGHT:
        if SiteSelected then
        begin
          if Selection.Right < MaxNoOfSites then
          begin
            s := Selection;
            s.Left := s.Left+1;
            s.Right := s.Right+1;
            Selection := s;
            if Selection.Right >= LeftCol+VisibleColCount then
              LeftCol := Selection.Right-VisibleColCount;
          end
        end
        else if SequenceSelected then
          Col := LeftCol
        else
          inherited;
      VK_UP:
        if SequenceSelected then
        begin
          if Selection.Top > 1 then
          begin
            s := Selection;
            s.Top := s.Top-1;
            s.Bottom := s.Bottom-1;
            Selection := s;
            if Selection.Top < TopRow then
              TopRow := Selection.Top;
          end
        end
        else if SiteSelected then
          Row := TopRow+VisibleRowCount-1
        else
          inherited;
      VK_DOWN:
        if SequenceSelected then
        begin
          if Selection.Bottom < NoOfSeqs then
          begin
            s := Selection;
            s.Top := s.Top+1;
            s.Bottom := s.Bottom+1;
            Selection := s;
            if Selection.Bottom >= TopRow+VisibleRowCount then
              TopRow := Selection.Bottom-VisibleRowCount;
          end
        end
        else if SiteSelected then
          Row := TopRow
        else
          inherited;
      VK_DELETE:
        if SequenceSelected then
          if not EditEnabled then
            exit
          else
            Delete
        else if (not EditBox.Visible) then
          if EditEnabled then
            Delete
          else if AlignEnabled then
            DeleteGaps
          else
            inherited
        else
          inherited;
      VK_BACK:
        if not BlockSelected and (Col > FixedCols) then
           if EditEnabled or (AlignEnabled and  (SeqList[Row-FixedRows][Col-FixedCols] = '-')) then
           begin
             try
               UndoInfoList.ColumnEditedNotify := nil;
               DeleteBasesAt(Row-FixedRows, Col-FixedCols, 1);
               Col := Col-1;
               ResetColCount;
               SetLength(FHeaderString, MaxNoOfSites);
             finally
               UpdateHeaderString(Col - FixedCols);
               UndoInfoList.ColumnEditedNotify := UpdateHeaderString;
             end;
             Invalidate;
           end
           else
             inherited
        else
          inherited;
      VK_ESCAPE:
        if SequenceSelected or BlockSelected then
          ClearSelection;
    else
      inherited;
    end;
  end
  else
    inherited;
  Invalidate;
end;

procedure TAlignGrid.KeyPress(var KeyW: Char);
var
  Key: AnsiChar;
begin
  Key := AnsiChar(KeyW);
  inherited;
  if Empty then exit;

  if  (Key = '-') or (Key = ' ') then
  begin
    if AlignEnabled then
      InsertGaps;
  end
  else if EditEnabled then
    if (IsDNA and (UpCase(Key) in DNASites)) or ((not IsDNA) and (UpCase(Key) in AASites)) then
    begin
      InsertBasesAt(Row-FixedRows, Col-FixedCols+1, Key);
      ResetColCount;
      if Col >= SeqList[Row-FixedRows].NoOfSites+FixedCols then
        Col := SeqList[Row-FixedRows].NoOfSites+FixedCols
      else
        Col := Col+1;
    end;
  Invalidate;
end;

procedure TAlignGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
begin
  if (not Visible) or (not IsVisibleCell(ACol, ARow)) then
    Exit;

  if ARow = 0 then
    DrawHeaderCell(ACol, ARect, AState)
  else if IsTaxaNameCol(ACol) then
    DrawTaxaNameCell(ARow, ARect, AState)
  else if IsGroupNameCol(aCol) then
    DrawGroupNameCell(ACol, ARow, ARect, AState)
  else
    DrawAlignmentCell(ACol, ARow, ARect, AState);
end;

procedure TAlignGrid.DrawTaxaNameCell(ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  aText: String;
  x, y: Integer;
  aSeq: TSequence = nil;
  oriFontStyle: TFontStyles;
begin
  if not Visible then Exit;
  if (not Assigned(SeqList)) or ((ARow - FixedRows) > SeqList.Count) or (SeqList.Count = 0) then
  begin
    with Canvas do
    begin
      Brush.Color := FixedHotColor;
      FillRect(aRect);
      DrawFixedCellBorder(0, aRow, aRect);
    end;
  end
  else
  begin
    aSeq := SeqList[ARow-FixedRows];
    if Assigned(aSeq) then
      aText := IntToStr(ARow) + '. ' + aSeq.SeqName;
    x := aRect.Left + 2;
    y := aRect.Top + 2;

    with Canvas do
    begin
      oriFontStyle := Font.Style;
      if RowIsSelected(aRow) then
      begin
        Brush.Color := FixedHotColor;
        Font.Color := clBlue;
        Font.Style := [fsBold];
      end
      else
      begin
        Brush.Color := FixedColor;
        Font.Color := clBlack;
      end;
      Brush.Style := bsSolid;
      FillRect(aRect);
      TextOut(x, y, aText);
      Font.Style := oriFontStyle;
    end;
    DrawFixedCellBorder(0, aRow, aRect);
  end;
end;

procedure TAlignGrid.DrawGroupNameCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  aText: String;
  x, y: Integer;
  aSeq: TSequence;
  oriFontStyle: TFontStyles;
begin
  if not Visible then Exit;
  if (not Assigned(SeqList)) or (SeqList.Count = 0) or ((ARow - FixedRows) > SeqList.Count) then
  begin
    with Canvas do
    begin
      Brush.Color := FixedHotColor;
      FillRect(aRect);
    end;
  end
  else
  begin
    aSeq := SeqList[ARow-FixedRows];
    if Assigned(aSeq) then
      aText := aSeq.GroupName;
    x := aRect.Left + 2;
    y := aRect.Top + 4;
    with Canvas do
    begin
      oriFontStyle := Font.Style;
      if RowIsSelected(aRow) then
      begin
        Brush.Color := FixedHotColor;
        Font.Color := clBlue;
        Font.Style := [fsBold];
      end
      else
      begin
        Brush.Color := FixedColor;
        Font.Color := clBlack;
      end;
      Brush.Style := bsSolid;
      FillRect(aRect);
      TextOut(x, y, aText);
      Font.Style := oriFontStyle;
    end;
  end;
  DrawFixedCellBorder(ACol, aRow, aRect);
end;

procedure TAlignGrid.DrawHeaderCell(ACol: Longint; ARect: TRect; AState: TGridDrawState);
var
  aText: String;
  x, y: Integer;
begin
  if not Visible then Exit;
  if IsFixedColumn(aCol) then
  begin
    if not CheckFixedColWidth(aCol) then { then it is hidden}
      Exit;
  end;
  if ColCount > FixedCols then
    ColWidths[FixedCols] := DefaultColWidth;

  if IsFixedColumn(ACol) then
  begin
    aText := Cols.Col[ACol].ColName;
    x := ARect.Left + 2;
  end
  else
  begin
    if aCol > Length(FHeaderString) then
      UpdateHeaderString(ACol-FixedCols+1);
    if Length(FHeaderString) >= (ACol - FixedCols + 1) then
      aText := FHeaderString[ACol-FixedCols+1]
    else
      aText := ' ';
    x := aRect.Left + Round((aRect.Right - aRect.Left)/2) - Round(Canvas.TextWidth(aText) / 2);
  end;
  y := ARect.Top + 2;

  with Canvas do
  begin
    if gdSelected in AState then
      Brush.Color := FixedHotColor
    else
      Brush.Color := FixedColor;
    Brush.Style := bsSolid;
    FillRect(ARect);
    TextOut(x, y, aText);
  end;
  if IsFixedColumn(ACol) then
    DrawSortArrow(ACol, ARect)
  else
    DrawFixedCellBorder(aCol, 0, aRect);
end;

procedure TAlignGrid.DrawAlignmentCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  ch : AnsiChar;
  flag, isConsensus: Boolean;
  x, y: Integer;
begin
  if not Visible then
    Exit;
  Canvas.Font.Style := [];
  if (ARow > SeqList.Count) or (NoOfSeqs = 0) then
  begin
    if IsCellSelected[aCol, aRow] then
      Canvas.Brush.Color := clRed
    else
      Canvas.Brush.Color := clCream;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ARect);
    DrawDataCellBorder(ACol, ARow, ARect);
    Exit;
  end;

  if EditBox.Visible and Selected(ACol,ARow) then
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(ARect);
  end;

  if ACol-FixedCols+1 <= NoOfSites[ARow-FixedRows] then
  begin
    flag := false;
    IsConsensus := False;
    if MarkConsensus then
      IsConsensus := IsConsensusBase(SeqList[ARow-FixedRows][ACol-FixedCols+1], ACol-FixedCols);

    if Enabled and (not Selected(ACol,ARow)) or EditBox.Visible then
    begin
      if ShowColor then
        if (ShowBGColor and (not IsConsensus)) or (not ShowBGColor and IsConsensus) then
          if IsDNA then
            Canvas.Brush.Color := GetBGColorForNucleotide(SeqList[ARow-FixedRows][ACol-FixedCols+1])
          else
            Canvas.Brush.Color := GetBGColorForAminoAcid(SeqList[ARow-FixedRows][ACol-FixedCols+1])
        else
          Canvas.Brush.Color := clWhite
      else if (IsConsensus and not ShowBGColor) or (not IsConsensus and ShowBGColor)  then
        Canvas.Brush.Color := clBlack
      else
        Canvas.Brush.Color := clWhite;
    end
    else
      Canvas.Brush.Color := SearchHighlightBgColor;

    flag := Canvas.Brush.Color <> clWhite;
    Canvas.FillRect(ARect);

    if Enabled then
      if (ACol-FixedCols+1 = SeqList[ARow-FixedRows].MarkedSiteIndex) and (not Selected(ACol,ARow)) then
      begin
        if ShowBGColor then
          Canvas.Brush.Color := clWhite
        else if ShowColor then
        begin
          if IsDNA then
            Canvas.Brush.Color := GetBGColorForNucleotide(SeqList[ARow-FixedRows][ACol-FixedCols+1])
          else
            Canvas.Brush.Color := GetBGColorForAminoAcid(SeqList[ARow-FixedRows][ACol-FixedCols+1])
        end
        else
          Canvas.Brush.Color := clBlack;
        Canvas.FillRect(ARect);
        flag := true;
      end
      else if HighlightSearchBox and (SearchBox <> '') then
        if not Selected(ACol,ARow) then
          if SiteInSearchBox(ARow-FixedRows, ACol-FixedCols+1) then
          begin
            Canvas.Brush.Color := SearchHighlightBgColor;
            Canvas.Font.Color := SearchHighlightFontColor;
            Canvas.FillRect(ARect);
            flag := true;
          end;
    ch := SeqList[ARow-FixedRows][ACol-FixedCols+1];
    if not Enabled then
      Canvas.Font.Color := clGray
    else if Selected(ACol,ARow) and not EditBox.Visible then
    begin
      if ACol-FixedCols+1 = SeqList[ARow-FixedRows].MarkedSiteIndex then
        Canvas.Font.Color := clBlue
      else if HighlightSearchBox and SiteInSearchBox(ARow-FixedRows, ACol-FixedCols+1) then
      begin
        Canvas.Font.Color := SearchHighlightFontColor;
        Canvas.Font.Style := [fsBold, fsItalic];
      end
      else
      begin
        Canvas.Font.Color := clBlue;
        Canvas.Brush.Color := RGBToColor(243, 244, 181);
      end;
      flag := Canvas.Brush.Color <> clWhite;
      Canvas.FillRect(ARect);
    end
    else if not ShowColor then
      if Canvas.Brush.Color = clBlack then
        Canvas.Font.Color := clWhite
      else if flag then
        Canvas.Font.Color := clBlue
      else
        Canvas.Font.Color := clBlack
    else if Canvas.Brush.Color <> clWhite then
      Canvas.Font.Color := clBlack
    else if IsDNA then
      Canvas.Font.Color := GetFontColorForNucleotide(ch)
    else
      Canvas.Font.Color := GetFontColorForAminoAcid(ch);
    x := aRect.Left + Round((DefaultColWidth-Canvas.TextWidth(ch))/2);
    y := aRect.Top + Round((DefaultRowHeight - Canvas.TextHeight(ch))/2);
    Canvas.TextOut(x, y, ch);
  end
  else if ForceAlignment and (ACol < MaxNoOfSites+FixedCols) then
  begin
    ch := '-';
    if Selected(ACol,ARow) then
    begin
      Canvas.Font.Color := clWhite;
      Canvas.Brush.Color := clMedGray;
    end
    else
    begin
      Canvas.Brush.Color := clWhite;
      Canvas.Font.Color := clMedGray;
    end;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ARect);
    x := Round((DefaultColWidth-Canvas.TextWidth(ch))/2);
    y := Round((DefaultRowHeight - Canvas.TextHeight(ch))/2);
    Canvas.TextOut(x, y, ch);
  end
  else
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ARect);
  end;
  if ACol = Col then
    DrawCurrentColumnBorder(ARect);
  if (ACol = Col) and (ARow = Row) then
    DrawDataCellBorder(ACol, ARow, ARect);
end;

procedure TAlignGrid.DrawSortArrow(aCol: LongInt; aRect: TRect);
begin
  with Canvas do
  begin
    { draw the triangle fill}
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    Pen.Color := clBlack;
    case Cols.Col[ACol].SortOrder of
      soAscending:
        begin
          FArrow[0] := Point(ARect.Right-14, ARect.Bottom -6);
          FArrow[1] := Point(ARect.Right-6, ARect.Bottom -6);
          FArrow[2] := Point(ARect.Right-10, ARect.Top +6);
        end;
      soDescending:
        begin
          FArrow[0] := Point(ARect.Right-14, ARect.Top +6);
          FArrow[1] := Point(ARect.Right-6, ARect.Top +6);
          FArrow[2] := Point(ARect.Right-10, ARect.Bottom -6);

        end;
    end;
    Canvas.Polygon(FArrow);

    { draw the triangle border}
    Brush.Style := bsClear;
    if Cols.Col[ACol].isDown then
      Canvas.Pen.Color := clSilver
    else
      Canvas.Pen.Color := clWhite;
    Canvas.Pen.Width := 2;
    Canvas.MoveTo(ARect.Left+1, ARect.Top);
    Canvas.LineTo(ARect.Right-1, ARect.Top); // Top Line
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(ARect.Left, ARect.Top);
    Canvas.LineTo(ARect.Left, ARect.Bottom); // Left Line

    if not Cols.Col[ACol].isDown then
    begin
      Canvas.Pen.Color := clSilver;
      Canvas.Pen.Width := 2;
      Canvas.MoveTo(ARect.Left, ARect.Bottom-1);  // Bottom line
      Canvas.LineTo(ARect.Right-1, ARect.Bottom-1);
      Canvas.MoveTo(ARect.Right-1, ARect.Top);    // Right line
      Canvas.LineTo(ARect.Right-1, ARect.Bottom-1);
      Canvas.Pen.Width := 1;
    end;
  end;
end;

procedure TAlignGrid.DrawFixedCellBorder(aCol, aRow: LongInt; aRect: TRect);
var
  points: array[0..2] of TPoint;
begin
  points[0].x := aRect.Left;
  points[0].y := aRect.Bottom - 1;
  points[1].x := aRect.Right - 1;
  points[1].y := aRect.Bottom - 1;
  points[2].x := aRect.Right - 1;
  points[2].y := aRect.Top;
  with Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Color := clActiveBorder;
    {$IFDEF UNIX}
    Pen.Color := clScrollBar;
    {$ENDIF}
    Pen.Style := psSolid;
    Pen.Width := 1;
    Polyline(points);
  end;
end;

procedure TAlignGrid.DrawDataCellBorder(aCol, aRow: LongInt; aRect: TRect);
var
  points: array[0..4] of TPoint;
begin
  points[0].x := aRect.Left;
  points[0].y := aRect.Bottom - 1;
  points[1].x := aRect.Right - 1;
  points[1].y := aRect.Bottom - 1;
  points[2].x := aRect.Right - 1;
  points[2].y := aRect.Top;
  points[3].x := aRect.Left;
  points[3].y := aRect.Top;
  points[4].x := points[0].x;
  points[4].y := points[0].y;
  with Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Color := clBlue;
    Pen.Style := psDot;
    Pen.Width := 1;
    Polyline(points);
  end;
end;

procedure TAlignGrid.DrawCurrentColumnBorder(aRect: TRect);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Pen.Color := clBlack;
    Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom);
    Line(aRect.Right - 1, aRect.Top, aRect.Right - 1, aRect.Bottom);
  end;
end;

function TAlignGrid.SiteIndexToColumn(site: Integer): Integer;
begin
  result := site + Cols.VisibleCount - 1;
end;

procedure TAlignGrid.SortByName(ASortOrder: TColumnsSortOrder);
var
  sl: TStringList;
  i,j: integer;
  flag: boolean;
begin
  if FixSequenceOrder then
    exit;
  sl := TStringList.Create;
  for i := 0 to NoOfSeqs-1 do
    sl.Add(Sequence[i].SeqName);

  sl.Sort;

  ClearSelection;

  flag := false;
  if ASortOrder = soAscending then
  begin
    for i := 0 to NoOfSeqs-2 do
    begin
      if Sequence[i].SeqName = sl[i] then
        continue;
      for j := i+1 to NoOfSeqs-1 do
        if Sequence[j].SeqName = sl[i] then
        begin
          MoveRow(j+1, i+1);
          UndoInfoList[UndoInfoList.Count-1].goNext := flag;
          flag := true;
          break;
        end;
    end;
    Cols.Col[0].SortOrder := soAscending;
  end
  else
  begin
    for i := NoOfSeqs-1 downto 1 do
    begin
      if Sequence[NoOfSeqs-1-i].SeqName = sl[i] then
        continue;
      for j := NoOfSeqs-i to NoOfSeqs-1 do
        if Sequence[j].SeqName = sl[i] then
        begin
          MoveRow(j+1, NoOfSeqs-i);
          UndoInfoList[UndoInfoList.Count-1].goNext := flag;
          flag := true;
          break;
        end;
    end;
    Cols.Col[0].SortOrder := soDescending;
  end;
  
  sl.Free;

  Invalidate;
end;

procedure TAlignGrid.OnInvalidate;
begin
  if MarkConsensus then
    UpdateConsensusSequence;
end;

procedure TAlignGrid.InitFont;
begin
  Font.Name := 'Open Sans';
  Font.Size := 8;
  Font.Color := clBlack;
  Font.CharSet := DEFAULT_CHARSET;
end;

function TAlignGrid.RowIsSelected(aRow: Integer): Boolean;
begin
  Result := ((Selection.Left = FixedCols) and
             (Selection.Right = (ColCount - 1)) and
             ((aRow >= Selection.Top) and (aRow <= Selection.Bottom)));
end;

function TAlignGrid.RectsAreEqual(r1: TGridRect; r2: TGridRect): Boolean;
begin
  Result := False;
  if r1.Top <> r2.Top then
    Exit;
  if r1.Bottom <> r2.Bottom then
    Exit;
  if r1.Left <> r2.Left then
    Exit;
  if r1.Right <> r2.Right then
    Exit;
  Result := True;
end;

function TAlignGrid.DataTypeString: String;
begin
  if IsProteinCoding or IsDNA then
    Result := 'dna'
  else
    Result := 'protein';
end;

procedure TAlignGrid.Invalidate;
begin
  OnInvalidate;
  inherited Invalidate;
end;



end.
