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

unit MTraceEdit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  LCLIntF, LCLType, Messages, SysUtils, Types, Classes, Graphics, Controls, Forms,
  ExtCtrls, Clipbrd, Math, StdCtrls, MGlobalSettings, LMessages;

type
  TArrayOfByte = array [0..MaxInt-1] of byte;
  PArrayOfByte = ^TArrayOfByte;

  TTrace = array [0..3] of array of smallint;
  TProb = array [0..3] of array of double;

  TTraceData = class
  private
    FNoOfBases: integer;
    FWidth: integer;
    FFileName: String;

    procedure SetNoOfBases(n: integer);
    procedure SetWidth(n: integer);

    function GetUnmaskedBases: AnsiString;

    procedure LoadFromSCFFile(buffer: PArrayOfByte; dataname: AnsiString);
    procedure LoadFromABIFile(buffer: PArrayOfByte);
  public
    Name: AnsiString;
    Trace: TTrace;
    Prob: TProb;
    Base: array of AnsiChar;
    Peak: array of integer;
    Score: array of integer;
    MaxPeak: integer;
    LeftClip: integer;
    RightClip: integer;

    property FileName: String read FFileName write FFileName;

    property NoOfBases: integer read FNoOfBases write SetNoOfBases;
    property Width: integer read FWidth write SetWidth;
    property UnmaskedBases: AnsiString read GetUnmaskedBases;

    procedure SetMaxpeak;
    procedure SetProbAt(index: integer);
    procedure SetScoreAt(index: integer);
    procedure SetProb;
    procedure SetScore;

    procedure ReverseComplement;

    function LoadFromFile(filename:String): boolean;
    procedure SaveToFile(filename: String);
  end;


  TEditAction = (eaDelete, eaChange, eaInsert, eaReverse, eaMaskLeft, eaMaskRight, eaClearMask);

  TUndoItem = class
  private
  public
    Action: TEditAction;
    Index : integer;
    Base  : AnsiChar;
    Peak  : integer;
    Score : integer;
    Prob  : array [0..3] of double;
    LMask : integer;
    RMask : integer;
    goNext: boolean;
  end;

  TUndoList = class(TList)
  private
    function GetItems(Index:integer): TUndoItem;
    procedure SetItems(Index:integer; AItem: TUndoItem);
  public
    property Items[index: integer]:TUndoItem read GetItems write SetItems;

    procedure Delete(Index: integer);
    procedure Clear; override;

    destructor Destroy; override;
  end;

  TTraceEdit = class(TCustomControl)
  private
    fCanvas: TControlCanvas;
    MousePos: TPoint;
    ScrollPos: TPoint;
    DragOri: TPoint;
    Points: array [0..3] of array of TPoint;
    PP: array of TPoint;

    FBorderStyle: TBorderStyle;
    FScrollBars: TScrollStyle;

    FData: TTraceData;
    FSelStart: integer;
    FSelLength: integer;
    FCursor: integer;
    FModified: boolean;
    FEditEnabled: boolean;
    FOnChange: TNotifyEvent;
    FOnMoved: TNotifyEvent;
    FHorzScale: double;
    FVertScale: double;
    FMaskColor: TColor;
    FLineWidth: integer;
    ScrollLimit: integer;
    Draging: boolean;
    BaseSelecting : boolean;
    OriBasePos: integer;

    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
//    procedure SetScrollBars(Value: TScrollStyle);
    procedure UpdateScrollRange;

    procedure SetData(newdata: TTraceData);
    procedure SetPoints;

    procedure SetSelStart(value: integer);
    procedure SetSelLength(value: integer);
    procedure SetCursor(value: integer);
    procedure DrawVertLine(x,y: integer);
    procedure DrawDragCursor;
    procedure SetHorzScale(value: double);
    procedure SetVertScale(value: double);

    function GetPitch: integer;
    function GetEmpty: boolean;

    function GetHorzPosition: integer;
    procedure SetHorzPosition(value: integer);

    procedure CMWantSpecialKey(var Msg: TLMessage); message CM_WANTSPECIALKEY;
  protected
    procedure SetBorderStyle(Value: TBorderStyle); override;
    procedure DeleteBaseCustom(index: integer);
    procedure InsertBaseCustom(index: integer; base: AnsiChar);
    procedure ChangeBaseCustom(base: AnsiChar);
    procedure ReverseComplementCustom;
    procedure MaskLeftCustom(index: integer);
    procedure MaskRightCustom(index: integer);

    procedure CreateParams(var Params: TCreateParams);  override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    UndoList: TUndoList;
    property Data: TTraceData read FData write SetData;
    property Empty: boolean read GetEmpty;
    property HorzPosition: integer read GetHorzPosition write SetHorzPosition;
    property Pitch: integer read GetPitch;
    property SelStart: integer read FSelStart write SetSelStart;
    property SelLength: integer read FSelLength write SetSelLength;
    property Cursor: integer read FCursor write SetCursor;
    property Modified: boolean read FModified;

    function PosToSite(pos: integer):integer;
    function SelectedSequence: AnsiString;

    procedure CopySequenceToClipboard;
    procedure CopyBasesToClipboard;
    procedure CopySelectionToClipboard;

    procedure DeleteBase;
    procedure InsertBase(base: AnsiChar);
    procedure ChangeBase(base: AnsiChar);
    procedure ReverseComplement;
    procedure Undo;

    procedure MaskLeft;
    procedure MaskRight;
    procedure ClearLeftMask;
    procedure ClearRightMask;
    procedure ClearMask;

    function Find(target: AnsiString): boolean;
    function FindNext(target: AnsiString): boolean;
    function FindPrev(target: AnsiString): boolean;

    procedure Paint; override;
    procedure DrawTraceData(ACanvas: TCanvas; x0, x1, x, y: integer);

    function LoadFromFile(filename: String): boolean;
    procedure SaveToFile(filename: String);
    procedure ExportToFASTAFile(filename: String);

    procedure Clear;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property EditEnabled: boolean read FEditEnabled write FEditEnabled;
    property HorzScale: double read FHorzScale write SetHorzScale;
    property VertScale: double read FVertScale write SetVertScale;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;

    property MaskColor: TColor read FMaskColor write FMaskColor;

    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property LineWidth: integer read FLineWidth write FLineWidth;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

const
  A: integer = 0;
  C: integer = 1;
  G: integer = 2;
  T: integer = 3;

function Complement(base: AnsiChar):AnsiChar;
begin
  case base of
    'T': result := 'A';
    'C': result := 'G';
    'A': result := 'T';
    'G': result := 'C';
    'U': result := 'A';
    'R': result := 'Y';
    'Y': result := 'R';
    'M': result := 'K';
    'K': result := 'M';
    'S': result := 'S';
    'W': result := 'W';
    'V': result := 'B';
    'D': result := 'H';
    'B': result := 'V';
    'H': result := 'D';
    'N': result := 'N';
    't': result := 'a';
    'c': result := 'g';
    'a': result := 't';
    'g': result := 'c';
    'u': result := 'a';
    'r': result := 'y';
    'y': result := 'r';
    'm': result := 'k';
    'k': result := 'm';
    's': result := 's';
    'w': result := 'w';
    'v': result := 'b';
    'd': result := 'h';
    'b': result := 'v';
    'h': result := 'd';
    'n': result := 'n';
  else
    result := '?';
  end;
end;

/////////////////
// TTraceData
/////////////////

procedure TTraceData.SetNoOfBases(n: integer);
begin
  FNoOfBases := n;
  SetLength(Peak, n);
  SetLength(Base, n);
  SetLength(Score, n);
  SetLength(Prob[A], n);
  SetLength(Prob[C], n);
  SetLength(Prob[G], n);
  SetLength(Prob[T], n);
end;

procedure TTraceData.SetWidth(n: integer);
begin
  FWidth := n;
  SetLength(Trace[A], n);
  SetLength(Trace[C], n);
  SetLength(Trace[G], n);
  SetLength(Trace[T], n);
end;

procedure TTraceData.SetProbAt(index: integer);
var
  j,n: integer;
begin
  n := 0;
  for j := A to T do
    n := n +Trace[j][Peak[index]];
  for j := A to T do
    if n <=  0 then
      Prob[j][index] :=  0
    else
      Prob[j][index] := Trace[j][Peak[index]]/n;
end;

procedure TTraceData.SetProb;
var
  i: integer;
begin
  if NoOfBases > 0 then
    for i := 0 to NoOfBases-1 do
      SetProbAt(i);
end;

procedure TTraceData.SetScoreAt(index: integer);
var
  p,p0,p1,d0,d1: integer;
  f: double;
begin
  if index = 0 then
  begin
    p0 := (3*Peak[0] -Peak[1]) div 2;
    if p0 < 0 then p0 := 0;
    p1 := (Peak[0] +Peak[1]) div 2;
  end
  else if index = NoOfBases-1 then
  begin
    p0 := (Peak[index] +Peak[index-1]) div 2;
    p1 := (3*Peak[index] -Peak[index-1]) div 2;
    if p1 >= Width then p1 := Width-1;
  end
  else
  begin
    p0 := (Peak[index] +Peak[index-1]) div 2;
    p1 := (Peak[index] +Peak[index+1]) div 2;
  end;

  case upcase(Base[index]) of
    'A': begin
           d0 := Trace[A][Peak[index]] -Trace[A][p0];
           d1 := Trace[A][Peak[index]] -Trace[A][p1];
           p :=  Trace[A][Peak[index]];
           f :=  Prob[A][index];
         end;
    'C': begin
           d0 := Trace[C][Peak[index]] -Trace[C][p0];
           d1 := Trace[C][Peak[index]] -Trace[C][p1];
           p :=  Trace[C][Peak[index]];
           f :=  Prob[C][index];
         end;
    'G': begin
           d0 := Trace[G][Peak[index]] -Trace[G][p0];
           d1 := Trace[G][Peak[index]] -Trace[G][p1];
           p :=  Trace[G][Peak[index]];
           f :=  Prob[G][index];
         end;
    'T', 
	'U': begin
           d0 := Trace[T][Peak[index]] -Trace[T][p0];
           d1 := Trace[T][Peak[index]] -Trace[T][p1];
           p :=  Trace[T][Peak[index]];
           f :=  Prob[T][index];
         end;
  else
    d0 := 0;
    d1 := 0;
    p := -1;
    f := 0;
  end;

  if p < 0.1 then
    Score[index] := 0
  else
    Score[index] := trunc((d0+d1)/2/p*MaxPeak*f);

  if Score[index] > MaxPeak then
     Score[index] := MaxPeak;
end;

procedure TTraceData.SetScore;
var
  i: integer;
begin
  for i := 0 to NoOfBases-1 do
    SetScoreAt(i);
end;

procedure TTraceData.SetMaxpeak;
var
  i: integer;
begin
  MaxPeak := 0;
  for i := 0 to Width-1 do
  begin
    if Trace[A][i] > MaxPeak then
      MaxPeak := Trace[A][i];
    if Trace[C][i] > MaxPeak then
      MaxPeak := Trace[C][i];
    if Trace[G][i] > MaxPeak then
      MaxPeak := Trace[G][i];
    if Trace[T][i] > MaxPeak then
      MaxPeak := Trace[T][i];
  end;
end;

procedure TTraceData.ReverseComplement;
var
  i,j: integer;
  r: double;
  b: AnsiChar;
begin
  if Width > 0 then
    for i := 0 to Width-1 do
    begin
      j := Trace[A][i];
      Trace[A][i] := Trace[T][Width-i-1];
      Trace[T][Width-i-1] := j;
      j := Trace[C][i];
      Trace[C][i] := Trace[G][Width-i-1];
      Trace[G][Width-i-1] := j;
    end;
  if NoOfBases > 0 then
  begin
    for i := 0 to NoOfBases-1 do
    begin
      r := Prob[A][i];
      Prob[A][i] := Prob[T][NoOfBases-i-1];
      Prob[T][NoOfBases-i-1] := r;
      r := Prob[C][i];
      Prob[C][i] := Prob[G][NoOfBases-i-1];
      Prob[G][NoOfBases-i-1] := r;

      j := Score[i];
      Score[i] := Score[NoOfBases-i-1];
      Score[NoOfBases-i-1] := j;
    end;
    for i := 0 to NoOfBases div 2 -1 do
    begin
      j := Peak[i];
      Peak[i] := Width-Peak[NoOfBases-i-1]-1;
      Peak[NoOfBases-i-1] := Width-j-1;
      b := Base[i];
      Base[i] := Complement(Base[NoOfBases-i-1]);
      Base[NoOfBases-i-1] := Complement(b);
    end;
    if NoOfBases mod 2 = 1 then
    begin
      Peak[NoOfBases div 2] := Width-Peak[NoOfBases div 2]-1;
      Base[NoOfBases div 2] := Complement(Base[NoOfBases div 2]);
    end;
  end;
  i := LeftClip;
  j := RightClip;
  LeftClip  := NoOfBases -j +1;
  RightClip := NoOfBases -i +1;
end;

function TTraceData.GetUnmaskedBases: AnsiString;
var
  i: integer;
begin
  setlength(result, RightClip-LeftClip-1);
  for i := LeftClip to RightClip-2 do
    result[i-LeftClip+1] := Base[i];
end;

function TTraceData.LoadFromFile(filename: String): boolean;
var
  F: file;
  buffer: PArrayOfByte;
  name, header: AnsiString;
  i,fs: integer;

  procedure DeleteMacBinary;
  var
    i: integer;
  begin
    for i := 0 to fs-129 do
      buffer[i] := buffer[i+128];
  end;

begin
  result := false;
  AssignFile(F, filename);
  FileMode := fmOpenRead;
  Reset(F, 1);
  FileMode := fmOpenReadWrite;
  fs := FileSize(F);
  GetMem(buffer, fs);
  for i := 0 to fs-1 do
    BlockRead(F, buffer[i], 1);
  CloseFile(F);

  header := AnsiChar(buffer[0])+AnsiChar(buffer[1])+AnsiChar(buffer[2])+AnsiChar(buffer[3]);
  if (header = '.scf') and (strtoInt(AnsiChar(buffer[36])) >= 3) then
  begin
    name := ExtractFileName(filename);
    if ExtractFileExt(filename) <> '' then
      name := System.Copy(name, 1, Length(name)-Length(ExtractFileExt(filename)));

    LoadFromSCFFile(buffer, name);
    result := true;
  end
  else if header = 'ABIF' then
  begin
    LoadFromABIFile(buffer);
    result := true;
  end
  else if fs > 132 then
  begin
    header := AnsiChar(buffer[128])+AnsiChar(buffer[129])+AnsiChar(buffer[130])+AnsiChar(buffer[131]);
    if (header = '.scf') and (strtoInt(AnsiChar(buffer[36])) >= 3) then
    begin
      name := ExtractFileName(filename);
      if ExtractFileExt(filename) <> '' then
        name := System.Copy(name, 1, Length(name)-Length(ExtractFileExt(filename)));

      DeleteMacBinary;
      LoadFromSCFFile(buffer, name);
      result := true;
    end
    else if header = 'ABIF' then
    begin
      DeleteMacBinary;
      LoadFromABIFile(buffer);
      result := true;
    end;
  end;

  if RightClip = 0 then
    RightClip := NoOfBases+1;

  FreeMemAndNil(buffer);

  SetMaxpeak;
//  SetProb;
//  SetScore;

  if result then
    FFileName := filename;
end;

procedure TTraceData.LoadFromSCFFile(buffer: PArrayOfByte; dataname: AnsiString);

type
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
    i: integer;
    p,n: ^integer;
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

    if header[40] = 2 then
    begin
      n := addr(header[4]);
      p := addr(header[8]);
      for i := 0 to 4*n^-1 do
      begin
        b := header[p^+i*2];
        header[p^+i*2] := header[p^+i*2+1];
        header[p^+i*2+1] := b;
      end;
    end;
    n := addr(header[12]);
    p := addr(header[24]);
    for i := 0 to n^-1 do
    begin
      b := header[p^+i*4];
      header[p^+i*4] := header[p^+i*4+3];
      header[p^+i*4+3] := b;
      b := header[p^+i*4+1];
      header[p^+i*4+1] := header[p^+i*4+2];
      header[p^+i*4+2] := b;
    end;
  end;

type
  psmallint = ^smallint;
  pinteger  = ^integer;

var
  DataHeader: PDataHeader;
  i,j,p: integer;
  k : smallint;
begin
  ConvHeader(buffer);
  DataHeader := PDataHeader(buffer);

  Name := dataname;
  Width := DataHeader.samples;
  NoOfBases := DataHeader.bases;

  LeftClip  := DataHeader.bases_left_clip;
  RightClip := DataHeader.bases_right_clip;

  // retrieve trace data
  p := DataHeader.samples_offset;
  if DataHeader.sample_size = 2 then
  begin
    for i := 0 to Width-1 do
      Trace[A][i] := psmallint(addr(buffer[p+i*2]))^;
    p := p +Width*2;
    for i := 0 to Width-1 do
      Trace[C][i] := psmallint(addr(buffer[p+i*2]))^;
    p := p +Width*2;
    for i := 0 to Width-1 do
      Trace[G][i] := psmallint(addr(buffer[p+i*2]))^;
    p := p +Width*2;
    for i := 0 to Width-1 do
      Trace[T][i] := psmallint(addr(buffer[p+i*2]))^;
  end
  else
  begin
    for i := 0 to Width-1 do
      Trace[A][i] := smallint(buffer[p+i]);
    p := p +Width;
    for i := 0 to Width-1 do
      Trace[C][i] := smallint(buffer[p+i]);
    p := p +Width;
    for i := 0 to Width-1 do
      Trace[G][i] := smallint(buffer[p+i]);
    p := p +Width;
    for i := 0 to Width-1 do
      Trace[T][i] := smallint(buffer[p+i]);
  end;

  // restore the original magnitudes
  for j := A to T do
  begin
    k := 0;
    for i := 0 to Width-1 do
    begin
      Trace[j][i] := Trace[j][i] +k;
      k := Trace[j][i];
    end;
    k := 0;
    for i := 0 to Width-1 do
    begin
      Trace[j][i] := Trace[j][i] +k;
      k := Trace[j][i];
    end;
  end;

  // retrieve peak indices
  p := DataHeader.bases_offset;
  for i := 0 to NoOfBases-1 do
    Peak[i] := pinteger(addr(buffer[p+i*4]))^;

  // retrieve base letters
  p := DataHeader.bases_offset+NoOfBases*8;
  for i := 0 to NoOfBases-1 do
    Base[i] := AnsiChar(buffer[p+i]);
end;

procedure TTraceData.LoadFromABIFile(buffer: PArrayOfByte);

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
    dataoffset: array[0..3] of AnsiChar;
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
  DataRecord: ^TDataRecordArray;
  p: ^integer;
  s: ^smallint;
  b4: array[0..3] of byte;
  i,j,n: integer;
  fwo: array[0..3] of integer;
  b : byte;
begin
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

  for i := 0 to n-1 do
    with DataRecord[i] do
      if tagname[0]+tagname[1]+tagname[2]+tagname[3] = 'FWO_' then
      begin
        case AnsiChar(dataoffset[0]) of
         'A': fwo[0] := 0;
         'C': fwo[0] := 1;
         'G': fwo[0] := 2;
         'T', 
		 'U': fwo[0] := 3;
        end;
        case AnsiChar(dataoffset[1]) of
         'A': fwo[1] := 0;
         'C': fwo[1] := 1;
         'G': fwo[1] := 2;
         'T', 
		 'U': fwo[1] := 3;
        end;
        case AnsiChar(dataoffset[2]) of
         'A': fwo[2] := 0;
         'C': fwo[2] := 1;
         'G': fwo[2] := 2;
         'T', 
		 'U': fwo[2] := 3;
        end;
        case AnsiChar(dataoffset[3]) of
         'A': fwo[3] := 0;
         'C': fwo[3] := 1;
         'G': fwo[3] := 2;
         'T', 
		 'U': fwo[3] := 3;
        end;
      end;
  for i := 0 to n-1 do
    with DataRecord[i] do
      if tagname[0]+tagname[1]+tagname[2]+tagname[3] = 'SMPL' then
        if datalen > 4 then
          for j := 1 to buffer[integer(dataoffset)] do
            Name := Name +AnsiChar(buffer[integer(dataoffset)+j])
        else
        begin
          for j := 1 to integer(dataoffset[0]) do
            Name := Name +dataoffset[j];
        end
      else if (tagname[0]+tagname[1]+tagname[2]+tagname[3] = 'PBAS') and (tagindex = 1) then
      begin
        NoOfBases := elementnum;
        for j := 0 to elementnum-1 do
          Base[j] := AnsiChar(buffer[integer(dataoffset)+j]);
      end
      else if (tagname[0]+tagname[1]+tagname[2]+tagname[3] = 'PLOC') and (tagindex = 1) then
      begin
        NoOfBases := elementnum;
        for j := 0 to elementnum-1 do
        begin
          b := buffer[integer(dataoffset)+j*2];
          buffer[integer(dataoffset)+j*2] := buffer[integer(dataoffset)+j*2+1];
          buffer[integer(dataoffset)+j*2+1] := b;
          s := addr(buffer[integer(dataoffset)+j*2]);
          Peak[j] := s^;
        end;
      end
      else if (tagname[0]+tagname[1]+tagname[2]+tagname[3] = 'DATA') and (tagindex >= 9) and (tagindex <= 12) then
      begin
        Width := elementnum;
        for j := 0 to elementnum-1 do
        begin
          b := buffer^[integer(dataoffset)+j*2];
          buffer[integer(dataoffset)+j*2] := buffer[integer(dataoffset)+j*2+1];
          buffer[integer(dataoffset)+j*2+1] := b;
          s := addr(buffer[integer(dataoffset)+j*2]);
          Trace[fwo[tagindex-9]][j] := s^;
        end;
      end;

  LeftClip  := 0;
  RightClip := 0;
end;

procedure TTraceData.SaveToFile(filename: String);
type
  Tb2 = array[0..1] of byte;
  Tb4 = array[0..3] of byte;
  Pb2 = ^Tb2;
  Pb4 = ^Tb4;
var
  F: File;
  i,j,n: integer;
  s1,s2: smallint;
  c4: array[0..3] of AnsiChar;
  s: array of smallint;

procedure WriteInt(n: integer);
var
  i: integer;
  b4: Tb4;
  p4: Pb4;
begin
  p4 := addr(n);
  for i := 0 to 3 do
    b4[i] := p4[3-i];
  BlockWrite(F, b4, 4);
end;

procedure WriteSmallInt(n: SmallInt);
var
  b2: Tb2;
  p2: Pb2;
begin
  p2 := addr(n);
  b2[0] := p2[1];
  b2[1] := p2[0];
  BlockWrite(F, b2, 2);
end;

begin
  AssignFile(F, filename);
  ReWrite(F, 1);

// Header
  c4[0] := '.';
  c4[1] := 's';
  c4[2] := 'c';
  c4[3] := 'f';
  BlockWrite(F, c4, 4);

  WriteInt(Width);
  WriteInt(128);
  WriteInt(NoOfBases);

  WriteInt(LeftClip);
  WriteInt(RightClip);

  WriteInt(128 +Width*8);

  n := 0;
  BlockWrite(F, n, 4);

  WriteInt(128 +Width*8 +NoOfBases*12);

  c4[0] := '3';
  c4[1] := '.';
  c4[2] := '0';
  c4[3] := '0';
  BlockWrite(F, c4, 4);

  WriteInt(2);
  WriteInt(4);

  n := 0;
  BlockWrite(F, n, 4);
  BlockWrite(F, n, 4);

  n := 0;
  for i := 0 to 17 do
    BlockWrite(F, n, 4);

//  Trace Data
  SetLength(s, Width);
  for j := A to T do
  begin
    for i := 0 to Width-1 do
      s[i] := Trace[j][i];

    s1 := 0;
    for i := 0 to Width-1 do
    begin
      s2 := s[i];
      s[i] := s[i] -s1;
      s1 := s2;
    end;
    s1 := 0;
    for i := 0 to Width-1 do
    begin
      s2 := s[i];
      s[i] := s[i] -s1;
      s1 := s2;
    end;

    for i := 0 to Width-1 do
      WriteSmallInt(s[i]);
  end;

// Peak
  for i := 0 to NoOfBases-1 do
    WriteInt(Peak[i]);

// Accuracy
  n := 0;
  for i := 0 to NoOfBases-1 do
    BlockWrite(F, n, 4);

// Bases
  for i := 0 to NoOfBases-1 do
    BlockWrite(F, Base[i], 1);

//  reserved
  c4[0] := #0;
  c4[1] := #0;
  c4[2] := #0;
  for i := 0 to NoOfBases-1 do
    BlockWrite(F, c4, 3);

  CloseFile(F);

  FFileName := filename;
end;


////////////
// TUndoList
////////////


destructor TUndoList.destroy;
begin
  Clear;
  inherited;
end;

procedure TUndoList.Delete(Index: integer);
begin
  Items[Index].Free;
  inherited;
end;

procedure TUndoList.Clear;
var
  i: integer;
begin
  for i := 0 to Count-1 do
    Items[i].Free;
  inherited;
end;

function TUndoList.GetItems(Index:integer): TUndoItem;
begin
  result := inherited Items[Index];
end;

procedure TUndoList.SetItems(Index:integer; AItem: TUndoItem);
begin
  inherited Items[Index] := AItem;
end;

///////////////////
//  TTraceEdit;
///////////////////

constructor TTraceEdit.Create(AOwner: TComponent);
begin
  inherited;
  fCanvas    := TControlCanvas.Create;
  UndoList   := TUndoList.Create;

  FBorderStyle := bsSingle;
  FScrollBars := ssHorizontal;
  FMaskColor := clSilver;
  FHorzScale := 1.25;
  FVertScale := 1.0;
  FLineWidth := 1;
  FSelStart  := -1;
  FSelLength := 1;
  FCursor := -1;
  Canvas.Font.Assign(Font);
end;

destructor TTraceEdit.Destroy;
begin
  fCanvas.Free;
  UndoList.Free;

  if Data <> nil then
    Data.Free;
  inherited;
end;

procedure TTraceEdit.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd(Self);
  end;
end;

procedure TTraceEdit.Clear;
begin
  Data.Free;
  Data := nil;
end;

function TTraceEdit.GetEmpty: boolean;
begin
  result := Data = nil;
end;

function TTraceEdit.PosToSite(pos: integer):integer;
begin
  result := 0;
  while (result < Data.NoOfBases-1) do
  begin
    if trunc(Data.Peak[result]*HorzScale) >= pos then
      break;
    inc(result);
  end;
end;

function TTraceEdit.GetPitch: integer;
begin
  result := 0;
  if Data = nil then exit;
  result := ceil((Data.Peak[Data.RightClip-2]-Data.Peak[Data.LeftClip])/(Data.RightClip-Data.LeftClip-2)*HorzScale);
end;

procedure TTraceEdit.SetHorzScale(value: double);
begin
  if FHorzScale = value then exit;
  if (value < 0.000000000001) or (value > 100000000000) then exit;

  if FCursor > 0 then
    FCursor := trunc(FCursor/FHorzScale*value);

  FHorzScale := value;
  SetPoints;
  UpdateScrollRange;
  Invalidate;
end;

procedure TTraceEdit.SetVertScale(value: double);
begin
  if (value < 0.000000000001) or (value > 100000000000) then exit;

  FVertScale := value;

  if Data = nil then exit;
  if Data.Width = 0 then exit;

  SetPoints;
  Invalidate;
end;

procedure TTraceEdit.Resize;
begin
  inherited;

  SetPoints;
  Invalidate;
end;

procedure TTraceEdit.SetPoints;
var
  r,h,i: integer;
begin
  if Data = nil then exit;

  Canvas.Font.Assign(Font);

  h := trunc(2.5*abs(Font.Height));
  r := Data.MaxPeak div (ClientHeight -h) +1;

  for i := 0 to Data.Width-1 do
  begin
    Points[A][i].X := trunc(i*HorzScale);
    Points[T][i].X := trunc(i*HorzScale);
    Points[C][i].X := trunc(i*HorzScale);
    Points[G][i].X := trunc(i*HorzScale);
    if trunc(Data.Trace[A][i]*VertScale/r) >= (ClientHeight -h) then
      Points[A][i].Y := h+1
    else
      Points[A][i].Y := ClientHeight-1-(trunc(Data.Trace[A][i]*VertScale/r));
    if trunc(Data.Trace[T][i]*VertScale/r) >= (ClientHeight -h) then
      Points[T][i].Y := h+1
    else
      Points[T][i].Y := ClientHeight-1-(trunc(Data.Trace[T][i]*VertScale/r));
    if trunc(Data.Trace[C][i]*VertScale/r) >= (ClientHeight -h) then
      Points[C][i].Y := h+1
    else
      Points[C][i].Y := ClientHeight-1-(trunc(Data.Trace[C][i]*VertScale/r));
    if trunc(Data.Trace[G][i]*VertScale/r) >= (ClientHeight -h) then
      Points[G][i].Y := h+1
    else
      Points[G][i].Y := ClientHeight-1-(trunc(Data.Trace[G][i]*VertScale/r));
  end;
end;

procedure TTraceEdit.SetData(newdata: TTraceData);
begin
  FData := newdata;
  FSelStart := -1;
  FSelLength := 1;
  FCursor := -1;
  Setlength(Points[A], Data.Width);
  Setlength(Points[T], Data.Width);
  Setlength(Points[C], Data.Width);
  Setlength(Points[G], Data.Width);
  Setlength(PP, Data.Width);

  SetPoints;
  Invalidate;

  FModified := false;
end;

procedure TTraceEdit.DrawTraceData(ACanvas: TCanvas; x0, x1, x, y: integer);
var
  i : integer;
  aRect: TRect;
begin
  if Data = nil then exit;
  if (x0 >= Points[A][Data.Width-1].X) then exit;
  if (x1 < 0) then exit;
  aRect := Rect(x0, x1, x, y);
  //ACanvas.Brush.Color := clWhite;
  //ACanvas.FillRect(aRect);
  ACanvas.Font.Assign(Font);
  if (Data.LeftClip > 0) and (x0 < Points[A][Data.Peak[Data.LeftClip]].X) then
  begin
    ACanvas.Brush.Color := MaskColor;
    aRect.Left   := 0;
    aRect.Right  := (Points[A][Data.Peak[Data.LeftClip]].X +Points[A][Data.Peak[Data.LeftClip-1]].X) div 2;
    aRect.Top    := 0;
    aRect.Bottom := ClientHeight;
    if aRect.Left < x0 then aRect.Left := x0;
    if aRect.Right >= x1 then aRect.Right := x1-1;
    aRect.Left   := aRect.Left +x;
    aRect.Right  := aRect.Right +x;
    aRect.Top    := aRect.Top +y;
    aRect.Bottom := aRect.Bottom +y;
    ACanvas.FillRect(aRect);
  end;
  if (Data.RightClip <= Data.NoOfBases) then
    if (x1 >= (Points[A][Data.Peak[Data.RightClip-2]].X+Points[A][Data.Peak[Data.RightClip-1]].X) div 2) then
    begin
      ACanvas.Brush.Color := MaskColor;
      aRect.Left   := (Points[A][Data.Peak[Data.RightClip-2]].X +Points[A][Data.Peak[Data.RightClip-1]].X) div 2;
      aRect.Right  := Width;
      aRect.Top    := 0;
      aRect.Bottom := ClientHeight;
      if aRect.Left < x0 then aRect.Left := x0;
      if aRect.Right >= x1 then aRect.Right := x1-1;
      aRect.Left   := aRect.Left +x;
      aRect.Right  := aRect.Right +x;
      aRect.Top    := aRect.Top +y;
      aRect.Bottom := aRect.Bottom +y;
      ACanvas.FillRect(aRect);
    end;

  ACanvas.Pen.Width := LineWidth;
  ACanvas.Pen.Color := clGreen;
  i := 1;
  while (Points[A][i].X < x0) and (i < Data.Width) do
    inc(i);
  while (i < Data.Width) do
  begin
    if (Points[A][i].X > x1) then break;
    ACanvas.MoveTo(x-x0+Points[A][i-1].X, y+Points[A][i-1].Y);
    ACanvas.LineTo(x-x0+Points[A][i].X, y+Points[A][i].Y);
    inc(i);
  end;

  ACanvas.Pen.Color := clBlue;
  i := 1;
  while (Points[C][i].X < x0) and (i < Data.Width) do
    inc(i);
  while (i < Data.Width) do
  begin
    if (Points[C][i].X > x1) then break;
    ACanvas.MoveTo(x-x0+Points[C][i-1].X, y+Points[C][i-1].Y);
    ACanvas.LineTo(x-x0+Points[C][i].X, y+Points[C][i].Y);
    inc(i);
  end;

  ACanvas.Pen.Color := clBlack;
  i := 1;
  while (Points[G][i].X < x0) and (i < Data.Width) do
    inc(i);
  while (i < Data.Width) do
  begin
    if (Points[G][i].X > x1) then break;
    ACanvas.MoveTo(x-x0+Points[G][i-1].X, y+Points[G][i-1].Y);
    ACanvas.LineTo(x-x0+Points[G][i].X, y+Points[G][i].Y);
    inc(i);
  end;

  ACanvas.Pen.Color := clRed;
  i := 1;
  while (Points[T][i].X < x0) and (i < Data.Width) do
    inc(i);
  while (i < Data.Width) do
  begin
    if (Points[T][i].X > x1) then break;
    ACanvas.MoveTo(x-x0+Points[T][i-1].X, y+Points[T][i-1].Y);
    ACanvas.LineTo(x-x0+Points[T][i].X, y+Points[T][i].Y);
    inc(i);
  end;

  ACanvas.Brush.Style := bsSolid;
  i := 0;
  while (i < Data.NoOfBases) and (Points[A][Data.Peak[i]].X < x0) do
    inc(i);
  while (i < Data.NoOfBases) do
  begin
    if (Points[A][Data.Peak[i]].X > x1) then break;
    if (i < Data.LeftClip) or (i >= Data.RightClip-1) then
      ACanvas.Brush.Color := MaskColor
    else
      ACanvas.Brush.Color := Color;
    case upcase(Data.Base[i]) of
      'A': ACanvas.Font.Color := clGreen;
      'C': ACanvas.Font.Color := clBlue;
      'G': ACanvas.Font.Color := clBlack;
      'T', 
	  'U': ACanvas.Font.Color := clRed;
    else
      ACanvas.Font.Color := clMaroon;
    end;
    ACanvas.TextOut(x-x0+Points[A][Data.Peak[i]].X-ACanvas.TextWidth(Data.Base[i]) div 2, y+Abs(ACanvas.Font.Height), Data.Base[i]);

    if ((i -Data.LeftClip) mod 10 = 9) and (i < Data.RightClip-1) then
    begin
      if i < Data.LeftClip then
        ACanvas.Brush.Color := MaskColor
      else
        ACanvas.Brush.Color := Color;
      ACanvas.Font.Color := clBlack;
      ACanvas.TextOut(x-x0+Points[A][Data.Peak[i]].X-ACanvas.TextWidth(IntToStr(i-Data.LeftClip+1)) div 2, y, IntToStr(i-Data.LeftClip+1));
    end;
    inc(i);
  end;

  ACanvas.Pen.Width := LineWidth;
  ACanvas.Pen.Color := clBlack;
  ACanvas.MoveTo(x, y+ClientHeight-1);
  ACanvas.LineTo(x+x1-x0, y+ClientHeight-1);
end;


procedure TTraceEdit.Paint;
var
  rect: TRect;
  i: integer;
begin
  if (Data = nil) or (Data.NoOfBases = 0) then
  begin
    inherited;
    exit;
  end;

  if abs(Font.Height) <> abs(Canvas.Font.Height) then
    SetPoints;
  for i := 0 to Data.Width-1 do
    PP[i].X := Points[A][i].X -ScrollPos.X;

  Canvas.Font.Assign(Font);
  if Data.LeftClip > 0 then
  begin
    Canvas.Brush.Color := MaskColor;
    rect.Left   := -ScrollPos.X;
    rect.Right  := (PP[Data.Peak[Data.LeftClip]].X +PP[Data.Peak[Data.LeftClip-1]].X) div 2;
    rect.Top    := 0;
    rect.Bottom := ClientHeight;
    Canvas.FillRect(rect);
  end;
  if Data.RightClip <= Data.NoOfBases then
  begin
    Canvas.Brush.Color := MaskColor;
    rect.Left   := (PP[Data.Peak[Data.RightClip-2]].X +PP[Data.Peak[Data.RightClip-1]].X) div 2;
    rect.Right  := PP[Data.Width-1].X;
    rect.Top    := 0;
    rect.Bottom := ClientHeight;
    Canvas.FillRect(rect);
  end;

  Canvas.Pen.Width := LineWidth;
  for i := 0 to Data.Width-1 do
    PP[i].Y := Points[A][i].Y;
  Canvas.Pen.Color := clGreen;
  Canvas.Polyline(PP);
  for i := 0 to Data.Width-1 do
    PP[i].Y := Points[C][i].Y;
  Canvas.Pen.Color := clBlue;
  Canvas.Polyline(PP);
  for i := 0 to Data.Width-1 do
    PP[i].Y := Points[G][i].Y;
  Canvas.Pen.Color := clBlack;
  Canvas.Polyline(PP);
  for i := 0 to Data.Width-1 do
    PP[i].Y := Points[T][i].Y;
  Canvas.Pen.Color := clRed;
  Canvas.Polyline(PP);

  Canvas.Brush.Style := bsSolid;
  for i := 0 to Data.NoOfBases-1 do
  begin
    if (i < Data.LeftClip) or (i >= Data.RightClip-1) then
      Canvas.Brush.Color := MaskColor
    else
      Canvas.Brush.Color := Color;
    case upcase(Data.Base[i]) of
      'A': Canvas.Font.Color := clGreen;
      'C': Canvas.Font.Color := clBlue;
      'G': Canvas.Font.Color := clBlack;
      'T', 
	  'U': Canvas.Font.Color := clRed;
    else   Canvas.Font.Color := clMaroon;
    end;
    Canvas.TextOut(PP[Data.Peak[i]].X-Canvas.TextWidth(Data.Base[i]) div 2, Abs(Canvas.Font.Height), Data.Base[i]);
    if ((i -Data.LeftClip) mod 10 = 9) and (i < Data.RightClip-1) then
    begin
      Canvas.Font.Color := clBlack;
      Canvas.TextOut(PP[Data.Peak[i]].X-Canvas.TextWidth(IntToStr(i-Data.LeftClip+1)) div 2, 0, IntToStr(i-Data.LeftClip+1));
    end;
  end;

  Canvas.Pen.Width := LineWidth;
  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo(0, ClientHeight-1);
  Canvas.LineTo(Width-1, ClientHeight-1);

  if SelStart >= 0 then
    for i := 0 to SelLength-1 do
    begin
      if SelStart+i >= Data.NoOfBases then break;
      Canvas.Font.Assign(Font);
      Canvas.Brush.Color := clNavy;
      Canvas.Font.Color := clWhite;
      Canvas.TextOut(PP[Data.Peak[SelStart+i]].X-Canvas.TextWidth(Data.Base[SelStart+i]) div 2, Abs(Canvas.Font.Height), Data.Base[SelStart+i]);
    end
  else if Cursor >= 0  then
  begin
    Canvas.Pen.Mode := pmXor;
    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(Cursor-ScrollPos.X, Abs(Canvas.Font.Height));
    Canvas.LineTo(Cursor-ScrollPos.X, Abs(Canvas.Font.Height)*2);
    Canvas.Pen.Mode := pmCopy;
  end;
  if (MousePos.Y >= abs(Canvas.Font.Height)) and (MousePos.Y < abs(Canvas.Font.Height)*2) then
  begin
    Canvas.Pen.Mode := pmXor;
    Canvas.Pen.Color := clGray;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(MousePos.X-ScrollPos.X, Abs(Canvas.Font.Height)*5 div 2);
    Canvas.LineTo(MousePos.X-ScrollPos.X, ClientHeight-1);
    Canvas.Pen.Mode := pmCopy;
  end;

  inherited;
end;

procedure TTraceEdit.DrawVertLine(x,y: integer);
begin
  if (MousePos.Y >= abs(Canvas.Font.Height)) and (MousePos.Y < abs(Canvas.Font.Height)*2) then
  begin
    Canvas.Pen.Mode := pmXor;
    Canvas.Pen.Color := clGray;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(MousePos.X-ScrollPos.X, Abs(Canvas.Font.Height)*5 div 2);
    Canvas.LineTo(MousePos.X-ScrollPos.X, ClientHeight-1);
    Canvas.Pen.Mode := pmCopy;
    Invalidate;
  end;
  if (Y >= abs(Canvas.Font.Height)) and (Y < abs(Canvas.Font.Height)*2) then
  begin
    Canvas.Pen.Mode := pmXor;
    Canvas.Pen.Color := clGray;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(X-ScrollPos.X, Abs(Canvas.Font.Height)*5 div 2);
    Canvas.LineTo(X-ScrollPos.X, ClientHeight-1);
    Canvas.Pen.Mode := pmCopy;
    Invalidate;
  end;
end;

procedure TTraceEdit.SetSelStart(value: integer);
var
  i: integer;
begin
  if value = FSelStart then exit;
  if Data = nil then exit;
  if value >= Data.NoOfBases then exit;

  Canvas.Font.Assign(Font);
  if SelStart >= 0 then
    for i := 0 to SelLength-1 do
    begin
      if ((SelStart+i) < Data.LeftClip) or ((SelStart+i) >= Data.RightClip-1) then
        Canvas.Brush.Color := MaskColor
      else
        Canvas.Brush.Color := Color;
      case upcase(Data.Base[SelStart+i]) of
        'A': Canvas.Font.Color := clGreen;
        'C': Canvas.Font.Color := clBlue;
        'G': Canvas.Font.Color := clBlack;
        'T', 
		'U': Canvas.Font.Color := clRed;
      else
        Canvas.Font.Color := clMaroon;
      end;
      Canvas.TextOut(Points[A][Data.Peak[SelStart+i]].X-ScrollPos.X-Canvas.TextWidth(Data.Base[SelStart+i]) div 2, Abs(Canvas.Font.Height), Data.Base[SelStart+i]);
    end;
  if value >= 0 then
    for i := 0 to SelLength-1 do
    begin
      if value+i >= Data.NoOfBases then break;
      Canvas.Brush.Color := clNavy;
      Canvas.Font.Color := clWhite;
      Canvas.TextOut(Points[A][Data.Peak[value+i]].X-ScrollPos.X-Canvas.TextWidth(Data.Base[value+i]) div 2, Abs(Canvas.Font.Height), Data.Base[value+i]);
    end;
  if value <> SelStart then
  begin
    FSelStart := value;
    if @FOnMoved <> nil then
      OnMoved(Self);
  end;
end;

procedure TTraceEdit.SetSelLength(value: integer);
var
  i,n1,n2: integer;
begin
  if value = FSelLength then exit;
  if value < 1 then exit;
  if value > Data.NoOfBases then exit;
  if Data = nil then exit;

  if SelStart >= 0 then
  begin
    if value > SelLength then
    begin
      n1 := SelLength;
      n2 := value-1;
    end
    else
    begin
      n1 := value;
      n2 := SelLength-1;
    end;
    for i := n1 to n2 do
    begin
      if SelStart+i >= Data.NoOfBases then break;
      if value > SelLength then
      begin
        Canvas.Brush.Color := clNavy;
        Canvas.Font.Color := clWhite;
      end
      else
      begin
        if ((SelStart+i) < Data.LeftClip) or ((SelStart+i) >= Data.RightClip-1) then
          Canvas.Brush.Color := MaskColor
        else
          Canvas.Brush.Color := Color;
        case upcase(Data.Base[SelStart+i]) of
          'A': Canvas.Font.Color := clGreen;
          'C': Canvas.Font.Color := clBlue;
          'G': Canvas.Font.Color := clBlack;
          'T', 
		  'U': Canvas.Font.Color := clRed;
        else
          Canvas.Font.Color := clMaroon;
        end;
      end;
      Canvas.TextOut(Points[A][Data.Peak[SelStart+i]].X-ScrollPos.X-Canvas.TextWidth(Data.Base[SelStart+i]) div 2, Abs(Canvas.Font.Height), Data.Base[SelStart+i]);
    end;
  end;

  FSelLength := value;
  if (SelStart >= 0) and (@FOnMoved <> nil) then
    OnMoved(Self);
end;

procedure TTraceEdit.SetCursor(value: integer);
begin
  if Cursor >= 0 then
  begin
    Canvas.Pen.Mode := pmXor;
    Canvas.Pen.Color := Color;
    Canvas.MoveTo(Cursor-ScrollPos.X, Abs(Canvas.Font.Height));
    Canvas.LineTo(Cursor-ScrollPos.X, Abs(Canvas.Font.Height)*2);
    Canvas.Pen.Mode := pmCopy;
  end;
  if value >= 0 then
  begin
    Canvas.Pen.Mode := pmXor;
    Canvas.Pen.Color := Color;
    Canvas.MoveTo(value-ScrollPos.X, Abs(Canvas.Font.Height));
    Canvas.LineTo(value-ScrollPos.X, Abs(Canvas.Font.Height)*2);
    Canvas.Pen.Mode := pmCopy;
  end;
  if value <> Cursor then
  begin
    FCursor := value;
    if @FOnMoved <> nil then
      OnMoved(Self);
  end;
end;

procedure TTraceEdit.DeleteBaseCustom(index: integer);
var
  i,d: integer;
begin
  d := Canvas.TextWidth(Data.Base[index]) div 2;
  if (index < Data.LeftClip) or (index >= Data.RightClip-1) then
  begin
    Canvas.Brush.Color := MaskColor;
    Canvas.Font.Color := MaskColor;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font.Color := Color;
  end;
  Canvas.TextOut(Points[A][Data.Peak[index]].X-ScrollPos.X-d, Abs(Canvas.Font.Height), Data.Base[index]);

  Canvas.Brush.Color := Color;
  Canvas.Font.Color := Color;
  for i := index to Data.RightClip-2 do
  begin
    if i < Data.LeftClip then continue;
    if (i -Data.LeftClip) mod 10 = 9 then
      Canvas.TextOut(Points[A][Data.Peak[i]].X-ScrollPos.X-(Canvas.TextWidth(IntToStr(i-Data.LeftClip+1)) div 2), 0, IntToStr(i-Data.LeftClip+1));
  end;

  if SelStart+SelLength >= Data.NoOfBases then
    if SelStart = Data.NoOfBases-1 then
    begin
      FSelStart := SelStart -1;
      FSelLength := 1;
    end
    else if SelLength > 1 then
      FSelLength := SelLength -1
    else
      FSelStart := SelStart -1;

  for i := index to Data.NoOfBases-2 do
  begin
    Data.Base[i] := Data.Base[i+1];
    Data.Peak[i] := Data.Peak[i+1];
    Data.Score[i] := Data.Score[i+1];
    Data.Prob[A][i] := Data.Prob[A][i+1];
    Data.Prob[C][i] := Data.Prob[C][i+1];
    Data.Prob[G][i] := Data.Prob[G][i+1];
    Data.Prob[T][i] := Data.Prob[T][i+1];
  end;

  dec(Data.FNoOfBases);
  SetLength(Data.Peak, Data.NoOfBases);
  SetLength(Data.Base, Data.NoOfBases);
  SetLength(Data.Score, Data.NoOfBases);
  SetLength(Data.Prob[A], Data.NoOfBases);
  SetLength(Data.Prob[C], Data.NoOfBases);
  SetLength(Data.Prob[G], Data.NoOfBases);
  SetLength(Data.Prob[T], Data.NoOfBases);

  if Data.LeftClip  > index then Dec(Data.LeftClip);
  if Data.RightClip-1 > index then Dec(Data.RightClip);

  Canvas.Font.Color := clBlack;
  for i := index to Data.RightClip-2 do
  begin
    if i < Data.LeftClip then continue;
    if (i -Data.LeftClip) mod 10 = 9 then
      Canvas.TextOut(Points[A][Data.Peak[i]].X-ScrollPos.X-(Canvas.TextWidth(IntToStr(i-Data.LeftClip+1)) div 2), 0, IntToStr(i-Data.LeftClip+1));
  end;

end;

procedure TTraceEdit.DeleteBase;
var
  UndoItem: TUndoItem;
  i: integer;
  gonext: boolean;
begin
  if Data = nil then exit;
  if SelStart < 0 then exit;

  gonext := false;
  for i := SelLength-1 downto 0 do
  begin
    UndoItem := TUndoItem.Create;
    UndoItem.Action := eaDelete;
    UndoItem.Index  := SelStart+i;
    UndoItem.Base   := Data.Base[SelStart+i];
    UndoItem.Peak   := Data.Peak[SelStart+i];
    UndoItem.Score  := Data.Score[SelStart+i];
    UndoItem.Prob[0]:= Data.Prob[0][SelStart+i];
    UndoItem.Prob[1]:= Data.Prob[1][SelStart+i];
    UndoItem.Prob[2]:= Data.Prob[2][SelStart+i];
    UndoItem.Prob[3]:= Data.Prob[3][SelStart+i];
    UndoItem.LMask  := Data.LeftClip;
    UndoItem.RMask  := Data.RightClip;
    UndoItem.goNext := gonext;
    UndoList.Add(UndoItem);
    gonext := true;

    DeleteBaseCustom(SelStart+i);
  end;

  FSelLength := 1;
  FSelStart := -1;

  if @FOnChange <> nil then
    OnChange(Self);

  FModified := true;
end;

procedure TTraceEdit.ChangeBaseCustom(base: AnsiChar);
var
  d: integer;
begin
  d := Canvas.TextWidth(Data.Base[SelStart]) div 2;
  if (SelStart < Data.LeftClip) or (SelStart >= Data.RightClip-1) then
  begin
    Canvas.Brush.Color := MaskColor;
    Canvas.Font.Color := MaskColor;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font.Color := Color;
  end;
  Canvas.TextOut(Points[A][Data.Peak[SelStart]].X-ScrollPos.X-d, Abs(Canvas.Font.Height), Data.Base[SelStart]);

  Data.Base[SelStart] := base;
  d := Canvas.TextWidth(Data.Base[SelStart]) div 2;
  if SelStart >= 0 then
  begin
    Canvas.Brush.Color := clNavy;
    Canvas.Font.Color := clWhite;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    case upcase(base) of
      'A': Canvas.Font.Color := clGreen;
      'C': Canvas.Font.Color := clBlue;
      'G': Canvas.Font.Color := clBlack;
      'T', 
	  'U': Canvas.Font.Color := clRed;
    else
      Canvas.Font.Color := clMaroon;
    end;
  end;
  Canvas.TextOut(Points[A][Data.Peak[SelStart]].X-ScrollPos.X-d, Abs(Canvas.Font.Height), Data.Base[SelStart]);

end;

procedure TTraceEdit.ChangeBase(base: AnsiChar);
var
  UndoItem: TUndoItem;
begin
  if SelStart < 0 then exit;
  if base = Data.Base[SelStart] then exit;

  UndoItem := TUndoItem.Create;
  UndoItem.Action := eaChange;
  UndoItem.Index  := SelStart;
  UndoItem.Base   := Data.Base[SelStart];
  UndoItem.Peak   := Data.Peak[SelStart];
  UndoItem.Score  := Data.Score[SelStart];
  UndoItem.Prob[0]:= Data.Prob[0][SelStart];
  UndoItem.Prob[1]:= Data.Prob[1][SelStart];
  UndoItem.Prob[2]:= Data.Prob[2][SelStart];
  UndoItem.Prob[3]:= Data.Prob[3][SelStart];

  UndoList.Add(UndoItem);

  ChangeBaseCustom(base);

  if @FOnChange <> nil then
    OnChange(Self);

  FModified := true;
end;

procedure TTraceEdit.InsertBaseCustom(index: integer; base: AnsiChar);
var
  i: integer;
begin
  Canvas.Brush.Color := Color;
  Canvas.Font.Color := Color;
  for i := index to Data.RightClip-2 do
  begin
    if i < Data.LeftClip then continue;
    if (i -Data.LeftClip) mod 10 = 9 then
      Canvas.TextOut(Points[A][Data.Peak[i]].X-ScrollPos.X-(Canvas.TextWidth(IntToStr(i-Data.LeftClip+1)) div 2), 0, IntToStr(i-Data.LeftClip+1));
  end;

  inc(Data.FNoOfBases);
  SetLength(Data.Peak, Data.NoOfBases);
  SetLength(Data.Base, Data.NoOfBases);
  SetLength(Data.Score, Data.NoOfBases);
  SetLength(Data.Prob[A], Data.NoOfBases);
  SetLength(Data.Prob[C], Data.NoOfBases);
  SetLength(Data.Prob[G], Data.NoOfBases);
  SetLength(Data.Prob[T], Data.NoOfBases);

  for i := Data.NoOfBases-1 downto index+1 do
  begin
    Data.Base[i] := Data.Base[i-1];
    Data.Peak[i] := Data.Peak[i-1];
    Data.Score[i] := Data.Score[i-1];
    Data.Prob[A][i] := Data.Prob[A][i-1];
    Data.Prob[C][i] := Data.Prob[C][i-1];
    Data.Prob[G][i] := Data.Prob[G][i-1];
    Data.Prob[T][i] := Data.Prob[T][i-1];
  end;

  if Data.LeftClip  > index then Inc(Data.LeftClip);
  if Data.RightClip > index then Inc(Data.RightClip);

  Data.Base[index] := base;
  Data.Peak[index] := Round(Cursor/HorzScale);
//  Data.SetProbAt(index);
//  Data.SetScoreAt(index);

  Canvas.Brush.Color := clNavy;
  Canvas.Font.Color := clWhite;
  Canvas.TextOut(Points[A][Data.Peak[index]].X-ScrollPos.X-(Canvas.TextWidth(Data.Base[index]) div 2), Abs(Canvas.Font.Height), Data.Base[index]);

  Canvas.Brush.Color := Color;
  Canvas.Font.Color := clBlack;
  for i := index to Data.RightClip-2 do
  begin
    if i < Data.LeftClip then continue;
    if (i -Data.LeftClip) mod 10 = 9 then
      Canvas.TextOut(Points[A][Data.Peak[i]].X-ScrollPos.X-(Canvas.TextWidth(IntToStr(i-Data.LeftClip+1)) div 2), 0, IntToStr(i-Data.LeftClip+1));
  end;

  Cursor := -1;
  SelStart := index;
end;

procedure TTraceEdit.InsertBase(base: AnsiChar);
var
  UndoItem: TUndoItem;
  i,n: integer;
begin
  if Data = nil then exit;
  if SelStart >= 0 then exit;
  if Cursor < 0 then exit;

  n := -1;
  for i := 0 to Data.NoOfBases-1 do
    if trunc(Data.Peak[i]*HorzScale) > Cursor then
    begin
      n := i;
      break;
    end;
  if n = -1 then n := Data.NoOfBases;

  UndoItem := TUndoItem.Create;
  UndoItem.Action := eaInsert;
  UndoItem.Index  := n;
  UndoItem.Base   := base;
  UndoItem.Peak   := Cursor;
  UndoItem.Score  := 0;
  UndoItem.Prob[0]:= 0;
  UndoItem.Prob[1]:= 0;
  UndoItem.Prob[2]:= 0;
  UndoItem.Prob[3]:= 0;
  UndoItem.LMask  := Data.LeftClip;
  UndoItem.RMask  := Data.RightClip;
  UndoList.Add(UndoItem);

  InsertBaseCustom(n, base);

  if @FOnChange <> nil then
    OnChange(Self);

  FModified := true;
end;

procedure TTraceEdit.ReverseComplementCustom;
begin
  Data.ReverseComplement;

  FCursor := -1;
  if SelStart > -1 then
    SelStart := Data.NoOfBases -SelStart -SelLength;

  SetPoints;
  Invalidate;
end;

procedure TTraceEdit.ReverseComplement;
var
  UndoItem: TUndoItem;
begin
  if Data = nil then exit;

  UndoItem := TUndoItem.Create;
  UndoItem.Action := eaReverse;
  UndoItem.LMask  := Data.LeftClip;
  UndoItem.RMask  := Data.RightClip;

  UndoList.Add(UndoItem);

  ReverseComplementCustom;

  FModified := true;
end;

procedure TTraceEdit.MaskLeftCustom(index: integer);
begin
  if index = Data.LeftClip then exit;
  Data.LeftClip := index;
  Invalidate;
end;

procedure TTraceEdit.MaskRightCustom(index: integer);
begin
  if index = Data.RightClip then exit;
  Data.RightClip := index;
  Invalidate;
end;

procedure TTraceEdit.MaskLeft;
var
  UndoItem: TUndoItem;
  index: integer;
begin
  if SelStart = -1 then exit;
  if (SelStart+1) = Data.LeftClip then
  begin
    ClearLeftMask;
    exit;
  end
  else if (SelStart+1) >= Data.RightClip then
    index := Data.RightClip-1
  else
    index := SelStart+1;
  if index = Data.LeftClip then exit;

  UndoItem := TUndoItem.Create;
  UndoItem.Action := eaMaskLeft;
  UndoItem.Index  := SelStart;
  UndoItem.LMask  := Data.LeftClip;
  UndoItem.RMask  := Data.RightClip;
  UndoList.Add(UndoItem);

  MaskLeftCustom(index);
end;

procedure TTraceEdit.MaskRight;
var
  UndoItem: TUndoItem;
  index: integer;
begin
  if SelStart = -1 then exit;
  if (SelStart+1) = Data.RightClip then
  begin
    ClearRightMask;
    exit;
  end
  else if (SelStart+1) <= Data.LeftClip then
    index := Data.LeftClip+1
  else
    index := SelStart+1;
  if index = Data.RightClip then exit;

  UndoItem := TUndoItem.Create;
  UndoItem.Action := eaMaskRight;
  UndoItem.Index  := SelStart;
  UndoItem.LMask  := Data.LeftClip;
  UndoItem.RMask  := Data.RightClip;
  UndoList.Add(UndoItem);

  MaskRightCustom(index);
end;

procedure TTraceEdit.ClearLeftMask;
var
  UndoItem: TUndoItem;
begin
  if (Data.LeftClip = 0) then exit;

  UndoItem := TUndoItem.Create;
  UndoItem.Action := eaClearMask;
  UndoItem.Index  := SelStart;
  UndoItem.LMask  := Data.LeftClip;
  UndoItem.RMask  := Data.RightClip;

  UndoList.Add(UndoItem);

  MaskLeftCustom(0);
  Invalidate;
end;

procedure TTraceEdit.ClearRightMask;
var
  UndoItem: TUndoItem;
begin
  if (Data.RightClip = Data.NoOfBases+1) then exit;

  UndoItem := TUndoItem.Create;
  UndoItem.Action := eaClearMask;
  UndoItem.Index  := SelStart;
  UndoItem.LMask  := Data.LeftClip;
  UndoItem.RMask  := Data.RightClip;

  UndoList.Add(UndoItem);

  MaskRightCustom(Data.NoOfBases+1);
  Invalidate;
end;

procedure TTraceEdit.ClearMask;
var
  UndoItem: TUndoItem;
begin
  if (Data.LeftClip = 0) and (Data.RightClip = Data.NoOfBases+1) then exit;

  UndoItem := TUndoItem.Create;
  UndoItem.Action := eaClearMask;
  UndoItem.Index  := SelStart;
  UndoItem.LMask  := Data.LeftClip;
  UndoItem.RMask  := Data.RightClip;

  UndoList.Add(UndoItem);

  MaskLeftCustom(0);
  MaskRightCustom(Data.NoOfBases+1);
  Invalidate;
end;

procedure TTraceEdit.Undo;
var
  UndoItem: TUndoItem;
  gonext: boolean;
  sl: integer;
begin
  if UndoList.Count = 0 then exit;
  sl := 0;
  repeat
    UndoItem := UndoList[UndoList.Count-1];
    gonext := UndoItem.goNext;
    case UndoItem.Action of
      eaDelete:
        begin
          SelStart := -1;
          Cursor    := UndoItem.Peak;
          InsertBaseCustom(UndoItem.Index, UndoItem.Base);
          inc(sl);
          if not gonext then
          begin
            MaskLeftCustom(UndoItem.LMask);
            MaskRightCustom(UndoItem.RMask);
            SelStart := SelStart -sl +1;
            SelLength := sl;
          end;
        end;
      eaChange:
        begin
          SelStart := UndoItem.Index;
          ChangeBaseCustom(UndoItem.Base);
        end;
      eaInsert:
        begin
          SelStart := UndoItem.Index;
          DeleteBaseCustom(SelStart);
          Cursor    := UndoItem.Peak;
        end;
      eaReverse:
        ReverseComplementCustom;
      eaMaskLeft:
        MaskLeftCustom(UndoItem.LMask);
      eaMaskRight:
        MaskRightCustom(UndoItem.RMask);
      eaClearMask:
        begin
          MaskLeftCustom(UndoItem.LMask);
          MaskRightCustom(UndoItem.RMask);
        end;
    end;
    UndoList.Delete(UndoList.Count-1);
  until (not gonext) or (UndoList.Count = 0);
  Invalidate;
  if UndoList.Count = 0 then
    FModified := false;
end;

function TTraceEdit.Find(target: AnsiString): boolean;
var
  i,j: integer;
begin
  result := false;
  if target = '' then exit;
  if Data = nil then exit;
  if Data.NoOfBases < length(target) then exit;

  for i := 0 to Data.NoOfBases-length(target) do
  begin
    for j := 1 to length(target) do
    begin
      if upcase(Data.Base[i+j-1]) <> upcase(target[j]) then
        break;
      if j = length(target) then
      begin
        SelLength := 1;
        SelStart := i;
        SelLength := j;
        Cursor := -1;
        result := true;
        break;
      end;
    end;
    if result then break;
  end;
end;

function TTraceEdit.FindNext(target: AnsiString): boolean;
var
  i,j: integer;
begin
  result := false;
  if target = '' then exit;
  if Data = nil then exit;
  if SelStart = -1 then
  begin
    result := Find(target);
    exit;
  end;
  if (Data.NoOfBases-SelStart) < length(target) then exit;

  for i := SelStart+1 to Data.NoOfBases-length(target) do
  begin
    for j := 1 to length(target) do
    begin
      if upcase(Data.Base[i+j-1]) <> upcase(target[j]) then
        break;
      if j = length(target) then
      begin
        SelLength := 1;
        SelStart := i;
        SelLength := j;
        Cursor := -1;
        result := true;
        break;
      end;
    end;
    if result then break;
  end;
end;

function TTraceEdit.FindPrev(target: AnsiString): boolean;
var
  i,j: integer;
begin
  result := false;
  if target = '' then exit;
  if Data = nil then exit;
  if SelStart <= length(target) then exit;

  for i := SelStart-1 downto 0 do
  begin
    for j := 1 to length(target) do
    begin
      if upcase(Data.Base[i+j-1]) <> upcase(target[j]) then
        break;
      if j = length(target) then
      begin
        SelLength := 1;
        SelStart := i;
        SelLength := j;
        Cursor := -1;
        result := true;
        break;
      end;
    end;
    if result then break;
  end;
end;

procedure TTraceEdit.CopySequenceToClipboard;
var
  buffer: PChar;
  i,n: integer;
begin
  if Data = nil then exit;
  GetMem(buffer, StringElementSize(Data.Name)*(Length(Data.UnmaskedBases)+Length(Data.Name)+4));
  buffer[0] := '>';
  for i := 1 to Length(Data.Name) do
    buffer[i] := Char(Data.Name[i]);
  n := Length(Data.Name)+1;
  buffer[n]   := #13;
  buffer[n+1] := #10;
  for i := 1 to Length(Data.UnmaskedBases) do
    buffer[i+n+1] := Char(Data.UnmaskedBases[i]);
  buffer[Length(Data.UnmaskedBases)+n+2] := #0;
  ClipBoard.SetTextBuf(buffer);
end;

procedure TTraceEdit.CopyBasesToClipboard;
var
  buffer: PChar;
  i: integer;
begin
  if Data = nil then exit;
  GetMem(buffer, StringElementSize(Data.UnmaskedBases)*(Length(Data.UnmaskedBases)+1));
  for i := 1 to Length(Data.UnmaskedBases) do
    buffer[i-1] := Char(Data.UnmaskedBases[i]);
  buffer[Length(Data.UnmaskedBases)] := #0;
  ClipBoard.SetTextBuf(buffer);
end;

procedure TTraceEdit.CopySelectionToClipboard;
var
  buffer: PChar;
  i: integer;
begin
  if Data = nil then exit;
  if (SelStart < 0) or (SelLength <= 1) then exit;

  GetMem(buffer, SizeOf(Char)*(SelLength+1));   // assuming selection element length is going to stay UTF-16 (2 bytes)
  for i := 0 to SelLength-1 do
    buffer[i] := Char(Data.Base[SelStart+i]);
  buffer[SelLength] := #0;
  ClipBoard.SetTextBuf(buffer);
end;

function TTraceEdit.LoadFromFile(filename: String): boolean;
var
  tmpdata: TTraceData;
begin
  tmpdata := TTraceData.Create;
  result := tmpdata.LoadFromFile(filename);
  if result then
    SetData(tmpdata)
  else
    tmpdata.Free;
end;

procedure TTraceEdit.SaveToFile(filename: String);
begin
  if Data = nil then exit;
  Data.SaveToFile(filename);
  Data.Name := ExtractFileName(filename);
  if ExtractFileExt(Data.Name) <> '' then
    Data.Name := Copy(Data.Name, 1, Length(Data.Name)-Length(ExtractFileExt(Data.Name)));
  FModified := false;
end;

procedure TTraceEdit.ExportToFASTAFile(filename: String);
var
  f: TextFile;
begin
  if Data = nil then exit;

  AssignFile(f, filename);
  ReWrite(f);

  write(f,'>');
  writeln(f, Data.Name);
  writeln(f, Data.UnmaskedBases);

  CloseFile(f);

  Data.Name := ExtractFileName(filename);
  if ExtractFileExt(Data.Name) <> '' then
    Data.Name := Copy(Data.Name, 1, Length(Data.Name)-Length(ExtractFileExt(Data.Name)));
end;

procedure TTraceEdit.DrawDragCursor;
var
  rect: TRect;
begin
  if DragOri.X < MousePos.X then
  begin
    rect.Left   := DragOri.X-ScrollPos.X;
    rect.Right  := MousePos.X-ScrollPos.X;
  end
  else
  begin
    rect.Left   := MousePos.X-ScrollPos.X;
    rect.Right  := DragOri.X-ScrollPos.X;
  end;
  if DragOri.Y < MousePos.Y then
  begin
    rect.Top    := DragOri.Y;
    rect.Bottom := MousePos.Y;
  end
  else
  begin
    rect.Top    := MousePos.Y;
    rect.Bottom := DragOri.Y;
  end;
  Canvas.DrawFocusRect(rect);
end;

procedure TTraceEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i,d,x1: integer;
begin
  if Data = nil then exit;
  x1 := X +ScrollPos.X;
  if (Y >= abs(Canvas.Font.Height)) and (Y < abs(Canvas.Font.Height)*2) then
  begin
    d := Canvas.TextWidth('G') div 2;
    if (SelStart >= 0) and (ssShift in Shift) and (Button = mbLeft) then
    begin
      for i := 0 to Data.NoOfBases-1 do
        if (x1 >= trunc(Data.Peak[i]*HorzScale)-d) and (x1 <= trunc(Data.Peak[i]*HorzScale)+d) then
        begin
          if i > SelStart then
            SelLength := i -SelStart +1
          else
          begin
            d := SelStart;
            SelStart  := i;
            SelLength := d -i +SelLength;
          end;
          break;
        end
    end
    else if Button = mbLeft then
    begin
      SelLength := 1;
      for i := 0 to Data.NoOfBases-1 do
        if (x1 >= trunc(Data.Peak[i]*HorzScale)-d) and (x1 <= trunc(Data.Peak[i]*HorzScale)+d) then
        begin
          SelStart := i;
          Cursor := -1;
          BaseSelecting := true;
          OriBasePos    := i;
          break;
        end
        else if x1 < trunc(Data.Peak[i]*HorzScale) then
        begin
          SelStart := -1;
          Cursor := x1;
          break;
        end;
    end;
  end
  else
  begin
    SelLength := 1;
    SelStart := -1;
    Cursor := -1;
    DragOri.X := x1;
    DragOri.Y := Y;
    DrawDragCursor;
    Draging := true;
  end;

  if not Focused then
    SetFocus;
  Invalidate;
  inherited;
end;

procedure TTraceEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Draging then
  begin
    DrawDragCursor;
    if ((DragOri.Y >= abs(Canvas.Font.Height)) and (Y < abs(Canvas.Font.Height)*2)) or
       ((Y >= abs(Canvas.Font.Height)) and (DragOri.Y < abs(Canvas.Font.Height)*2)) then
      if DragOri.X < MousePos.X then
      begin
        SelStart  := PosToSite(DragOri.X);
        SelLength := PosToSite(MousePos.X) -SelStart;
      end
      else
      begin
        SelStart  := PosToSite(MousePos.X);
        SelLength := PosToSite(DragOri.X) -SelStart;
      end;
  end;

  Draging := false;
  BaseSelecting := false;

  inherited;
end;

procedure TTraceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i,j,k,n1,n2: integer;
begin
  if Data = nil then exit;

  if (Y >= abs(Canvas.Font.Height)) and (Y < abs(Canvas.Font.Height)*2) then
    Screen.Cursor := crIBeam
  else
    Screen.Cursor := crArrow;

  DrawVertLine(x+ScrollPos.X, y);

  if BaseSelecting then
  begin
    i := OriBasePos;
    if X >= Data.Peak[OriBasePos]-ScrollPos.X then
    begin
      while (i <= Data.NoOfBases-1) and (Data.Peak[i] < X+ScrollPos.X) do
        inc(i);
      if i = OriBasePos then
        j := 1
      else
        j := i-OriBasePos;
      i := OriBasePos;
    end
    else
    begin
      while (i >= 0) and (Data.Peak[i] > X+ScrollPos.X) do
        dec(i);
      inc(i);
      j := OriBasePos -i +1;
    end;
    if (i <> SelStart) or (j <> SelLength) then
    begin
      if j < SelLength then
      begin
        if i > SelStart then
        begin
          n1 := SelStart;
          n2 := i-1;
        end
        else
        begin
          n1 := SelStart +j;
          n2 := SelStart +SelLength -1;
        end;
        for k := n1 to n2 do
        begin
          if (k < Data.LeftClip) or (k >= Data.RightClip-1) then
            Canvas.Brush.Color := MaskColor
          else
            Canvas.Brush.Color := Color;
          case upcase(Data.Base[k]) of
            'A': Canvas.Font.Color := clGreen;
            'C': Canvas.Font.Color := clBlue;
            'G': Canvas.Font.Color := clBlack;
            'T', 
			'U': Canvas.Font.Color := clRed;
          else
            Canvas.Font.Color := clMaroon;
          end;
          Canvas.TextOut(Points[A][Data.Peak[k]].X-ScrollPos.X-Canvas.TextWidth(Data.Base[k]) div 2, Abs(Canvas.Font.Height), Data.Base[k]);
        end;
      end
      else
      begin
        if i < SelStart then
        begin
          n1 := i;
          n2 := SelStart-1;
        end
        else
        begin
          n1 := SelStart+SelLength;
          n2 := SelStart+j-1;
        end;
        Canvas.Brush.Color := clNavy;
        Canvas.Font.Color := clWhite;
        for k := n1 to n2 do
          Canvas.TextOut(Points[A][Data.Peak[k]].X-ScrollPos.X-Canvas.TextWidth(Data.Base[k]) div 2, Abs(Canvas.Font.Height), Data.Base[k]);
      end;
      FSelStart  := i;
      FSelLength := j;
    end;
  end;

  if Draging then
    DrawDragCursor;
  if X < 0 then
    MousePos.X := ScrollPos.X
  else if X > ClientWidth then
    MousePos.X := ClientWidth+ScrollPos.X
  else
    MousePos.X := X+ScrollPos.X;
  if Y < 0 then
    MousePos.Y := 0
  else if Y > ClientHeight then
    MousePos.Y := ClientHeight
  else
    MousePos.Y := Y;
  if Draging then
    DrawDragCursor;

  inherited;
end;

procedure TTraceEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  ch: AnsiChar;
  s: integer;
begin
  inherited;
  if Data = nil then exit;
  if not EditEnabled then exit;
  case Key of
    VK_LEFT  : if SelStart > 0 then
                 if Shift = [ssShift] then
                   if SelLength = 1 then
                     SelStart := SelStart-1
                   else
                     SelLength := SelLength -1
                 else
                 begin
                   SelLength := 1;
                   SelStart := SelStart-1;
                 end;
    VK_RIGHT : if (SelStart >= 0) and (SelStart < Data.NoOfBases-1) then
                 if Shift = [ssShift] then
                   SelLength := SelLength+1
                 else
                 begin
                   s := SelLength;
                   SelLength := 1;
                   SelStart := SelStart+s;
                 end;
    VK_DELETE: begin
                 s := SelStart;
                 DeleteBase;
                 if s < Data.NoOfBases then
                   SelStart := s;
               end;
  end;
  if ssCtrl in Shift then
    Exit;
  if (Key = ORD('A')) or (Key = ORD('T')) or (Key = ORD('C')) or (Key = ORD('G')) or (Key = ORD('U'))
  or (Key = ORD('R')) or (Key = ORD('Y')) or (Key = ORD('M')) or (Key = ORD('K')) or (Key = ORD('S')) or (Key = ORD('W'))
  or (Key = ORD('B')) or (Key = ORD('V')) or (Key = ORD('D')) or (Key = ORD('H')) or (Key = ORD('N')) then
  begin
    if SelLength > 1 then
      SelLength := 1
    else
    begin
      if ssShift in Shift then
        ch := AnsiChar(Chr(Key))
      else
        ch := AnsiChar(Chr(Key+32));
      if SelStart >= 0 then
        ChangeBase(ch)
      else
        InsertBase(ch);
    end;
  end;
end;

procedure TTraceEdit.CMWantSpecialKey(var Msg: TLMessage);
begin
  inherited;
  if (Msg.WParam = VK_LEFT) or (Msg.WParam = VK_RIGHT) then Msg.Result := 1;
end;

procedure TTraceEdit.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,  WS_HSCROLL or WS_VSCROLL, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or ScrollBar[FScrollBars];
    if FBorderStyle = bsSingle then
      if NewStyleControls { and Ctl3D} then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
end;

{
procedure TTraceEdit.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
    SetPoints;
  end;
end;
}

procedure TTraceEdit.WMSize(var Msg: TWMSize);
begin
  inherited;
  UpdateScrollRange;
end;

procedure TTraceEdit.WMHScroll(var Msg: TWMHScroll);

  function GetTrackPos: integer;
  var
    si: TScrollInfo;
  begin
    si.cbSize := SizeOf(TScrollInfo);
    si.fMask  := SIF_TRACKPOS;
    GetScrollInfo(Handle, SB_HORZ, si);
    result := si.nTrackPos;
  end;

var
  x,dx: integer;
begin
  if Data = nil then exit;
  if Data.NoOfBases = 0 then exit;
  
  dx := Data.Width div Data.NoOfBases;

  case Msg.ScrollCode of
    SB_THUMBPOSITION: x := Msg.Pos;
    SB_THUMBTRACK   : x := GetTrackPos;
    SB_PAGERIGHT    : x := (ScrollPos.X +ClientWidth) div dx;
    SB_PAGELEFT     : x := (ScrollPos.X -ClientWidth) div dx;
    SB_LINERIGHT    : x := (ScrollPos.X div dx) +1;
    SB_LINELEFT     : x := (ScrollPos.X div dx) -1;
  else
    x := ScrollPos.X div dx;
  end;

  if x < 0 then
    x := 0
  else if x*dx > ScrollLimit then
    x := ScrollLimit div dx;

  SetScrollPos(Handle, SB_HORZ, x, ScrollPos.X <> x*dx);
  ScrollWindowEx(Handle, ScrollPos.X-x*dx, 0, nil, nil, 0, nil, SW_INVALIDATE or SW_ERASE);
  ScrollPos.X := x*dx;
end;

procedure TTraceEdit.UpdateScrollRange;
var
  dx: integer;
begin
  if (Data = nil) or (Data.NoOfBases <= 0) then
    exit;
  dx := Data.Width div Data.NoOfBases;

  ScrollLimit := ceil(Data.Width*HorzScale-ClientWidth);
  if ScrollLimit < 0 then
    ScrollLimit := 0
  else if ScrollLimit mod dx <> 0 then
    ScrollLimit := ScrollLimit +dx -(ScrollLimit mod dx);

  if ScrollLimit < ScrollPos.X then
    SetHorzPosition(ScrollLimit);

  ShowScrollBar(Handle, SB_HORZ, (0 < ScrollLimit));
  if ScrollLimit > 0 then
    SetScrollRange(Handle, SB_HORZ, 0, ScrollLimit div dx, true);
end;

function TTraceEdit.GetHorzPosition: integer;
begin
  result := ScrollPos.X;
end;

procedure TTraceEdit.SetHorzPosition(value: integer);
var
  Msg: TWMHScroll;
  dx: integer;
begin
  if value = ScrollPos.X then exit;
  dx := Data.Width div Data.NoOfBases;
  if value < 0 then
    value := 0
  else if value > ScrollLimit then
    value := ScrollLimit;
  Msg.ScrollCode := SB_THUMBPOSITION;
  Msg.Pos        := value div dx;
  WMHScroll(Msg);
end;

function TTraceEdit.SelectedSequence: AnsiString;
var
  i: integer;
begin
  if SelLength > 0 then
  begin
    setlength(result, SelLength);
    for i := 1 to SelLength do
      result[i] := Data.Base[selstart+i-1];
  end
  else
    result := '';
end;

procedure Register;
begin
  RegisterComponents('Tamura Tools', [TTraceEdit]);
end;

end.
