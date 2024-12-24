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

unit msubtreedlg;

{$mode objfpc}{$H+}

interface

{$IFDEF VISUAL_BUILD}
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, StdCtrls, ExtDlgs, Spin, Types, MegaConsts, MTreeBox,
  mimageform;

type

  { TSubtreeDlg }

  TSubtreeDlg = class(TForm)
    ImageLoadBtn: TButton;
    ImageClearBtn: TButton;
    ImageExportBtn: TButton;
    ShowImageCkBx: TCheckBox;
    ImagePositionCBx: TComboBox;
    CompressSubtreeCBx: TCheckBox;
    FillStyleCmbBx: TComboBox;
    FontDlg: TFontDialog;
    Image: TImage;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    NamesFontBtn: TBitBtn;
    CaptionFontBtn: TBitBtn;
    Panel8: TPanel;
    ShowTaxonNameCkBx: TCheckBox;
    ShowNodeMarkerCkBx: TCheckBox;
    ShowTaxonMarkerCkBx: TCheckBox;
    ShowCaptionCBx: TCheckBox;
    CompressedSizeSEdit: TSpinEdit;
    VertAlignCBx: TCheckBox;
    ShowBracketCBx: TCheckBox;
    BracketColorCmbBx: TColorButton;
    ColorCmbBx: TColorButton;
    BranchOptionCmbBx: TComboBox;
    BranchWidthCmbBx: TComboBox;
    BranchStyleCmbBx: TComboBox;
    BracketStyleCmbBx: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OverwriteMarkerCkBx: TCheckBox;
    ClearCkBx: TCheckBox;
    MarkerColorCmbBx: TColorButton;
    MarkerShapeCmbBx: TComboBox;
    DefaultCkBx: TCheckBox;
    HelpBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    NameEditBx: TEdit;
    OkBtn: TBitBtn;
    OpenImageDlg: TOpenPictureDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    MainPanel: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    SaveImageDlg: TSavePictureDialog;
    BracketWidthSEdit: TSpinEdit;
    SubtreeTabSheet: TTabSheet;
    CaptionTabSheet: TTabSheet;
    ImageTabSheet: TTabSheet;
    procedure BracketStyleCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure BranchOptionCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure BranchStyleCmbBxChange(Sender: TObject);
    procedure BranchStyleCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure BranchWidthCmbBxChange(Sender: TObject);
    procedure BranchWidthCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CaptionFontBtnClick(Sender: TObject);
    procedure FillStyleCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageClearBtnClick(Sender: TObject);
    procedure ImageExportBtnClick(Sender: TObject);
    procedure ImageLoadBtnClick(Sender: TObject);
    procedure MarkerShapeCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure NamesFontBtnClick(Sender: TObject);
  private
    SubtreeFont, SubtreeCapFont: TFont;

    procedure SetAlignCaption(value: boolean);
    procedure SetPixelsPerGroupMember(value: integer);
    function GetAlignCaption: boolean;
    function GetPixelsPerGroupMember: integer;
  public
    property AlignCaption: boolean read GetAlignCaption write SetAlignCaption;
    property PixelsPerGroupMember: integer read GetPixelsPerGroupMember write SetPixelsPerGroupMember;

    procedure SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
    procedure GetSubtreeAttrib(NodeAttrib: TNodeAttrib);
  end;

var
  SubtreeDlg: TSubtreeDlg;
{$ENDIF}

implementation

{$IFDEF VISUAL_BUILD}
uses
  TreeExplorer_HC, MegaVerConsts;

{$R *.lfm}

{ TSubtreeDlg }

procedure TSubtreeDlg.CaptionFontBtnClick(Sender: TObject);
begin
  FontDlg.Font.Assign(SubtreeCapFont);
  if FontDlg.Execute then
    SubtreeCapFont.Assign(FontDlg.Font);
end;

procedure TSubtreeDlg.FillStyleCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var point : array[0..3] of TPoint;
begin
  with FillStyleCmbBx do begin
    Canvas.FillRect(arect);
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clBlack;
    point[0].x := aRect.left+2;
    point[1].x := aRect.right-2;
    point[2].x := aRect.right-2;
    point[3].x := aRect.left+2;
    point[0].y := (aRect.top+2);
    point[1].y := (aRect.top+2);
    point[2].y := (aRect.bottom-2);
    point[3].y := (aRect.bottom-2);
    case Index of
      0: Canvas.Brush.Style := bsSolid;
      1: Canvas.Brush.Style := bsClear;
      2: Canvas.Brush.Style := bsBDiagonal;
      3: Canvas.Brush.Style := bsFDiagonal;
      4: Canvas.Brush.Style := bsCross;
      5: Canvas.Brush.Style := bsDiagCross;
      6: Canvas.Brush.Style := bsHorizontal;
      7: Canvas.Brush.Style := bsVertical;
    end;
    Canvas.Polygon(point);
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(aRect.left+90, aRect.top+2, Items[Index]);
  end;
end;

procedure TSubtreeDlg.FormCreate(Sender: TObject);
begin
  HelpContext := HC_Subtree_Drawing_Options;
  SubtreeFont := TFont.Create;
  SubtreeCapFont := TFont.Create;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Subtree Drawing Options';
  ImageForm.UpdateImgList(Self);
end;

procedure TSubtreeDlg.FormDestroy(Sender: TObject);
begin
  SubtreeFont.Free;
  SubtreeCapFont.Free;
end;

procedure TSubtreeDlg.FormShow(Sender: TObject);
begin
  DefaultCkBx.Checked := false;
  if (NameEditBx.Text = '') then
  begin
    PageControl1.ActivePageIndex := 0;
    NameEditBx.SetFocus;
  end
  else
    CancelBtn.SetFocus;
end;

procedure TSubtreeDlg.ImageClearBtnClick(Sender: TObject);
var
  NullBitmap: TBitmap;
begin
  NullBitmap := TBitmap.Create;
  Image.Picture.Bitmap.Assign(NullBitmap);
  Image.Refresh;
  NullBitmap.Free;
  ImageExportBtn.Enabled := not Image.Picture.Bitmap.Empty;
  ImageClearBtn.Enabled := not Image.Picture.Bitmap.Empty;
end;

procedure TSubtreeDlg.ImageExportBtnClick(Sender: TObject);
var
 JpegImage: TJPEGImage;
begin
  if not SaveImageDlg.Execute then exit;
  if UpperCase(ExtractFileExt(SaveImageDlg.FileName)) = '.BMP' then
    Image.Picture.Bitmap.SaveToFile(SaveImageDlg.FileName)
  else
  begin
    JpegImage := TJPEGImage.Create;
    JpegImage.Assign(Image.Picture.Bitmap);
    JpegImage.SaveToFile(SaveImageDlg.FileName);
  end;
end;

procedure TSubtreeDlg.ImageLoadBtnClick(Sender: TObject);
var
 JpegImage: TJPEGImage;
begin
  if not OpenImageDlg.Execute then exit;
  if UpperCase(ExtractFileExt(OpenImageDlg.FileName)) = '.BMP' then
    Image.Picture.Bitmap.LoadFromFile(OpenImageDlg.FileName)
  else
  begin
    JpegImage := TJPEGImage.Create;
    JpegImage.LoadFromFile(OpenImageDlg.FileName);
    Image.Picture.Bitmap.Assign(JpegImage);
  end;
  ImageExportBtn.Enabled := not Image.Picture.Bitmap.Empty;
  ImageClearBtn.Enabled := not Image.Picture.Bitmap.Empty;
end;

procedure TSubtreeDlg.BranchOptionCmbBxDrawItem(Control: TWinControl;Index: Integer; ARect: TRect; State: TOwnerDrawState);
var point : array[0..3] of TPoint;
begin
  with BranchOptionCmbBx do begin
    Canvas.FillRect(arect);
    if Index = 3 then
    begin
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := clBlack;
    end
    else
    begin
      Canvas.Pen.Width := 3;
      Canvas.Pen.Color := clRed;
    end;
    point[0].x := arect.left+55;
    point[1].x := arect.left+30;
    point[2].x := arect.left+30;
    point[3].x := arect.left+55;
    point[0].y := (arect.top+3);
    point[1].y := (arect.top+3);
    point[2].y := (arect.bottom-3);
    point[3].y := (arect.bottom-3);
    Canvas.PolyLine(point);

    point[0].x := arect.left+5;
    point[1].x := arect.left+5;
    point[2].x := arect.left+30;
    point[0].y := arect.bottom-3;
    point[1].y := (arect.top+arect.bottom) div 2;
    point[2].y := (arect.top+arect.bottom) div 2;
    case Index of
      0: begin
           point[0].x := arect.left+5;
           point[1].x := arect.left+5;
           point[2].x := arect.left+30;
           point[0].y := arect.bottom-3;
           point[1].y := (arect.top+arect.bottom) div 2;
           point[2].y := (arect.top+arect.bottom) div 2;
           Canvas.PolyLine(Slice(point,3));
         end;
      1,3: begin
           Canvas.Pen.Width := 3;
           Canvas.Pen.Color := clRed;
           point[0].x := arect.left+5;
           point[1].x := arect.left+30;
           point[0].y := (arect.top+arect.bottom) div 2;
           point[1].y := (arect.top+arect.bottom) div 2;
           Canvas.PolyLine(Slice(point,2));
           Canvas.Pen.Width := 1;
           Canvas.Pen.Color := clBlack;
           point[0].x := arect.left+5;
           point[1].x := arect.left+5;
           point[0].y := arect.bottom-3;
           point[1].y := (arect.top+arect.bottom) div 2 -2;
           Canvas.PolyLine(Slice(point,2));
         end;
      2: begin
           Canvas.Pen.Width := 1;
           Canvas.Pen.Color := clBlack;
           point[0].x := arect.left+5;
           point[1].x := arect.left+5;
           point[2].x := arect.left+30;
           point[0].y := arect.bottom-3;
           point[1].y := (arect.top+arect.bottom) div 2;
           point[2].y := (arect.top+arect.bottom) div 2;
           Canvas.PolyLine(Slice(point,3));
         end;
    end;
  end;
end;

procedure TSubtreeDlg.BracketStyleCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  point : array[0..3] of TPoint;
begin
  with BracketStyleCmbBx do begin
    Canvas.FillRect(arect);
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clBlack;
    case Index of
      0: begin
           point[0].x := aRect.left+3;
           point[1].x := aRect.left+7;
           point[2].x := aRect.left+7;
           point[3].x := aRect.left+2;
           point[0].y := (aRect.top+2);
           point[1].y := (aRect.top+2);
           point[2].y := (aRect.bottom-1);
           point[3].y := (aRect.bottom-1);
           Canvas.PolyLine(point);
         end;
      1: begin
           point[0].x := aRect.left+5;
           point[1].x := aRect.left+5;
           point[0].y := (aRect.top+2);
           point[1].y := (aRect.bottom-1);
           Canvas.PolyLine(Slice(point,2));
         end;
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(aRect.left+12, aRect.top+1, Items[Index]);
  end;
end;

procedure TSubtreeDlg.BranchStyleCmbBxChange(Sender: TObject);
begin
  if BranchStyleCmbBx.ItemIndex <> 0 then
    BranchWidthCmbBx.ItemIndex := 1;
end;

procedure TSubtreeDlg.BranchStyleCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  point : array[0..1] of TPoint;
  textsize : integer;
begin
  with BranchStyleCmbBx do begin
    Canvas.FillRect(arect);
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    textsize := Canvas.TextWidth(Items[1]);
    point[0].x := aRect.left+4;
    point[1].x := aRect.right-textsize-6;
    case Index of
      0   : begin
              Canvas.Pen.Style := psSolid;
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              Canvas.Polyline(point);
            end;
      1   : begin
              Canvas.Pen.Style := psDash;
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              Canvas.Polygon(point);
            end;
      2   : begin
              Canvas.Pen.Style := psDot;
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              Canvas.Polygon(point);
            end;
    end;
    Canvas.TextOut(aRect.right-textsize-2, aRect.top+2, Items[Index]);
  end;
end;

procedure TSubtreeDlg.BranchWidthCmbBxChange(Sender: TObject);
begin
  if BranchWidthCmbBx.ItemIndex <> 1 then
    BranchStyleCmbBx.ItemIndex := 0;
end;

procedure TSubtreeDlg.BranchWidthCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var point : array[0..3] of TPoint;
    textsize : integer;
begin
  with BranchWidthCmbBx do
  begin
    Canvas.FillRect(arect);
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Color := clBlack;
    Canvas.Brush.Style := bsSolid;
    textsize := Canvas.TextWidth(Items[Index]);
    point[0].x := arect.left+4;
    point[1].x := arect.right-textsize-4;
    point[2].x := arect.right-textsize-4;
    point[3].x := arect.left+4;
    case Index of
      0,1 : begin
              point[0].y := (arect.top+arect.bottom) div 2;
              point[1].y := (arect.top+arect.bottom) div 2;
              Canvas.Polyline(Slice(point,2));
            end;
      2   : begin
              point[0].y := (arect.top+arect.bottom) div 2;
              point[1].y := (arect.top+arect.bottom) div 2;
              point[2].y := (arect.top+arect.bottom) div 2 +1;
              point[3].y := (arect.top+arect.bottom) div 2 +1;
              Canvas.Polygon(point);
            end;
      3   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -1;
              point[1].y := (aRect.top+aRect.bottom) div 2 -1;
              point[2].y := (aRect.top+aRect.bottom) div 2 +1;
              point[3].y := (aRect.top+aRect.bottom) div 2 +1;
              Canvas.Polygon(point);
            end;
      4   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -1;
              point[1].y := (aRect.top+aRect.bottom) div 2 -1;
              point[2].y := (aRect.top+aRect.bottom) div 2 +2;
              point[3].y := (aRect.top+aRect.bottom) div 2 +2;
              Canvas.Polygon(point);
            end;
      5   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -2;
              point[1].y := (aRect.top+aRect.bottom) div 2 -2;
              point[2].y := (aRect.top+aRect.bottom) div 2 +2;
              point[3].y := (aRect.top+aRect.bottom) div 2 +2;
              Canvas.Polygon(point);
            end;
      6   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -2;
              point[1].y := (aRect.top+aRect.bottom) div 2 -2;
              point[2].y := (aRect.top+aRect.bottom) div 2 +3;
              point[3].y := (aRect.top+aRect.bottom) div 2 +3;
              Canvas.Polygon(point);
            end;
      7   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -3;
              point[1].y := (aRect.top+aRect.bottom) div 2 -3;
              point[2].y := (aRect.top+aRect.bottom) div 2 +4;
              point[3].y := (aRect.top+aRect.bottom) div 2 +4;
              Canvas.Polygon(point);
            end;
      8  : begin
              point[0].y := (aRect.top+aRect.bottom) div 2 -4;
              point[1].y := (aRect.top+aRect.bottom) div 2 -4;
              point[2].y := (aRect.top+aRect.bottom) div 2 +5;
              point[3].y := (aRect.top+aRect.bottom) div 2 +5;
              Canvas.Polygon(point);
            end;
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(aRect.right-textsize-2, aRect.top+2, Items[Index]);
  end;
end;

procedure TSubtreeDlg.MarkerShapeCmbBxDrawItem(Control: TWinControl;Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  d,c : integer;
  p : array[0..3] of TPoint;
begin
  with MarkerShapeCmbBx.Canvas do
  begin
    d := (aRect.Bottom - aRect.Top) div 2;
    c := Canvas.TextWidth('N')+d;

    FillRect(aRect);
    Pen.Color := clBlack;
    Brush.Color := clBlack;
    case Index of
      1,3,5,7,9  : Brush.Style := bsClear;
      2,4,6,8,10 : Brush.Style := bsSolid;
    end;
    case Index of
      1,2 : Ellipse(c-d+2, aRect.Top+2, c+d-1, aRect.Bottom-1);
      3,4 : Rectangle(c-d+2, aRect.Top+2, c+d-2, aRect.Bottom-2);
      5,6 : begin
              p[0].x := c;
              p[0].y := aRect.Top +3;
              p[1].x := c-d +3;
              p[1].y := aRect.Bottom -2;
              p[2].x := c+d -3;
              p[2].y := aRect.Bottom -2;
              Polygon(Slice(p,3));
            end;
      7,8 : begin
              p[0].x := c-d +3;
              p[0].y := aRect.Top +3;
              p[1].x := c+d -3;
              p[1].y := aRect.Top +3;
              p[2].x := c;
              p[2].y := aRect.Bottom -2;
              Polygon(Slice(p,3));
            end;
      9,10: begin
              p[0].x := c;
              p[0].y := aRect.Top +2;
              p[1].x := c-d +2;
              p[1].y := aRect.Top +d;
              p[2].x := c;
              p[2].y := aRect.Bottom -2;
              p[3].x := c+d -2;
              p[3].y := aRect.Top +d;
              Polygon(p);
            end;
    end;
    Brush.Style := bsClear;
    TextOut(aRect.Left+2, aRect.Top, MarkerShapeCmbBx.Items[Index]);
  end;
end;

procedure TSubtreeDlg.NamesFontBtnClick(Sender: TObject);
begin
  FontDlg.Font.Assign(SubtreeFont);
  if FontDlg.Execute then
    SubtreeFont.Assign(FontDlg.Font);
end;

procedure TSubtreeDlg.SetAlignCaption(value: boolean);
begin
  VertAlignCBx.Checked := value;
end;

procedure TSubtreeDlg.SetPixelsPerGroupMember(value: integer);
begin
  CompressedSizeSEdit.Value := value;
end;

function TSubtreeDlg.GetAlignCaption: boolean;
begin
  result := VertAlignCBx.Checked;
end;

function TSubtreeDlg.GetPixelsPerGroupMember: integer;
begin
  result := CompressedSizeSEdit.Value;
end;

procedure TSubtreeDlg.SetSubtreeAttrib(NodeAttrib: TNodeAttrib);
begin
  ShowTaxonNameCkBx.Checked := NodeAttrib.ShowTaxonName;
  ShowTaxonMarkerCkBx.Checked := NodeAttrib.ShowTaxonMarker;
  OverwriteMarkerCkBx.Checked := NodeAttrib.OverwriteMarker;

  NameEditBx.Text := NodeAttrib.Caption;
  ShowCaptionCBx.Checked     := NodeAttrib.ShowCaption;
  ShowNodeMarkerCkBx.Checked := NodeAttrib.ShowNodeMarker;
  ShowBracketCBx.Checked     := NodeAttrib.ShowBracket;
  case NodeAttrib.BracketStyle of
   brsSquare: BracketStyleCmbBx.ItemIndex := 0;
   brsLine  : BracketStyleCmbBx.ItemIndex := 1;
   brsNone  : BracketStyleCmbBx.ItemIndex := 2;
  end;
  BracketColorCmbBx.ButtonColor := NodeAttrib.BracketColor;
  BracketWidthSEdit.Value := NodeAttrib.BracketLineWidth;
  SubtreeFont.Assign(NodeAttrib.Font);
  SubtreeCapFont.Assign(NodeAttrib.CaptionFont);
  ColorCmbBx.ButtonColor := NodeAttrib.LineColor;
  case NodeAttrib.FillStyle of
    bsSolid:      FillStyleCmbBx.ItemIndex := 0;
    bsClear:      FillStyleCmbBx.ItemIndex := 1;
    bsBDiagonal:  FillStyleCmbBx.ItemIndex := 2;
    bsFDiagonal:  FillStyleCmbBx.ItemIndex := 3;
    bsCross:      FillStyleCmbBx.ItemIndex := 4;
    bsDiagCross:  FillStyleCmbBx.ItemIndex := 5;
    bsHorizontal: FillStyleCmbBx.ItemIndex := 6;
    bsVertical:   FillStyleCmbBx.ItemIndex := 7;
  end;
  BranchWidthCmbBx.ItemIndex := NodeAttrib.LineWidth;
  case NodeAttrib.LineStyle of
    psSolid : BranchStyleCmbBx.ItemIndex := 0;
    psDash  : BranchStyleCmbBx.ItemIndex := 1;
    psDot   : BranchStyleCmbBx.ItemIndex := 2;
  end;
  case NodeAttrib.BranchOption of
    boFullBranch: BranchOptionCmbBx.ItemIndex := 0;
    boHalfBranch: BranchOptionCmbBx.ItemIndex := 1;
    boNoBranch  : BranchOptionCmbBx.ItemIndex := 2;
    boBranchOnly: BranchOptionCmbBx.ItemIndex := 3;
  end;
  MarkerShapeCmbBx.ItemIndex := integer(NodeAttrib.Marker.Shape);
  MarkerColorCmbBx.ButtonColor := NodeAttrib.Marker.Color;
  CompressSubtreeCBx.Checked := NodeAttrib.ManualCompressed;

  ShowImageCkBx.Checked := NodeAttrib.ShowImage;
  case NodeAttrib.GraphicAlign of
    gaRight : ImagePositionCBx.ItemIndex := 0;
    gaLeft  : ImagePositionCBx.ItemIndex := 1;
    gaTop   : ImagePositionCBx.ItemIndex := 2;
    gaBottom: ImagePositionCBx.ItemIndex := 3;
  end;
  Image.Picture.Bitmap.Assign(NodeAttrib.Image);
  ImageExportBtn.Enabled := not Image.Picture.Bitmap.Empty;
  ImageClearBtn.Enabled := not Image.Picture.Bitmap.Empty;

  ClearCkBx.Checked := NodeAttrib.OverwriteDownstream;
end;

procedure TSubtreeDlg.GetSubtreeAttrib(NodeAttrib: TNodeAttrib);
begin
  NodeAttrib.Caption := NameEditBx.Text;
  NodeAttrib.ShowCaption := ShowCaptionCBx.Checked;
  NodeAttrib.ShowNodeMarker  := ShowNodeMarkerCkBx.Checked;
  NodeAttrib.ShowBracket := ShowBracketCBx.Checked;
  NodeAttrib.Font.Assign(SubtreeFont);
  NodeAttrib.CaptionFont.Assign(SubtreeCapFont);
  case BracketStyleCmbBx.ItemIndex of
    0: NodeAttrib.BracketStyle := brsSquare;
    1: NodeAttrib.BracketStyle := brsLine;
    2: NodeAttrib.BracketStyle := brsNone;
  end;
  NodeAttrib.BracketColor := BracketColorCmbBx.ButtonColor;
  NodeAttrib.BracketLineWidth := BracketWidthSEdit.Value;

  NodeAttrib.LineColor := ColorCmbBx.ButtonColor;
  case FillStyleCmbBx.ItemIndex of
    0: NodeAttrib.FillStyle := bsSolid;
    1: NodeAttrib.FillStyle := bsClear;
    2: NodeAttrib.FillStyle := bsBDiagonal;
    3: NodeAttrib.FillStyle := bsFDiagonal;
    4: NodeAttrib.FillStyle := bsCross;
    5: NodeAttrib.FillStyle := bsDiagCross;
    6: NodeAttrib.FillStyle := bsHorizontal;
    7: NodeAttrib.FillStyle := bsVertical;
  end;
  NodeAttrib.LineWidth := BranchWidthCmbBx.ItemIndex;
  case BranchStyleCmbBx.ItemIndex of
    0: NodeAttrib.LineStyle := psSolid;
    1: NodeAttrib.LineStyle := psDash;
    2: NodeAttrib.LineStyle := psDot;
  end;
  case BranchOptionCmbBx.ItemIndex of
    0: NodeAttrib.BranchOption := boFullBranch;
    1: NodeAttrib.BranchOption := boHalfBranch;
    2: NodeAttrib.BranchOption := boNoBranch;
    3: NodeAttrib.BranchOption := boBranchOnly;
  end;

  NodeAttrib.ShowTaxonName   := ShowTaxonNameCkBx.Checked;
  NodeAttrib.ShowTaxonMarker := ShowTaxonMarkerCkBx.Checked;

  NodeAttrib.Marker.Shape := TNodeMarkerShape(MarkerShapeCmbBx.ItemIndex);
  NodeAttrib.Marker.Color := MarkerColorCmbBx.ButtonColor;
  NodeAttrib.ManualCompressed   := CompressSubtreeCBx.Checked;

  case ImagePositionCBx.ItemIndex of
    0: NodeAttrib.GraphicAlign := gaRight;
    1: NodeAttrib.GraphicAlign := gaLeft;
    2: NodeAttrib.GraphicAlign := gaTop;
    3: NodeAttrib.GraphicAlign := gaBottom;
  end;
  If not Image.Picture.Bitmap.Empty then
    NodeAttrib.Image.Assign(Image.Picture.Bitmap);
  NodeAttrib.ShowImage := ShowImageCkBx.Checked;

  NodeAttrib.OverwriteMarker := OverwriteMarkerCkBx.Checked;
  NodeAttrib.OverwriteDownstream := ClearCkBx.Checked;
end;
{$ENDIF}
end.

