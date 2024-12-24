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

unit mformatdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, StdCtrls, Spin, ActnList, Types, MTreeBox, mimageform;

type
  //{$IFNDEF DARWIN}
  //TMarkerStyleArray = array[0..MaxInt] of TNodeMarker;
  //{$ELSE}
  //TMarkerStyleArray = array[0..65535] of TNodeMarker;
  //{$ENDIF}
  TMarkerStyleArray = array of TNodeMarker;

  { TFormatDlg }

  TFormatDlg = class(TForm)
    ActionTaxonFont: TAction;
    ActionGroupFont: TAction;
    ActionScaleBarFont: TAction;
    ActionBranchInfoFont: TAction;
    ActionTimesFont: TAction;
    ActionBLensFont: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    ShowHeightErrBarCkBx: TCheckBox;
    HelpBtn: TBitBtn;
    OkBtn: TBitBtn;
    edtTimeLength: TSpinEdit;
    edtTimeTick: TSpinEdit;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    CondensedLabel: TLabel;
    ConsensusLabel: TLabel;
    CondensedPercentLabel: TLabel;
    ConsensusPercentLabel: TLabel;
    Panel7: TPanel;
    ShowScaleCkBx: TCheckBox;
    ScaleWidthCmbBx: TComboBox;
    ShowTimeScaleCkBx: TCheckBox;
    CondensedSEdit: TSpinEdit;
    ConsensusSEdit: TSpinEdit;
    UnitEdit: TEdit;
    Label41: TLabel;
    ScaleThickLabel: TLabel;
    ScaleLengthLabel: TLabel;
    Label44: TLabel;
    MarkerColorCmbBx: TColorButton;
    MarkerShapeCmbBx: TComboBox;
    Label39: TLabel;
    Label40: TLabel;
    Panel6: TPanel;
    edtScaleLength: TSpinEdit;
    edtTick: TSpinEdit;
    TaxonListBx: TListBox;
    ShowTaxonMarkerCkBx: TCheckBox;
    Panel5: TPanel;
    TaxonFontBtn: TBitBtn;
    ShowTaxonNameCkBx: TCheckBox;
    edtBLenCutoff: TFloatSpinEdit;
    InfoFontBtn: TBitBtn;
    CancelBtn: TBitBtn;
    BranchWidthCmbBx: TComboBox;
    BLenFontBtn: TBitBtn;
    Label37: TLabel;
    Label38: TLabel;
    TimesFontBtn: TBitBtn;
    BLenPlaceLabel: TLabel;
    BLenDecimalsLabel: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    ShowStatsCkBx: TCheckBox;
    ShowBLenCkBx: TCheckBox;
    ShowTimesChkBx: TCheckBox;
    DivTimeDecimalsSEdit: TSpinEdit;
    StatsHideCkBx: TCheckBox;
    BLenHideCkBx: TCheckBox;
    BLenDecimalsSEdit: TSpinEdit;
    TimesHMarginSpinEdit: TSpinEdit;
    StatsPositionCmbBx: TComboBox;
    HolzNameCBx2: TCheckBox;
    HolzNameCBx1: TCheckBox;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    BLenUnitLabel2: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    StatsPlaceLabel: TLabel;
    StatsHOffsetLabel: TLabel;
    Label28: TLabel;
    StatsVOffsetLabel: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    BLenUnitLabel: TLabel;
    Label9: TLabel;
    AngleSEdit: TSpinEdit;
    Panel2: TPanel;
    RadiusSEdit: TSpinEdit;
    MarginSEdit: TSpinEdit;
    AngleSEdit2: TSpinEdit;
    HorzSEdit2: TSpinEdit;
    StatsCutoffSEdit: TSpinEdit;
    StatsHMarginSEdit: TSpinEdit;
    BLenPositionCmbBx: TComboBox;
    DivTimePositionComboBox: TComboBox;
    StatsVMarginSEdit: TSpinEdit;
    TimesVMarginSpinEdit: TSpinEdit;
    TimeUnitEdit: TEdit;
    VertSEdit: TSpinEdit;
    HorzSEdit1: TSpinEdit;
    WidthSEdit: TSpinEdit;
    TreeImage: TImage;
    PageControl: TPageControl;
    RectTreeTabSheet: TTabSheet;
    CircleTreeTabSheet: TTabSheet;
    RadiationTreeTabSheet: TTabSheet;
    TreeShapePageControl: TPageControl;
    Panel1: TPanel;
    TreeTabSheet: TTabSheet;
    BranchTabSheet: TTabSheet;
    TaxonTabSheet: TTabSheet;
    ScaleTabSheet: TTabSheet;
    CutoffTabSheet: TTabSheet;
    procedure ActionBLensFontExecute(Sender: TObject);
    procedure ActionBranchInfoFontExecute(Sender: TObject);
    procedure ActionGroupFontExecute(Sender: TObject);
    procedure ActionScaleBarFontExecute(Sender: TObject);
    procedure ActionTaxonFontExecute(Sender: TObject);
    procedure ActionTimesFontExecute(Sender: TObject);
    procedure BranchWidthCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure CancelBtnClick(Sender: TObject);
    procedure edtScaleLengthExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MarkerColorCmbBxColorChanged(Sender: TObject);
    procedure MarkerShapeCmbBxChange(Sender: TObject);
    procedure MarkerShapeCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OkBtnClick(Sender: TObject);
    procedure ScaleWidthCmbBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure TaxonListBxClick(Sender: TObject);
    procedure TaxonListBxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    Marker : TMarkerStyleArray;
    HorzFactor, HorzRatio : double;
    procedure InitForm;
    procedure FocusInvalidValue(SpinEdit: TSpinEdit);
    procedure FocusInvalidFloatValue(SpinEdit: TFloatSpinEdit);
    function FindPageForSpinEdit(SpinEdit: TSpinEdit): TTabSheet;
    function FindPageForFloatSpinEdit(SpinEdit: TFloatSpinEdit): TTabSheet;
    function ValidateSpinEdits: Boolean;
  public
    EnableBlenOptions: Boolean;
  end;

var
  FormatDlg: TFormatDlg;

implementation

uses
  MTreeViewForm, TreeExplorer_HC, MegaVerConsts, MegaConsts,
  math;


{$R *.lfm}

{ TFormatDlg }

procedure TFormatDlg.CancelBtnClick(Sender: TObject);
begin

end;

procedure TFormatDlg.edtScaleLengthExit(Sender: TObject);
var
  f: Double;
begin
  if not TryStrToFloat(edtScaleLength.Text, f) then
  begin
    FocusInvalidValue(edtScaleLength);
    Exit;
  end;
  edtTick.MaxValue := edtScaleLength.Value;
end;

procedure TFormatDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SetLength(Marker, 0);
  if ModalResult <> mrOK then
    Exit;
  if not OKBtn.Focused then
  begin
    OKBtn.SetFocus;
    OKBtn.Click;
  end;
end;

procedure TFormatDlg.FormCreate(Sender: TObject);
begin
  EnableBlenOptions := true;
  BranchWidthCmbBx.Canvas.Pen.Mode := pmCopy;
  BranchWidthCmbBx.Canvas.Pen.Width := 0;
  BranchWidthCmbBx.Canvas.Pen.Style := psSolid;

  TaxonListBx.Canvas.Pen.Mode := pmCopy;
  TaxonListBx.Canvas.Pen.Width := 0;
  TaxonListBx.Canvas.Pen.Style := psSolid;
  MarkerShapeCmbBx.Canvas.Pen.Mode := pmCopy;
  MarkerShapeCmbBx.Canvas.Pen.Width := 0;
  MarkerShapeCmbBx.Canvas.Pen.Style := psSolid;

  ScaleWidthCmbBx.Canvas.Pen.Mode := pmCopy;
  ScaleWidthCmbBx.Canvas.Pen.Width := 0;
  ScaleWidthCmbBx.Canvas.Pen.Style := psSolid;

  HelpContext         := HC_Format_dialog_box_in_Tree_Explorer;
  HelpBtn.Helpcontext := HC_Format_dialog_box_in_Tree_Explorer;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Tree Options';
  ImageForm.UpdateImgList(Self);
end;

procedure TFormatDlg.FormShow(Sender: TObject);
var i: integer;
begin
  try
    BeginFormUpdate;

    TaxonListBx.Items.Clear;
    with Owner as TTreeViewForm do
    begin
      SetLength(Marker, Tree.NoOfOtus);
      for i := 1 to Tree.NoOfOTUs do
      begin
        Marker[i - 1] := Tree.Marker[i];
        TaxonListBx.Items.Add(Tree.OTUName[i]);
      end;

      case Tree.TreeStyle of
        tsTraditional: TreeShapePageControl.ActivePage := RectTreeTabSheet;
        tsCircle     : TreeShapePageControl.ActivePage := CircleTreeTabSheet;
        tsRadiation  : TreeShapePageControl.ActivePage := RadiationTreeTabSheet;
      end;
      if Tree.ShowDivergenceTimes then
        ShowTimesChkBx.Checked := True
      else
        ShowTimesChkBx.Checked := False;
    end;

    MarkerShapeCmbBx.ItemIndex := 0;
    InitForm;
  finally
    EndFormUpdate;
  end;
end;

procedure TFormatDlg.MarkerColorCmbBxColorChanged(Sender: TObject);
var
  i : integer;
begin
  if TaxonListBx.Focused then Exit;
  for i := 0 to TaxonListBx.Items.Count-1 do
    if TaxonListBx.Selected[i] then
      Marker[i].Color := MarkerColorCmbBx.ButtonColor;
  TaxonListBx.Refresh;
end;

procedure TFormatDlg.MarkerShapeCmbBxChange(Sender: TObject);
var
  i : integer;
begin
  if TaxonListBx.Focused then Exit;
  for i := 0 to TaxonListBx.Items.Count-1 do
    if TaxonListBx.Selected[i] then
      Marker[i].Shape := TNodeMarkerShape(MarkerShapeCmbBx.ItemIndex);
  TaxonListBx.Refresh;
end;

procedure TFormatDlg.MarkerShapeCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  d,c : integer;
  p : array[0..3] of TPoint;
begin
  with MarkerShapeCmbBx.Canvas do begin
    d := (aRect.Bottom -aRect.Top) div 2;
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

procedure TFormatDlg.OkBtnClick(Sender: TObject);
var
  TempMargin: TPoint;
  a: TNodeAttrib;
  i: integer;
  AOwner: TTreeViewForm;
begin
  AOwner := TTreeViewForm(Owner);
  with aOwner do
  begin
    a := TNodeAttrib.Create;
    if Tree.GroupAttrib.Count > 0 then
      for i := 0 to Tree.GroupAttrib.Count-1 do
        if Tree.GroupAttrib[i].LineWidth = Tree.BranchPen.Width then
        begin
          a.Assign(Tree.GroupAttrib[i]);
          a.LineWidth := BranchWidthCmbBx.ItemIndex;
          Tree.SetGroupAttrib(a, a.Caption);
        end;
    if Tree.AttribList.Count > 1 then
      for i := 1 to Tree.AttribList.Count-1 do
        if Tree.AttribList[i].LineWidth = Tree.BranchPen.Width then
        begin
          a.Assign(Tree.AttribList[i]);
          a.LineWidth := BranchWidthCmbBx.ItemIndex;
          Tree.SetSubtreeAttrib(a, a.NodeIndex);
        end;

    Tree.BranchPen.Width := BranchWidthCmbBx.ItemIndex;
    Tree.AttribList[0].LineWidth := Tree.BranchPen.Width;
    Tree.CondenseValue := CondensedSEdit.Value;
    Tree.ConsensusValue := ConsensusSEdit.Value;

    if Tree.isStats then
    begin
      if ShowStatsCkBx.Checked then
        Tree.ShowStats := true
      else
        Tree.ShowStats := false;
      if StatsHideCkBx.Checked then
        Tree.StatsCutoff := StatsCutoffSEdit.Value
      else
        Tree.StatsCutoff := 0;
      Tree.StatsPosition := TBranchInfoPosition(StatsPositionCmbBx.ItemIndex);
      TempMargin.X := StatsHMarginSEdit.Value*4;
      TempMargin.Y := StatsVMarginSEdit.Value*4;
      Tree.StatsMargin := TempMargin;
    end;

    if Tree.IsTimes then
    begin
      if ShowTimesChkBx.Checked then
      begin
        Tree.ShowDivergenceTimes := True;
        ActionDisplayDivergenceTimes.Checked := True;
        FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'True');
      end
      else
      begin
        Tree.ShowDivergenceTimes := False;
        ActionDisplayDivergenceTimes.Checked := False;
        FigureGenerator.AssignData(DISP_DIV_TIMES_STR, 'False')
      end;

      Tree.TimesPosition := TBranchInfoPosition(DivTimePositionComboBox.ItemIndex);
      TempMargin.X := TimesHMarginSpinEdit.Value * 4;
      TempMargin.Y := TimesVMarginSpinEdit.Value * 4;
      Tree.TimesMargin := TempMargin;
      Tree.DivTimeDecimals := DivTimeDecimalsSEdit.Value;
    end;

    if EnableBLenOptions then
    begin
      if Tree.isBranchLength then
      begin
        if ShowBLenCkBx.Checked then
          Tree.ShowBLen := true
        else
          Tree.ShowBLen := false;
        if BLenHideCkBx.Checked then
          Tree.BLenCutoff := edtBLenCutoff.Value
        else
          Tree.BLenCutoff := Tree.MinBranchLength;
        Tree.BLenPosition := TBranchInfoPosition(BLenPositionCmbBx.ItemIndex);
      end;
      if Tree.isBranchLength then
        Tree.BLenDecimals := BLenDecimalsSEdit.Value;
      if IsBootTree and BootTree.isBranchLength then
        BootTree.BLenDecimals := BLenDecimalsSEdit.Value;
    end;


    if ShowTaxonNameCkBx.Checked then
      Tree.ShowOTUName := true
    else
      Tree.ShowOTUName := false;
    if ShowTaxonMarkerCkBx.Checked then
      Tree.ShowOTUMarker := true
    else
      Tree.ShowOTUMarker := false;
    for i := 1 to Tree.NoOfOTUs do
        Tree.Marker[i] := Marker[i - 1];

    if Tree.IsScale then
    begin
      if OriTree.isScale then begin
        OriTree.ShowScale := ShowScaleCkBx.Checked;
        OriTree.ScalePen.Width := ScaleWidthCmbBx.ItemIndex;
        OriTree.ScaleUnit := UnitEdit.Text;
        OriTree.ScaleText := edtScaleLength.Text;
        OriTree.ScaleTick := edtTick.Value;
      end;
      if IsBootTree and BootTree.isScale then begin
        BootTree.ShowScale := ShowScaleCkBx.Checked;
        BootTree.ScalePen.Width := ScaleWidthCmbBx.ItemIndex;
        BootTree.ScaleUnit := UnitEdit.Text;
        BootTree.ScaleText := edtScaleLength.Text;
        BootTree.ScaleTick := edtTick.Value;
      end;
      if IsReltimeTree and ReltimeTree.isScale then begin
        ReltimeTree.ShowScale := ShowScaleCkBx.Checked;
        ReltimeTree.ScalePen.Width := ScaleWidthCmbBx.ItemIndex;
        ReltimeTree.ScaleUnit := UnitEdit.Text;
        ReltimeTree.ScaleText := edtScaleLength.Text;
        ReltimeTree.ScaleTick := edtTick.Value;
        if ReltimeTree.TimeFactor > 0.0 then begin
          ReltimeTree.ShowTimeScale := ShowTimeScaleCkBx.Checked;
          ReltimeTree.TimeUnit := TimeUnitEdit.Text;
          ReltimeTree.TimeText := edtTimeLength.Text;
          ReltimeTree.TimeTick := edtTimeTick.Value;
        end;
      end;
    end;

    case Tree.TreeStyle of
      tsTraditional:
        begin
          if Tree.ShowTopologyOnly or (not Tree.isBranchLength) then
            Tree.TreeWidth := WidthSEdit.Value
          else
            Tree.PixelsPerUnit := HorzSEdit1.Value/HorzFactor;
          Tree.PixelsPerOTU := VertSEdit.Value;
        end;
      tsRadiation:
        begin
          Tree.PixelsPerUnit := HorzSEdit2.Value/HorzFactor;
          Tree.StartAngle := AngleSEdit2.Value;
          Tree.HorzTaxonName := HolzNameCBx2.Checked;
        end;
      tsCircle:
        begin
          Tree.Radius := RadiusSEdit.Value;
          Tree.StartAngle := AngleSEdit.Value;
          Tree.CenterMargin := MarginSEdit.Value;
          Tree.HorzTaxonName := HolzNameCBx1.Checked;
        end;
    end;

    Tree.ShowHeightErrBar := ShowHeightErrBarCkBx.Checked;
    ReltimeTree.ShowHeightErrBar := ShowHeightErrBarCkBx.Checked;
    ActionDisplayErrorBars.Checked := Tree.ShowHeightErrBar;
    if ReltimeTree.ShowHeightErrBar then
      FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'True')
    else
      FigureGenerator.AssignData(DISP_ERR_BARS_STR, 'False');

    if IsBootTree then
      if Tree = OriTree then
      begin
        BootTree.AssignTreeAttrib(OriTree);
        for i := 1 to Tree.NoOfOTUs do
          BootTree.Marker[i] := OriTree.Marker[i];
      end
      else if Tree = BootTree then
      begin
        OriTree.AssignTreeAttrib(BootTree);
        for i := 1 to Tree.NoOfOTUs do
          OriTree.Marker[i] := BootTree.Marker[i];
      end;
       UpdateInfoBox;
    Tree.Refresh;
  end;
  a.Free;

  ModalResult := mrOK;
end;

procedure TFormatDlg.ScaleWidthCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  point : array[0..3] of TPoint;
  textsize : integer;
begin
  with ScaleWidthCmbBx do begin
    Canvas.FillRect(aRect);
    Canvas.Pen.Color := Canvas.Font.Color;
    Canvas.Brush.Color := Canvas.Font.Color;
    Canvas.Brush.Style := bsSolid;
    textsize := Canvas.TextWidth(Items[Index]);
    point[0].x := aRect.left+4;
    point[1].x := aRect.right -textsize -8;
    point[2].x := aRect.right -textsize -8;
    point[3].x := aRect.left+4;
    case Index of
      0,1 : begin
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              Canvas.Polyline(Slice(point,2));
            end;
      2   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              point[2].y := (aRect.top+aRect.bottom) div 2 +1;
              point[3].y := (aRect.top+aRect.bottom) div 2 +1;
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
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(aRect.right-textsize-2, aRect.top, Items[Index]);
  end;
end;

procedure TFormatDlg.TaxonListBxClick(Sender: TObject);
begin
  Marker[TaxonListBx.ItemIndex].Shape := TNodeMarkerShape(MarkerShapeCmbBx.ItemIndex);
  Marker[TaxonListBx.ItemIndex].Color := MarkerColorCmbBx.ButtonColor;
  TaxonListBx.Refresh;
end;

procedure TFormatDlg.TaxonListBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  d : integer;
  p : array[0..3] of TPoint;
begin
  //if not Assigned(Marker) then
  //  Exit;
  d := aRect.Bottom -aRect.Top;
  with TaxonListBx.Canvas do begin
    FillRect(aRect);
    Pen.Color := Marker[Index].Color;
    Brush.Color := Marker[Index].Color;
    case integer(Marker[Index].Shape) of
      1,3,5,7,9  : Brush.Style := bsClear;
      2,4,6,8,10 : Brush.Style := bsSolid;
    end;
    case integer(Marker[Index].Shape) of
      1,2 : Ellipse(aRect.Left+2, aRect.Top+2, aRect.Left+d-1, aRect.Bottom-1);
      3,4 : Rectangle(aRect.Left+2, aRect.Top+2, aRect.Left+d-2, aRect.Bottom-2);
      5,6 : begin
              p[0].x := aRect.Left +(d div 2);
              p[0].y := aRect.Top +3;
              p[1].x := aRect.Left +3;
              p[1].y := aRect.Top +d -2;
              p[2].x := aRect.Left +d -3;
              p[2].y := aRect.Top +d -2;
              Polygon(Slice(p,3));
            end;
      7,8 : begin
              p[0].x := aRect.Left +3;
              p[0].y := aRect.Top +3;
              p[1].x := aRect.Left +d -3;
              p[1].y := aRect.Top +3;
              p[2].x := aRect.Left +(d div 2);
              p[2].y := aRect.Top + d -2;
              Polygon(Slice(p,3));
            end;
      9,10: begin
              p[0].x := aRect.Left +(d div 2);
              p[0].y := aRect.Top +2;
              p[1].x := p[0].X -(d div 2) +2;
              p[1].y := aRect.Top +(d div 2);
              p[2].x := aRect.Left +(d div 2);
              p[2].y := aRect.Bottom -2;
              p[3].x := p[0].X +(d div 2) -2;
              p[3].y := aRect.Top +(d div 2);
              Polygon(p);
            end;
    end;
    Brush.Style := bsClear;
    TextOut(aRect.Left+d+2, aRect.Top, TaxonListBx.Items[Index]);
  end;
end;

procedure TFormatDlg.InitForm;
var
  TempMargin: TPoint;
  AOwner: TTreeViewForm;
begin
  AOwner := TTreeViewForm(Owner);
  ActionBranchInfoFont.Enabled := (Owner as TTreeViewForm).ActionBranchInfoFont.Enabled;
  ActionScaleBarFont.Enabled := (Owner as TTreeViewForm).ActionScaleBarFont.Enabled;
  with Owner as TTreeViewForm do
  begin
    BranchWidthCmbBx.ItemIndex := Tree.BranchPen.Width;
    CondensedSEdit.Value := Tree.CondenseValue;
    ConsensusSEdit.Value := Tree.ConsensusValue;
    if Tree.isStats and (Tree.TreeStyle <> tsRadiation) and (not ActionConsensus.Checked) then
    begin
      CondensedLabel.Enabled := true;
      CondensedPercentLabel.Enabled := true;
      CondensedSEdit.Enabled := true;
    end
    else
    begin
      CondensedLabel.Enabled := false;
      CondensedPercentLabel.Enabled := false;
      CondensedSEdit.Enabled := false;
    end;
    if Tree.isConsensus and (Tree.TreeStyle <> tsRadiation) and ActionConsensus.Checked then
    begin
      ConsensusLabel.Enabled := true;
      ConsensusPercentLabel.Enabled := true;
      ConsensusSEdit.Enabled := true;
    end
    else begin
      ConsensusLabel.Enabled := false;
      ConsensusPercentLabel.Enabled := false;
      ConsensusSEdit.Enabled := false;
    end;

    InfoFontBtn.Enabled := false;

    if Tree.isTimes then
    begin
      TimesHMarginSpinEdit.Enabled := True;
      TimesVMarginSpinEdit.Enabled := True;
      TempMargin := Tree.TimesMargin;
      TimesHMarginSpinEdit.Value := TempMargin.X div 4;
      TimesVMarginSpinEdit.Value := TempMargin.Y div 4;
      TimesFontBtn.Enabled := True;
      DivTimePositionComboBox.Enabled := True;
      DivTimePositionComboBox.ItemIndex := Integer(Tree.TimesPosition);
      DivTimePositionComboBox.Color := clWindow;
      DivTimeDecimalsSEdit.Enabled := True;
      DivTimeDecimalsSEdit.Value := Tree.DivTimeDecimals;
      ShowTimesChkBx.Checked := Tree.ShowDivergenceTimes;
      ShowTimesChkBx.Enabled := True;
    end
    else
    begin
      TimesHMarginSpinEdit.Enabled := False;
      TimesVMarginSpinEdit.Enabled := False;
      TimesFontBtn.Enabled := False;
      DivTimePositionComboBox.Enabled := False;
      DivTimePositionComboBox.ItemIndex := -1;
      DivTimePositionComboBox.Color := clBtnFace;
      DivTimeDecimalsSEdit.Enabled := False;
      DivTimeDecimalsSEdit.Value := 0;
      ShowTimesChkBx.Enabled := False;
      ShowTimesChkBx.Checked := False;
    end;

    if Tree.isStats then
    begin
      InfoFontBtn.Enabled := true;
      ShowStatsCkBx.Enabled := true;
      StatsHideCkBx.Enabled := true;
      StatsCutoffSEdit.Enabled := true;
      StatsPositionCmbBx.Enabled := true;
      StatsHMarginSEdit.Enabled := true;
      StatsVMarginSEdit.Enabled := true;

      ShowStatsCkBx.Enabled := true;
      StatsHideCkBx.Enabled := true;
      StatsPlaceLabel.Enabled := true;
      StatsHoffsetLabel.Enabled := true;
      StatsVoffsetLabel.Enabled := true;
      Label1.Enabled := true;
      Label14.Enabled := true;
      StatsPositionCmbBx.Color := clWindow;

      if Tree.ShowStats then
        ShowStatsCkBx.Checked := true
      else
        ShowStatsCkBx.Checked := false;
      if Tree.StatsCutoff = 0 then
        StatsHideCkBx.Checked := false
      else
      begin
        StatsHideCkBx.Checked := true;
        StatsCutoffSEdit.Value := Tree.StatsCutoff;
      end;

      StatsPositionCmbBx.ItemIndex := integer(Tree.StatsPosition);
      StatsHMarginSEdit.MinValue := -Tree.PixelsPerOTU;
      StatsHMarginSEdit.MaxValue := Tree.PixelsPerOTU;
      StatsVMarginSEdit.MinValue := -Tree.PixelsPerOTU div 2;
      StatsVMarginSEdit.MaxValue := Tree.PixelsPerOTU div 2;
      TempMargin := Tree.StatsMargin;
      StatsHMarginSEdit.Value := TempMargin.X div 4;
      StatsVMarginSEdit.Value := TempMargin.Y div 4;
    end
    else
    begin
      ShowStatsCkBx.Enabled := false;
      StatsHideCkBx.Enabled := false;
      StatsCutoffSEdit.Enabled := false;
      StatsPositionCmbBx.Enabled := false;
      StatsHMarginSEdit.Enabled := false;
      StatsVMarginSEdit.Enabled := false;

      ShowStatsCkBx.Enabled := false;
      StatsHideCkBx.Enabled := false;
      StatsPlaceLabel.Enabled := false;
      StatsHoffsetLabel.Enabled := false;
      StatsVoffsetLabel.Enabled := false;
      Label1.Enabled := false;
      Label14.Enabled := false;
      StatsPositionCmbBx.Color := clBtnFace;
    end;

    if (Tree.isBranchLength and EnableBLenOptions
    and (Tree.TreeStyle = tsTraditional)) then
    begin
      ShowBLenCkBx.Enabled := true;
      BLenHideCkBx.Enabled := true;
      edtBLenCutoff.Enabled := true;
      BLenPositionCmbBx.Enabled := true;
      BLenDecimalsSEdit.Enabled := true;
      BLenPlaceLabel.Enabled := true;
      BLenDecimalsLabel.Enabled := true;
      Label3.Enabled := true;
      BLenPositionCmbBx.Color := clWindow;

      if Tree.ShowBLen then
        ShowBLenCkBx.Checked := true
      else
        ShowBLenCkBx.Checked := false;
      if Tree.BLenCutoff > Tree.MinBranchLength then
      begin
        BLenHideCkBx.Checked := true;
        edtBLenCutoff.Value := Tree.BLenCutoff;
      end
      else
        BLenHideCkBx.Checked := false;
      if Tree.MinBranchLength < 0.0 then
        edtBLenCutoff.MinValue := Tree.MinBranchLength
      else
        edtBLenCutoff.MinValue := 0.0;
      edtBLenCutoff.MaxValue := Tree.MaxBranchLength;
      BLenDecimalsSEdit.Value :=  Tree.BLenDecimals;
      BLenPositionCmbBx.ItemIndex := integer(Tree.BLenPosition);
    end
    else
    begin
      ShowBLenCkBx.Enabled := false;
      BLenHideCkBx.Enabled := false;
      edtBLenCutoff.Enabled := false;
      BLenPositionCmbBx.Enabled := false;
      BLenDecimalsSEdit.Enabled := false;
      BLenPlaceLabel.Enabled := false;
      BLenDecimalsLabel.Enabled := false;
      Label3.Enabled := false;
      BLenPositionCmbBx.Color := clBtnFace;
    end;

    if Tree.ShowOTUName then
      ShowTaxonNameCkBx.Checked := true
    else
      ShowTaxonNameCkBx.Checked := false;
    if Tree.ShowOTUMarker then
      ShowTaxonMarkerCkBx.Checked := true
    else
      ShowTaxonMarkerCkBx.Checked := false;

    if Tree.isScale then
    begin
      ScaleTabSheet.Enabled := true;
      ShowScaleCkBx.Enabled := true;
      //ScaleWidthLabel.Enabled := true;
      //ScaleUnitLabel.Enabled := true;
      //ScaleLengthLabel.Enabled := true;
      //ScaleThickLabel.Enabled := true;
      ShowScaleCkBx.Enabled := true;
      UnitEdit.Enabled := true;
      edtScaleLength.Enabled := true;
      ScaleWidthCmbBx.Color := clWindow;

      case Tree.ScalePen.Width of
        0 : ScaleWidthCmbBx.ItemIndex := 0;
        1 : ScaleWidthCmbBx.ItemIndex := 1;
        2 : ScaleWidthCmbBx.ItemIndex := 2;
        3 : ScaleWidthCmbBx.ItemIndex := 3;
        4 : ScaleWidthCmbBx.ItemIndex := 4;
        5 : ScaleWidthCmbBx.ItemIndex := 5;
      else
        ScaleWidthCmbBx.ItemIndex := -1;
      end;

      ShowScaleCkBx.Checked := Tree.ShowScale;
      UnitEdit.Text := Tree.ScaleUnit;
      if Tree.TreeStyle = tsCircle then
        edtScaleLength.MaxValue := Tree.LongestPath*1.1
      else
        edtScaleLength.MaxValue := Tree.LongestPath;
      edtScaleLength.MinValue := power(10, floor(log10(edtScaleLength.MaxValue/100)));

      edtScaleLength.Value := StrToFloat(Tree.ScaleText);
      if edtScaleLength.Value > 0 then
        if Pos(FormatSettings.DecimalSeparator, Tree.ScaleText) = 0 then
          edtScaleLength.Increment := 1
        else
          edtScaleLength.Increment := Power(10, Pos(FormatSettings.DecimalSeparator, Tree.ScaleText)-Length(Tree.ScaleText));

      if Tree.IsLinearized or ToggleTimeScaleAction.Enabled then
      begin
        edtTick.Enabled := true;
        ScaleLengthLabel.Caption := 'Major tick interval';
        ScaleLengthLabel.Left := 18;
        ScaleThickLabel.Caption := 'Minor tick interval';
        ScaleThickLabel.Left := 18;

        edtTick.MaxValue := edtScaleLength.Value;
        edtTick.MinValue := edtScaleLength.MinValue;
        if Tree.ScaleTick < 0.0000000000001 then
          edtTick.Value := edtScaleLength.Value
        else
          edtTick.Value := Tree.ScaleTick;
        edtTick.Increment := edtScaleLength.Increment;

        ShowHeightErrBarCkBx.Enabled := AOwner.ActionDisplayErrorBars.Enabled and AOwner.ActionDisplayErrorBars.Visible;
        ShowHeightErrBarCkBx.Checked := Tree.ShowHeightErrBar;
      end
      else
      begin
        edtTick.Enabled := false;
        ScaleLengthLabel.Caption := 'Scale length';
        ScaleLengthLabel.Left := 42;
        ScaleThickLabel.Caption := 'Tick interval';
        ScaleThickLabel.Left := 42;

        ShowHeightErrBarCkBx.Enabled := false;
      end;

      if Tree.IsTimeScale and (Tree.IsLinearized or ToggleTimeScaleAction.Enabled) then
      begin
        ShowTimeScaleCkBx.Enabled := true;
        TimeUnitEdit.Enabled := true;
        edtTimeLength.Enabled := true;
        edtTimeTick.Enabled := true;

        ShowTimeScaleCkBx.Enabled := true;
        //TimeScaleUnitLabel.Enabled := true;
        //TimeScaleLengthLabel.Enabled := true;
        //TimeScaleTickLabel.Enabled := true;
        TimeUnitEdit.Color := clWindow;

        ShowTimeScaleCkBx.Checked := Tree.ShowTimeScale;
        TimeUnitEdit.Text := Tree.TimeUnit;
        edtTimeLength.MaxValue := 10*StrToFloat(Tree.TimeText);
        edtTimeLength.MinValue := power(10, floor(log10(edtTimeLength.MaxValue/100)));
        edtTimeLength.Value := StrToFloat(Tree.TimeText);
        edtTimeTick.MaxValue := edtTimeLength.Value;
        edtTimeTick.MinValue := edtTimeLength.MinValue;
        if Tree.TimeTick < 0.000000000001 then
          edtTimeTick.Value := edtTimeLength.Value
        else
          edtTimeTick.Value := Tree.TimeTick;
        edtTimeTick.Increment := edtTimeLength.Increment;
      end
      else begin
        ShowTimeScaleCkBx.Enabled := false;
        TimeUnitEdit.Enabled := false;
        edtTimeLength.Enabled := false;
        edtTimeTick.Enabled := false;

        ShowTimeScaleCkBx.Enabled := false;
        //TimeScaleUnitLabel.Enabled := false;
        //TimeScaleLengthLabel.Enabled := false;
        //TimeScaleTickLabel.Enabled := false;
        TimeUnitEdit.Color := clBtnFace;
      end;
    end
    else begin
      ScaleTabSheet.Enabled := false;
      ShowScaleCkBx.Enabled := false;

      //ScaleWidthLabel.Enabled := false;
      //ScaleUnitLabel.Enabled := false;
      ScaleLengthLabel.Enabled := false;
      ScaleThickLabel.Enabled := false;
      ShowScaleCkBx.Enabled := false;
      UnitEdit.Enabled := false;
      edtScaleLength.Enabled := false;
      edtTick.Enabled := false;
      ScaleWidthCmbBx.Color := clBtnFace;
      UnitEdit.Color := clBtnFace;

      ShowTimeScaleCkBx.Enabled := false;
      TimeUnitEdit.Enabled := false;
      edtTimeLength.Enabled := false;
      edtTimeTick.Enabled := false;

      ShowTimeScaleCkBx.Enabled := false;
      //TimeScaleUnitLabel.Enabled := false;
      //TimeScaleLengthLabel.Enabled := false;
      //TimeScaleTickLabel.Enabled := false;
      TimeUnitEdit.Color := clBtnFace;
    end;

    if Tree.ShowTopologyOnly  or (not Tree.isBranchLength) then
    begin
      HorzSEdit1.Enabled := false;
      HorzSEdit1.Text := '';
      BLenUnitLabel.Caption := 'Pixels / ';
    end
    else begin
      HorzSEdit1.Enabled := true;

      HorzRatio := Tree.TreeWidth/Tree.PixelsPerUnit;
      HorzFactor := Power(10, 2-Floor(Log10(Tree.PixelsPerUnit)));
      HorzSEdit1.MaxValue := Floor(8190*HorzFactor/HorzRatio);
      HorzSEdit1.Value := Round(Tree.PixelsPerUnit*HorzFactor);
      if HorzFactor >= 1 then
        BLenUnitLabel.Caption := 'Pixels / ' +FloatToStrF(HorzFactor, ffFixed, 15, 0)
      else if HorzFactor > 0 then
        BLenUnitLabel.Caption := 'Pixels / ' +FloatToStrF(HorzFactor, ffFixed, 15, -Floor(Log10(HorzFactor)));
    end;
    WidthSEdit.Value := Tree.TreeWidth;
    VertSEdit.Value := Tree.PixelsPerOTU;

    RadiusSEdit.Value := Tree.Radius;
    AngleSEdit.Value := Tree.StartAngle;

    HorzSEdit2.MaxValue := HorzSEdit1.MaxValue;
    HorzSEdit2.Value := HorzSEdit1.Value;
    AngleSEdit2.Value := AngleSEdit.Value;
    BLenUnitLabel2.Caption := BLenUnitLabel.Caption;

    HolzNameCBx1.Checked := Tree.HorzTaxonName;
    HolzNameCBx2.Checked := Tree.HorzTaxonName;
  end;
end;

procedure TFormatDlg.FocusInvalidValue(SpinEdit: TSpinEdit);
begin
  PageControl.ActivePage := FindPageForSpinEdit(SpinEdit);
  ShowMessage(SpinEdit.Text + ' is not a valid numeric value');
  SpinEdit.SelectAll;
  SpinEdit.SetFocus;
end;

procedure TFormatDlg.FocusInvalidFloatValue(SpinEdit: TFloatSpinEdit);
begin
  PageControl.ActivePage := FindPageForFloatSpinEdit(SpinEdit);
  ShowMessage(SpinEdit.Text + ' is not a valid numeric value');
  SpinEdit.SelectAll;
  SpinEdit.SetFocus;
end;

function TFormatDlg.FindPageForSpinEdit(SpinEdit: TSpinEdit): TTabSheet;
begin
  if (SpinEdit = edtScaleLength) or
     (SpinEdit = edtTick) or
     (SpinEdit = edtTimeLength) or
     (SpinEdit = edtTimeTick) then
    Result := ScaleTabSheet
  else if (SpinEdit = BLenDecimalsSEdit) or
          (SpinEdit = DivTimeDecimalsSEdit) or
          (SpinEdit = TimesHMarginSpinEdit) or
          (SpinEdit = TimesVMarginSpinEdit) or
          (SpinEdit = StatsHMarginSEdit) or
          (SpinEdit = StatsVMarginSEdit) or
          (SpinEdit = StatsCutoffSEdit) then
    Result := BranchTabSheet
  else if (SpinEdit = VertSEdit) or
          (SpinEdit = HorzSEdit1) or
          (SpinEdit = AngleSEdit) or
          (SpinEdit = RadiusSEdit) or
          (SpinEdit = MarginSEdit) or
          (SpinEdit = HorzSEdit2) or
          (SpinEdit = AngleSEdit2) or
          (SpinEdit = WidthSEdit) then
    Result := TreeTabSheet
  else if (SpinEdit = ConsensusSEdit) or
          (SpinEdit = CondensedSEdit) then
    Result := CutoffTabSheet
  else
  begin
    Result := nil;
    Assert(False, 'Invalid SpinEdit');
  end;
end;

function TFormatDlg.FindPageForFloatSpinEdit(SpinEdit: TFloatSpinEdit): TTabSheet;
begin
  Result := BranchTabSheet;
  if (SpinEdit = edtBLenCutoff) then
    Result := BranchTabSheet;
end;

function TFormatDlg.ValidateSpinEdits: Boolean;
var
  f: Double;
begin
  Result := False;
  if not TryStrToFloat(edtScaleLength.Text, f) then
  begin
    FocusInvalidValue(edtScaleLength);
    Exit;
  end;

  if not TryStrToFloat(edtTick.Text, f) then
  begin
    FocusInvalidValue(edtTick);
    Exit;
  end;

  if not TryStrToFloat(edtTimeLength.Text, f) then
  begin
    FocusInvalidValue(edtTimeLength);
    Exit;
  end;

  if not TryStrToFloat(edtTimeTick.Text, f) then
  begin
    FocusInvalidValue(edtTimeTick);
    Exit;
  end;

  if not TryStrToFloat(edtBLenCutoff.Text, f) then
  begin
    FocusInvalidFloatValue(edtBLenCutoff);
    Exit;
  end;

  if not TryStrToFloat(BLenDecimalsSEdit.Text, f) then
  begin
    FocusInvalidValue(BLenDecimalsSEdit);
    Exit;
  end;

  if not TryStrToFloat(DivTimeDecimalsSEdit.Text, f) then
  begin
    FocusInvalidValue(DivTimeDecimalsSEdit);
    Exit;
  end;

  if not TryStrToFloat(StatsCutoffSEdit.Text, f) then
  begin
    FocusInvalidValue(StatsCutoffSEdit);
    Exit;
  end;

  if not TryStrToFloat(StatsVMarginSEdit.Text, f) then
  begin
    FocusInvalidValue(StatsVMarginSEdit);
    Exit;
  end;

  if not TryStrToFloat(StatsHMarginSEdit.Text, f) then
  begin
    FocusInvalidValue(StatsHMarginSEdit);
    Exit;
  end;

  if not TryStrToFloat(TimesVMarginSpinEdit.Text, f) then
  begin
    FocusInvalidValue(TimesVMarginSpinEdit);
    Exit;
  end;

  if not TryStrToFloat(TimesHMarginSpinEdit.Text, f) then
  begin
    FocusInvalidValue(TimesHMarginSpinEdit);
    Exit;
  end;

  if not TryStrToFloat(WidthSEdit.Text, f) then
  begin
    FocusInvalidValue(WidthSEdit);
    Exit;
  end;

  if not TryStrToFloat(AngleSEdit2.Text, f) then
  begin
    FocusInvalidValue(AngleSEdit2);
    Exit;
  end;

  if not TryStrToFloat(HorzSEdit2.Text, f) then
  begin
    FocusInvalidValue(HorzSEdit2);
    Exit;
  end;

  if not TryStrToFloat(MarginSEdit.Text, f) then
  begin
    FocusInvalidValue(MarginSEdit);
    Exit;
  end;

  if not TryStrToFloat(RadiusSEdit.Text, f) then
  begin
    FocusInvalidValue(RadiusSEdit);
    Exit;
  end;

  if not TryStrToFloat(AngleSEdit.Text, f) then
  begin
    FocusInvalidValue(AngleSEdit);
    Exit;
  end;

  if not TryStrToFloat(HorzSEdit1.Text, f) then
  begin
    FocusInvalidValue(HorzSEdit1);
    Exit;
  end;

  if not TryStrToFloat(VertSEdit.Text, f) then
  begin
    FocusInvalidValue(VertSEdit);
    Exit;
  end;

  if not TryStrToFloat(ConsensusSEdit.Text, f) then
  begin
    FocusInvalidValue(ConsensusSEdit);
    Exit;
  end;

  if not TryStrToFloat(CondensedSEdit.Text, f) then
  begin
    FocusInvalidValue(CondensedSEdit);
    Exit;
  end;
  Result := True;
end;

procedure TFormatDlg.BranchWidthCmbBxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  point : array[0..3] of TPoint;
  textsize : integer;
begin
  with BranchWidthCmbBx do begin
    Canvas.FillRect(aRect);
    Canvas.Pen.Color := Canvas.Font.Color;
    Canvas.Brush.Color := Canvas.Font.Color;
    Canvas.Brush.Style := bsSolid;
    textsize := Canvas.TextWidth(Items[Index]);
    point[0].x := aRect.left+4;
    point[1].x := aRect.right -textsize -8;
    point[2].x := aRect.right -textsize -8;
    point[3].x := aRect.left+4;
    case Index of
      0,1 : begin
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              Canvas.Polyline(Slice(point,2));
            end;
      2   : begin
              point[0].y := (aRect.top+aRect.bottom) div 2;
              point[1].y := (aRect.top+aRect.bottom) div 2;
              point[2].y := (aRect.top+aRect.bottom) div 2 +1;
              point[3].y := (aRect.top+aRect.bottom) div 2 +1;
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
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(aRect.right -textsize -2, aRect.top, Items[Index]);
  end;
end;

procedure TFormatDlg.ActionTaxonFontExecute(Sender: TObject);
begin
  (Owner as TTreeViewForm).ActionTaxonFont.Execute;
end;

procedure TFormatDlg.ActionTimesFontExecute(Sender: TObject);
begin
  (Owner as TTreeViewForm).ActionTimesFont.Execute;
end;

procedure TFormatDlg.ActionGroupFontExecute(Sender: TObject);
begin

end;

procedure TFormatDlg.ActionBranchInfoFontExecute(Sender: TObject);
begin
  (Owner as TTreeViewForm).ActionBranchInfoFont.Execute;
end;

procedure TFormatDlg.ActionBLensFontExecute(Sender: TObject);
begin
  (Owner as TTreeViewForm).ActionBLenFont.Execute;
end;

procedure TFormatDlg.ActionScaleBarFontExecute(Sender: TObject);
begin
  (Owner as TTreeViewForm).ActionScaleBarFont.Execute;
end;

end.

