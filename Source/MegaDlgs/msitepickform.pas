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

unit msitepickform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Grids, StdCtrls, Buttons, Spin, MegaConsts, MDomainInfo,
  MOtuInfo, mimageform;

type

  { TSitePickForm }

  TSitePickForm = class(TForm)
    ButtonImages: TImageList;
    CancelBtn: TImage;
    OkBtn: TImage;
    SiteNumSE: TSpinEdit;
    UndefinedChkBx: TCheckBox;
    SeqGrid: TDrawGrid;
    Panel1: TPanel;
    StatusBar: TStatusBar;
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InitImages;
    procedure OkBtnClick(Sender: TObject);
    procedure OkBtnMouseEnter(Sender: TObject);
    procedure OkBtnMouseLeave(Sender: TObject);
    procedure SeqGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SiteNumSEChange(Sender: TObject);
  private
    //DispChar: array [0..1] of Char;
  public
    FNoOfTaxa:     LongInt;
    FNoOfSites:    LongInt;
    FOtuInfos:     TAllOtuInfo;
    FDomainMarks:  TAllSiteDomainMark;
    IsPickingStart: Boolean; // set to true for start site and false for others
    CurFocusRow:   LongInt;
    CurFocusCol:   LongInt;
    CurFocusInfo:  TDomainInfo;
    CurOtuInfo:    TOtuInfo;
    CurDomainInfo: TDomainInfo;
    procedure Initialize;
    procedure GoToSite(Value: Integer);
    function  GetSitePicked: Integer;  // -1 if undefined; or 0 - count
  end;

var
  SitePickForm: TSitePickForm;

implementation

uses
  MEditorForm, MD_InputSeqData;

{$R *.lfm}

{ TSitePickForm }

procedure TSitePickForm.OkBtnClick(Sender: TObject);
var
  i: Integer;
begin
  if UndefinedChkBx.Checked then
    Exit;

  ModalResult := mrOK; // by default
  if (FDomainMarks[SeqGrid.Col-1] = nil) then // a pickable site
  begin
    if IsPickingStart then
    begin
      if CurDomainInfo.ToSite < 0 then // no problem, undefined
        Exit;
      // otherwise check
      for i:=SeqGrid.Col-1 to CurDomainInfo.ToSite do
        if (FDomainMarks[i] <> nil) and (FDomainMarks[i] <> CurDomainInfo) and (FDomainMarks[i].ParentDomain <> CurDomainInfo) then
        begin
           ShowMessage('Current site selection leads to a Noncontinuous domain. Please choose another site.');
           ModalResult := mrNone;
           Exit;
        end;
    end
    else // picking
    begin
      if CurDomainInfo.FromSite < 0 then // no problem, undefined
        Exit;
      // otherwise check
      for i:= CurDomainInfo.FromSite  to SeqGrid.Col-1 do
        if (FDomainMarks[i] <> nil) and (FDomainMarks[i] <> CurDomainInfo) and (FDomainMarks[i].ParentDomain <> CurDomainInfo) then
        begin
           ShowMessage('Current site selection leads to a Noncontinuous domain. Please choose another site.');
           ModalResult := mrNone;
           Exit;
        end;
    end;
  end
  else if (FDomainMarks[SeqGrid.Col-1] <> CurDomainInfo) and (FDomainMarks[SeqGrid.Col-1] <> CurDomainInfo.ParentDomain) then
  begin
    ModalResult := mrNone;
    ShowMessage('Selected site is used by another domain.  Please choose another site.');
  end;
end;

procedure TSitePickForm.OkBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(1, OkBtn.Picture.Bitmap);
end;

procedure TSitePickForm.OkBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(0, OkBtn.Picture.Bitmap);
end;

procedure TSitePickForm.FormActivate(Sender: TObject);
begin
  if SeqGrid.Col = 0 then
    SeqGrid.Col := 1;
  CurFocusInfo := FDomainMarks[SeqGrid.Col-1];
  if CurFocusInfo = nil then
    StatusBar.Panels[1].Text := 'Independent'
  else if Length(CurFocusInfo.GeneName) > 0 then
    StatusBar.Panels[1].Text := CurFocusInfo.Name+' ('+CurFocusInfo.GeneName+')'
  else
    StatusBar.Panels[1].Text := CurFocusInfo.Name;
end;

procedure TSitePickForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSitePickForm.CancelBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TSitePickForm.CancelBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(2, CancelBtn.Picture.Bitmap);
end;

procedure TSitePickForm.FormCreate(Sender: TObject);
begin
  FNoOfTaxa := -1;
  FNoOfSites:= -1;
  FOtuInfos := nil;
  FDomainMarks := D_InputSeqData.FDomainMarks;
  CurFocusRow  := -1;
  CurFocusCol  := -1;
  CurOtuInfo    := nil;
  CurDomainInfo := nil;
  IsPickingStart := True;
  InitImages;
  ImageForm.UpdateImgList(Self);
end;

procedure TSitePickForm.InitImages;
begin
  {$IFDEF DARWIN}
  CancelBtn.Proportional:=True;
  OkBtn.Proportional:=True;
  {$ENDIF}
end;

procedure TSitePickForm.SeqGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  PrevBrushColor, PrevPenColor, PrevFontColor : TColor;
  DispChar: Char;
  AStr: String;
  X, Y: Integer;
  ts: TTextStyle;
begin
  if not Visible then
    Exit;
  with SeqGrid, Canvas do
  begin
    if (ACol > FNoOfSites) or (ARow >= FNoOfTaxa) then
      Exit;
    ts := Canvas.TextStyle;

    PrevBrushColor := Brush.Color;
    PrevPenColor   := Pen.Color;
    PrevFontColor  := Font.Color;
    if (ACol = 0) then // fixed-type column for taxa names
    begin
      DispChar :=  ' ';
      FillRect(aRect);

      X := aRect.Left + 2;               // write the check box
      Y := aRect.Top + 2;
      ts.Alignment := taLeftJustify;
      AStr := FOtuInfos[ARow].Name;
      if Length(FOtuInfos[ARow].GpName) > 0 then
        AStr := AStr + ' {'+FOtuInfos[ARow].GpName+'}';
      TextRect(aRect, X, Y, AStr, ts);
      if (aCol <> 0) and (ARow <> CurFocusRow) then
       StatusBar.Panels[0].Text := AStr;
    end
    else
    begin
      // each site is displayed as such
      DispChar := PAnsiChar(FOtuInfos[ARow].Data)[ACol-1];

      Brush.Color := clWhite;
      Font.Color  := clBlack;

      if FDomainMarks[ACol-1] <> nil then // by default raise a red color
      begin
        if CurDomainInfo.IsSibling(FDomainMarks[ACol-1]) then
        begin
          Brush.Color := clRed;
          Font.Color := clGray;
        end
        else if CurDomainInfo.IsParent(FDomainMarks[ACol-1]) then
        begin
          Brush.Color := RGBToColor($ff, $a5, $00);
          Font.Color := clWhite;
        end
        else
        begin
          Brush.Color := clRed;
          Font.Color  := clWhite;
        end;
      end;

      if (CurDomainInfo <> nil) and (FDomainMarks[ACol-1] = CurDomainInfo) then
      begin
        Brush.Color := clYellow;
        Font.Color  := clBlack;
      end;

      if gdSelected in aState then
      begin
        Brush.Color := clNavy;
        Font.Color  := clWhite;
      end;

      FillRect(aRect);
      ts.Alignment := taCenter;
      with aRect do
        TextRect(aRect, 0, Top+2, DispChar, ts);

      // write the coding frame
      if (FDomainMarks[ACol-1] <> nil) then
      begin
        with aRect do
          if (FDomainMarks[ACol-1] <> nil) then
          begin
            if meg1stBase in FDomainMarks[ACol-1].CodonSiteAttr[ACol-1] then
            begin
              Pen.Color   := clGray;
              PolyLine([Point(Right, Bottom-2), Point(Left+2,  Bottom-2), Point(Left+2,  Top+2),  Point(Right, Top+2)]);
            end
            else if meg2ndBase in FDomainMarks[ACol-1].CodonSiteAttr[ACol-1] then
            begin
              Pen.Color   := clGray;
              MoveTo(Left, Bottom-2); LineTo(Right, Bottom-2);
              MoveTo(Left, Top+2);    LineTo(Right, Top+2);
            end
            else if meg3rdBase in FDomainMarks[ACol-1].CodonSiteAttr[ACol-1] then
            begin
              Pen.Color   := clGray;
              PolyLine([Point(Left,    Bottom-2), Point(Right-2, Bottom-2), Point(Right-2, Top+2),    Point(Left,  Top+2)]);
            end;
         end;
      end;

      Brush.Color := PrevBrushColor;
      Pen.Color   := PrevPenColor;
      if gdFocused in aState then
        DrawFocusRect(aRect);

      if gdSelected in aState then
      begin
        if (ARow >= 0) and (ARow <> CurFocusRow) then
        begin
          CurFocusRow := ARow;
          AStr := FOtuInfos[ARow].Name;
          if Length(FOtuInfos[ARow].GpName) > 0 then
              AStr := AStr + ' {'+FOtuInfos[ARow].GpName+'}';
           StatusBar.Panels[0].Text := AStr;
        end;

        if ACol >= 0 then
        begin
          if ACol <> CurFocusCol then
            SiteNumSE.Value := ACol;
          if FDomainMarks[ACol-1] <> CurFocusInfo then
          begin
            CurFocusInfo := FDomainMarks[ACol-1];
            if CurFocusInfo  = nil then
              StatusBar.Panels[1].Text := 'Independent'
            else if  Length(CurFocusInfo.GeneName) > 0 then
              StatusBar.Panels[1].Text := CurFocusInfo.Name+' ('+CurFocusInfo.GeneName+')'
            else
              StatusBar.Panels[1].Text := CurFocusInfo.Name;
          end;
          CurFocusCol := ACol;
        end;
      end;
    end;
    Brush.Color := PrevBrushColor;
    Pen.Color   := PrevPenColor  ;
    Font.Color  := PrevFontColor ;
  end;
end;

procedure TSitePickForm.SiteNumSEChange(Sender: TObject);
begin
  if Sender <> SiteNumSE then Exit;

  if Length(SiteNumSE.Text) =0 then
    Exit;

  if SiteNumSE.Value > FNoOfSites then
  begin
    SiteNumSE.Value := FNoOfSites;
    Exit;
  end;

  SeqGrid.Col     := Trunc(SiteNumSE.Value);
  if SeqGrid.Col < SeqGrid.LeftCol then
    SeqGrid.LeftCol := SeqGrid.Col
  else if SeqGrid.Col > (SeqGrid.LeftCol+SeqGrid.VisibleColCount) then
    SeqGrid.LeftCol := SeqGrid.Col
end;

procedure TSitePickForm.Initialize;
begin
  with SeqGrid do
  begin
    DefaultColWidth   := Canvas.TextWidth('W')+4;
    DefaultRowHeight := Canvas.TextHeight(' ')+2;
    ColWidths[0]     := 10*DefaultColWidth;
    RowCount := 1;
    ColCount := FNoOfSites+1;
    FixedRows := 0;
    FixedCols := 0;
  end;
  SiteNumSE.MinValue := 1;
  SiteNumSE.MaxValue := FNoOfSites;
  SiteNumSE.Value := 1;
end;

procedure TSitePickForm.GoToSite(Value: Integer);
begin
  UndefinedChkBx.Checked := False;
  if Value < 0 then
  begin
    SeqGrid.LeftCol := 1;
    SeqGrid.Col     := 1;
  end
  else
  begin
    SeqGrid.LeftCol := Value+1;  // as the first column is names
    SeqGrid.Col     := Value+1;
  end;
  SiteNumSE.value := SeqGrid.Col;
end;

function TSitePickForm.GetSitePicked: Integer;
begin
  Result := -1;
  if  UndefinedChkBx.Checked then
    Result := -1
  else
    Result := SeqGrid.Col-1;
end;

end.

