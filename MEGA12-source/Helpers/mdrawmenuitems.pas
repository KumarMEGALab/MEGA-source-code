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

unit mdrawmenuitems;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Menus, ActnList, Graphics;

const
  MENUITEMBGCOLOR = $00f0f0f0;
  MENUITEMACTIVEBGCOLOR = clBtnFace;
  MENUITEMFONTCOLOR = $00333333;
  MENUITEMACTIVEFONTCOLOR = $003641c7;
  MAINMENUBGCOLOR = clWhite;
  MAINMENUFONTCOLOR = $00333333;
  MAINMENUACTIVEBGCOLOR = $003c9a3d;//$003641c7;
  MAINMENUACTIVEFONTCOLOR = clWhite;

type

  { TMenuRenderer }

  TMenuRenderer = class(TObject)
    private
      FCaptionTokens: TStringList;
      FFontSize: Integer;
      FTokenToUnderLine: Integer;
      procedure FindCaptionTokens(aCaption: String);
      procedure DrawRectEdges(ACanvas: TCanvas; ARect: TRect; DoLeft, DoTop, DoRight, DoBottom: Boolean);
    public
      constructor Create;
      destructor Destroy; override;
      procedure PopupMenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState; Checkmark: TBitmap);
      procedure MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState; Checkmark, MenuBitmap: TBitmap);
      procedure MeasureMenuItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer; const aIcon: TBitmap);
      property FontSize: Integer read FFontSize write FFontSize;
  end;

var
  MenuRenderer: TMenuRenderer;

implementation

uses
  mega_main, MegaUtils;

procedure TMenuRenderer.FindCaptionTokens(aCaption: String);
var
  ampersandPosition: Integer;
begin
  if FCaptionTokens.Count > 0 then
    FCaptionTokens.Clear;
  ampersandPosition := Pos('&', aCaption);
  if (ampersandPosition = 0) or (ampersandPosition = Length(aCaption)) then
  begin
    FCaptionTokens.Add(aCaption);
    FTokenToUnderLine := -1;
    Exit;
  end;

  if ampersandPosition = 1 then
  begin
    FTokenToUnderLine := 0;
    FCaptionTokens.Add(Copy(aCaption, 2, 1));
    FCaptionTokens.Add(Copy(aCaption, 3, Length(aCaption)));
    Exit;
  end;

  FCaptionTokens.Add(Copy(aCaption, 1, ampersandPosition - 1));
  FTokenToUnderLine := 1;
  FCaptionTokens.Add(Copy(aCaption, ampersandPosition + 1, 1));
  FCaptionTokens.Add(Copy(aCaption, ampersandPosition + 2, Length(aCaption)));
end;

procedure TMenuRenderer.DrawRectEdges(ACanvas: TCanvas; ARect: TRect; DoLeft,DoTop, DoRight, DoBottom: Boolean);
begin
  if DoLeft then
    ACanvas.Line(aRect.Left, aRect.Top, aRect.Left, aRect.Bottom);
  if DoTop then
    ACanvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
  if DoRight then
    ACanvas.Line(aRect.Right - 1, aRect.Top, aRect.Right - 1, aRect.Bottom);
  if DoBottom then
    ACanvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
end;

constructor TMenuRenderer.Create;
begin
  FCaptionTokens := TStringList.Create;
  FFontSize := 10;
end;

destructor TMenuRenderer.Destroy;
begin
  if Assigned(FCaptionTokens) then
    FCaptionTokens.Free;
  inherited Destroy;
end;

procedure TMenuRenderer.PopupMenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState; Checkmark: TBitmap);
var
  item: TMenuItem;
  x, y: Integer;
  aText: String;
  isTopItem: Boolean;
begin
  if (not Sender.ClassNameIs('TMenuItem')) then
    Exit;
  item := TMenuItem(Sender);
  if not Item.Visible then Exit;
  isTopItem := Item.IsInMenuBar;
  x := aRect.Left + 18;
  y := aRect.Top + 2;
  with ACanvas do
  begin
    Font.Size := FFontSize;
    if (odSelected in AState) or (odFocused in AState) then
    begin
      if isTopItem then
      begin
        Brush.Color := MainMenuActiveBgColor;
        Font.Color := MainMenuActiveFontColor;
      end
      else
      begin
        Brush.Color := MenuItemActiveBgColor;
        Font.Color := MenuItemActiveFontColor;
      end;
    end
    else if (odGrayed in AState) or (odDisabled in AState) then
    begin
      Brush.Color := MenuItemBgColor;
      Font.Color := clGrayText;
    end
    else if odHotLight in aState then
    begin
      Brush.Color := MainMenuActiveBgColor;
      Font.Color := MainMenuActiveFontColor;
    end
    else
    begin
      Brush.Color := MenuItemBgColor;
      Font.Color := MenuItemFontColor;
    end;
    FillRect(ARect);
    if item.Checked then
      Draw(aRect.Left + 2, aRect.Top + Round((aRect.Bottom - aRect.Top - Checkmark.Height)/2), Checkmark);
    if Trim(item.Caption) <> EmptyStr then
      aText := Trim(item.Caption)
    else if Assigned(item.Action) and (item.Action is TCustomAction) then
      with item.Action as TCustomAction do
        aText := Caption;
    if aText = '-' then
    begin
      Pen.Color := clLtGray;
      y := aRect.Top + Round((aRect.Bottom - aRect.Top) / 2);
      Line(aRect.Left + 2, y, aRect.Right - 2, y);
    end
    else
      TextOut(x, y, aText);
  end;
end;

procedure TMenuRenderer.MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState; Checkmark, MenuBitmap: TBitmap);
var
  aText: String;
  ts: TTextStyle;
  x, y: Integer;
  aItem: TMenuItem = nil;
  isTopItem: Boolean = False;
  i: Integer;
begin
  if Sender is TMenuItem then
  begin
    FCaptionTokens.Clear;
    aItem := TMenuItem(Sender);
    if not aItem.Visible then
      Exit;
    isTopItem := aItem.IsInMenuBar;
    aText := Trim(aItem.Caption);
    if odNoAccel in AState then
    begin
      aText := StringReplace(aText, '&', EmptyStr, []);
      FTokenToUnderLine := -1;
      FCaptionTokens.Add(aText);
    end
    else
      FindCaptionTokens(aText);
    ts := ACanvas.TextStyle;
    ts.Alignment := taLeftJustify;
    ts.Layout := tlCenter;
    if isTopItem then
      x := aRect.Left + Round((RectWidth(aRect) - ACanvas.TextWidth(StringReplace(aText, '&', EmptyStr, [])))/2)
    else
      x := aRect.Left + MenuBitmap.Width + 2*MENU_ITEM_MARGIN;
    y := aRect.Top + 2;
    with ACanvas do
    begin
      Font.Size := FFontSize;
      if (odSelected in AState) or (odFocused in AState) then
      begin
        if isTopItem then
        begin
          Brush.Color := MainMenuActiveBgColor;
          Font.Color := MainMenuActiveFontColor;
        end
        else
        begin
          Brush.Color := MenuItemActiveBgColor;
          Font.Color := MenuItemActiveFontColor;
        end;
      end
      else if (odGrayed in AState) or (odDisabled in AState) then
      begin
        Brush.Color := MenuItemBgColor;
        Font.Color := clGrayText;
      end
      else if odHotLight in aState then
      begin
        Brush.Color := MainMenuActiveBgColor;
        Font.Color := MainMenuActiveFontColor;
      end
      else
      begin
        Brush.Color := MenuItemBgColor;
        Font.Color := MenuItemFontColor;
      end;

      Brush.Style := bsSolid;
      if Brush.Color = MainMenuActiveBgColor then
      begin
        GradientFill(ARect, Brush.Color, $004d6610, gdVertical);
        Pen.Color := clGrayText;
        DrawRectEdges(ACanvas, aRect, True, True, True, True);
      end
      else
        FillRect(ARect);
      Brush.Style := bsClear;
      if aText = '-' then
      begin
        Pen.Color := clLtGray;
        Pen.Style := psSolid;
        Brush.Style := bsClear;
        y := aRect.Top + Round((aRect.Bottom - aRect.Top)/2);
        Line(aRect.Left, y, aRect.Right, y);
      end
      else
      begin
        if FCaptionTokens.Count > 0 then
        begin
          for i := 0 to FCaptionTokens.Count - 1 do
          begin
            if i = FTokenToUnderLine then
              Font.Style := [fsUnderline]
            else
              Font.Style := [];
            TextRect(aRect, x, y, FCaptionTokens[i], ts);
            x := x + TextWidth(FCaptionTokens[i]);
          end;
        end;
      end;
      if aItem.Checked then
        Draw(aRect.Left + 4, aRect.Top + (aRect.Bottom - aRect.Top - Checkmark.Height) div 2, Checkmark)
      else if (aItem.ImageIndex >= 0) and (aItem.ImageIndex < MegaForm.NewIcons.Count) then
      begin
        if (odGrayed in AState) or (odDisabled in AState) then
          MegaForm.DisabledIcons.GetBitmap(aItem.ImageIndex, MenuBitmap)
        else
          MegaForm.NewIcons.GetBitmap(aItem.ImageIndex, MenuBitmap);
        Draw(aRect.Left + 4, aRect.Top + (aRect.Bottom - aRect.Top - MenuBitmap.Height) div 2, MenuBitmap);
      end;
    end;
  end;
end;

procedure TMenuRenderer.MeasureMenuItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer; const aIcon: TBitmap);
const
  MARGIN = 8;
  PADDING = 4;
var
  aText: String;
  aItem: TMenuItem = nil;
  isTopItem: Boolean = False;
begin
  if not (Sender is TMenuItem) then
    exit;
  aItem := TMenuItem(Sender);
  isTopItem := aItem.IsInMenuBar;
  with ACanvas do
  begin
    Font.Size := FFontSize;
    aText := aItem.Caption;
    if Trim(aText) = EmptyStr then
      aText := 'WW';
    aWidth := aCanvas.TextWidth(aText) + MARGIN;
    if not IsTopItem then
      aWidth := aWidth + aIcon.Width;
    if aText = '-' then
      aHeight := MARGIN
    else
      aHeight := aCanvas.TextHeight(aText) + PADDING;
  end;
  if aItem.Count > 0 then
    aWidth := aWidth + aIcon.Width;
end;

end.

