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

unit mimageform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  ExtCtrls, Buttons;

const
  COLLAPSED_ARROW_INDEX = 1;
  EXPANDED_ARROW_INDEX = 0;
  RIGHT_ARROW_INDEX = 1;
  LEFT_ARROW_INDEX = 2;

  CANCEL_BTN = 0;
  COMPUTE_BTN = 1;
  DOWNLOAD_BTN = 2;
  NO_BTN = 3;
  OK_BTN = 4;
  SAVE_SETTINGS_BTN = 5;
  YES_BTN = 6;
  NEXT_BTN = 7;
  HELP_BTN = 8;

type

  { TImageForm }

  TImageForm = class(TForm)
    DisabledIconsLarge: TImageList;
    DlgButtons: TImageList;
    DlgButtons2: TImageList;
    DlgButtonsHover: TImageList;
    DlgButtonsHover2: TImageList;
    Buttons_90x30: TImageList;
    Buttons_90x30_hover: TImageList;
    Buttons_180x60_hover: TImageList;
    Buttons_180x60: TImageList;
    HelpIconsSmall: TImageList;
    HelpIconsLarge: TImageList;
    Logo_160: TImageList;
    Logo_80: TImageList;
    SmallArrowIcons2: TImageList;
    DefaultIcons: TImageList;
    ImagesSmall: TImageList;
    ImagesLarge: TImageList;
    OrigDisabledIcons: TImageList;
    DisabledIcons128: TImageList;
    DisabledIcons: TImageList;
    DisabledIcons36: TImageList;
    Panel1: TPanel;
    SmallArrowIcons: TImageList;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure UpdateToolbar(tbar: TToolBar);
    procedure UpdateImgList(aForm: TForm);
    function GetSmallArrowIcons: TImageList;
    procedure GetDialogButtonImage_115px(index: Integer; image: TCustomBitmap);
    function GetDialogButtonImageList: TImageList;
    function GetDailogButtonHoverImageList: TImageList;
    function GetDialogButton2ImageList: TImageList;
    function GetDailogButton2HoverImageList: TImageList;
    function GetLogoImageList: TImageList;
    function GetHelpIconImageList: TImageList;
  end;


var
  ImageForm: TImageForm;

implementation

uses
  {$IFDEF VISUAL_BUILD}
  mega_main,
  {$ENDIF}
  ActnList, math, MegaConsts, MegaUtils;

{$R *.lfm}

{ TImageForm }

procedure TImageForm.FormCreate(Sender: TObject);
begin

end;

procedure TImageForm.UpdateToolbar(tbar: TToolBar);
var
  i: Integer = 0;
  aToolbar: TToolbar = nil;
begin
  {$IFDEF VISUAL_BUILD}
  if (tbar.Images <> nil) then
  begin
    tbar.Images := DefaultIcons;
    if MegaForm.UseLargeIconSize then
    begin
      tbar.DisabledImages := DisabledIconsLarge;
      tbar.ImagesWidth := 32;
    end
    else
    begin
      tbar.DisabledImages := DisabledIcons;
      tbar.ImagesWidth := 18;
    end;
  end;
  {$ENDIF}
end;

procedure TImageForm.UpdateImgList(aForm: TForm);
var
  i: Integer;
  aToolbar: TToolbar = nil;
begin
  {$IFDEF VISUAL_BUILD}
  for i:=0 to aForm.ComponentCount-1 do
  begin
    if (aForm <> MegaForm) and (aForm.Components[i].ClassName='TToolBar') then
    begin
      if aForm.ClassNameIs('TAnalysisPrefDlg') then
        continue;
      aToolBar := TToolBar(aForm.Components[i]);
      if (aToolbar.Images <> nil) then
      begin
        if (aToolbar.Name='TrayToolbar2') or
           (aToolbar.Name='PinupsToolbar') or
           ((aForm.ClassNameIs('TTreeInputForm')) and (aToolbar.Name = 'ToolBar1')) or
           ((aForm.ClassNameIs('TSpeciesMapDlg')) and (aToolbar.Name = 'ToolBar2')) then
          continue
        else
        begin
          aToolbar.Images := DefaultIcons;
          if MegaForm.UseLargeIconSize then
          begin
            aToolbar.DisabledImages := DisabledIconsLarge;
            aToolbar.ImagesWidth := 32;
          end
          else
          begin
            aToolbar.DisabledImages := DisabledIcons;
            aToolbar.ImagesWidth := 18;
          end;
        end;
      end;
    end;

    if aForm.Components[i].ClassName='TMainMenu' then
    begin
      if (TMainMenu(aForm.Components[i]).Images <> nil) then
        TMainMenu(aForm.Components[i]).Images := DefaultIcons;
    end;

    if aForm.Components[i].ClassName='TPopupMenu' then
    begin
      if (TPopupMenu(aForm.Components[i]).Images<>nil) then
        TPopupMenu(aForm.Components[i]).Images := DefaultIcons;
    end;

    if aForm.Components[i].ClassName = 'TActionList' then
    begin
      if TActionList(aForm.Components[i]).Images <> nil then
        TActionList(aForm.Components[i]).Images := DefaultIcons;
    end;
  end;
  {$ENDIF}
end;

function TImageForm.GetSmallArrowIcons: TImageList;
var
  scaleFactor: Double = -1;
begin
  scaleFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scaleFactor, 1.25, FP_CUTOFF) > 0 then
    Result := SmallArrowIcons2
  else
    Result := SmallArrowIcons;
end;

procedure TImageForm.GetDialogButtonImage_115px(index: Integer; image: TCustomBitmap);
var
  aList: TImageList = nil;
  scaleFactor: Double = 1;
begin
  scaleFactor := PixelsPerInch/DesignTimePPI;
  if CompareValue(scaleFactor, 1.25, FP_CUTOFF) >= 0 then
    aList := DlgButtons2
  else
    aList := DlgButtons;
  Assert((index >= 0) and (index < aList.Count), Format('TImageList index out of bounds. Got %d but range is 0 - %d', [index, aList.Count - 1]));
  aList.GetBitmap(index, image);
end;

function TImageForm.GetDialogButtonImageList: TImageList;
begin
  if CompareValue(ScalingFactor, 1.25, FP_CUTOFF) >= 0 then
    Result := DlgButtons2
  else
    Result := DlgButtons;
end;

function TImageForm.GetDailogButtonHoverImageList: TImageList;
begin
  if CompareValue(ScalingFactor, 1.25, FP_CUTOFF) >= 0 then
    Result := DlgButtonsHover2
  else
    Result := DlgButtonsHover;
end;

function TImageForm.GetDialogButton2ImageList: TImageList;
begin
  if CompareValue(ScalingFactor, 1.25, FP_CUTOFF) >= 0 then
    Result := Buttons_180x60
  else
    Result := Buttons_90x30;
end;

function TImageForm.GetDailogButton2HoverImageList: TImageList;
begin
  if CompareValue(ScalingFactor, 1.25, FP_CUTOFF) >= 0 then
    Result := Buttons_180x60_hover
  else
    Result := Buttons_90x30_hover;
end;

function TImageForm.GetLogoImageList: TImageList;
begin
  if CompareValue(ScalingFactor, 1.25, FP_CUTOFF) >= 0 then
    Result := Logo_160
  else
    Result := Logo_80;
end;

function TImageForm.GetHelpIconImageList: TImageList;
begin
  if CompareValue(ScalingFactor, 1.25, FP_CUTOFF) >= 0 then
    Result := HelpIconsLarge
  else
    Result := HelpIconsSmall;
end;

end.

