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

unit frame_utils;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Forms, Graphics, ExtCtrls, Controls;

type
  ITreeToolbarFrame = interface
     ['{E3D41699-887B-4837-8E69-3E1B625A38D1}']
     function GetHeaderPanel: TPanel;
     function GetBodyPanel: TPanel;
     function GetExpandCollapseImage: TImage;
     procedure ToggleCollapseBodyPanel;
     procedure SetParentControl(aParent: TWinControl);
     procedure SetAlignVal(aAlign: TAlign);
  end;

  { TFrameUtils }

  TFrameUtils = class(TObject)
    private
      procedure DoToggleCollapsePanel(f: ITreeToolbarFrame);
    public
      procedure InitFrame(aFrame: ITreeToolbarFrame; aParent: TWinControl; IsExpanded: Boolean);
      procedure PanelOnMouseEnter(aPanel: TObject);
      procedure PanelOnMouseLeave(aPanel: TObject);
      procedure ImageOnMouseEnter(aImage: TObject);
      procedure ImageOnMouseLeave(aImage: TObject);
      procedure PanelOnClick(aPanel: TObject);
      procedure CollapsExandImageOnClick(aImage: TObject);
  end;

var
  FrameUtils: TFrameUtils = nil;

implementation

uses
  MegaConsts, mimageform;

procedure TFrameUtils.DoToggleCollapsePanel(f: ITreeToolbarFrame);
begin
  f.ToggleCollapseBodyPanel;
  if f.GetBodyPanel.Visible then
    ImageForm.SmallArrowIcons.GetBitmap(EXPANDED_ARROW_INDEX, f.GetExpandCollapseImage.Picture.Bitmap)
  else
    ImageForm.SmallArrowIcons.GetBitmap(COLLAPSED_ARROW_INDEX, f.GetExpandCollapseImage.Picture.Bitmap);
end;

procedure TFrameUtils.InitFrame(aFrame: ITreeToolbarFrame; aParent: TWinControl; IsExpanded: Boolean);
begin
  aFrame.SetParentControl(aParent);
  aFrame.SetAlignVal(alTop);
  aFrame.GetHeaderPanel.OnMouseEnter := @PanelOnMouseEnter;
  aFrame.GetHeaderPanel.OnMouseLeave := @PanelOnMouseLeave;
  aFrame.GetHeaderPanel.OnClick := @PanelOnClick;
  aFrame.GetExpandCollapseImage.OnMouseEnter := @ImageOnMouseEnter;
  aFrame.GetExpandCollapseImage.OnMouseLeave := @ImageOnMouseLeave;
  aFrame.GetExpandCollapseImage.OnClick := @CollapsExandImageOnClick;
  aFrame.GetHeaderPanel.Color :=  TE_FRAME_HEADER_DEFAULT_COLOR;
  aFrame.GetHeaderPanel.Font.Color := TE_FRAME_HEADER_DEFAULT_FONT_COLOR;
  aFrame.GetBodyPanel.Color := TE_FRAME_BODY_COLOR;
  if not IsExpanded then
    DoToggleCollapsePanel(aFrame)
  else
    ImageForm.SmallArrowIcons.GetBitmap(EXPANDED_ARROW_INDEX, aFrame.GetExpandCollapseImage.Picture.Bitmap);
  aFrame.GetExpandCollapseImage.Invalidate;
end;

procedure TFrameUtils.PanelOnMouseEnter(aPanel: TObject);
begin
  if aPanel is TPanel then
  begin
    with aPanel as TPanel do
    begin
      Color := TE_FRAME_HEADER_HOVER_COLOR;
      Font.Color := TE_FRAME_HEADER_HOVER_FONT_COLOR;
      Invalidate;
    end;
  end
  else
    raise Exception.Create('invalid class passed to TFrameUtils: ' + aPanel.ClassName);
end;

procedure TFrameUtils.PanelOnMouseLeave(aPanel: TObject);
begin
  if aPanel is TPanel then
  begin
    with aPanel as TPanel do
    begin
      Color := TE_FRAME_HEADER_DEFAULT_COLOR;
      Font.Color := TE_FRAME_HEADER_DEFAULT_FONT_COLOR;
      Invalidate;
    end;
  end
  else
    raise Exception.Create('invalid class passed to TFrameUtils: ' + aPanel.ClassName);
end;

procedure TFrameUtils.ImageOnMouseEnter(aImage: TObject);
var
  aPanel: TPanel = nil;
  img: TImage;
begin
  if aImage is TImage then
  begin
    img := TImage(aImage);
    if img.Parent is TPanel then
    begin
      aPanel := TPanel(img.Parent);
      with aPanel as TPanel do
      begin
        Color := TE_FRAME_HEADER_HOVER_COLOR;
        Font.Color := TE_FRAME_HEADER_HOVER_FONT_COLOR;
        Invalidate;
      end;
    end;
  end
  else
    raise Exception.Create('invalid class passed to TFrameUtils: ' + aPanel.ClassName);
end;

procedure TFrameUtils.ImageOnMouseLeave(aImage: TObject);
var
  aPanel: TPanel = nil;
  img: TImage;
begin
  if aImage is TImage then
  begin
    img := TImage(aImage);
    if img.Parent is TPanel then
    begin
      aPanel := TPanel(img.Parent);
      with aPanel as TPanel do
      begin
        Color := TE_FRAME_HEADER_DEFAULT_COLOR;
        Font.Color := TE_FRAME_HEADER_DEFAULT_FONT_COLOR;
        Invalidate;
      end;
    end;
  end
  else
    raise Exception.Create('invalid class passed to TFrameUtils: ' + aPanel.ClassName);
end;

procedure TFrameUtils.PanelOnClick(aPanel: TObject);
var
  p: TPanel = nil;
  f: ITreeToolbarFrame;
begin
  if aPanel is TPanel then
  begin
    p := TPanel(aPanel);
    if Assigned(p.Owner) and Supports(p.Owner, ITreeToolbarFrame) then
    begin
      f := (p.Owner as ITreeToolbarFrame);
      DoToggleCollapsePanel(f);
      if Assigned(p.Parent) then
        p.Parent.Invalidate;
      if Assigned(p.Parent.Parent) then
        p.Parent.Parent.Invalidate;
    end
    else
      raise Exception.Create('invalid class owner passed to TFrameUtils');
  end
  else
    raise Exception.Create('invalid class passed to TFrameUtils: ' + aPanel.ClassName);
end;

procedure TFrameUtils.CollapsExandImageOnClick(aImage: TObject);
var
  p: TPanel = nil;
  f: ITreeToolbarFrame;
  aImg: TImage = nil;
begin
  if aImage is TImage then
  begin
    aImg := TImage(aImage);
    if Assigned(aImg.Parent) and (aImg.Parent is TPanel) then
    begin
      p := TPanel(aImg.Parent);
      if Assigned(p.Owner) and Supports(p.Owner, ITreeToolbarFrame) then
      begin
        f := (p.Owner as ITreeToolbarFrame);
        DoToggleCollapsePanel(f);
        if Assigned(p.Parent) and Assigned(p.Parent.Parent) then
          p.Parent.Parent.Invalidate;
      end
      else
        raise Exception.Create('invalid class owner passed to TFrameUtils');
    end;
  end
  else
    raise Exception.Create('invalid class passed to TFrameUtils: ' + aImage.ClassName);
end;

end.

