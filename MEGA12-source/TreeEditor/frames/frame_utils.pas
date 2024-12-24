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

unit frame_utils;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Forms, Graphics, StdCtrls, ExtCtrls, Controls, ComCtrls, ImgList,
  ActnList, fgl;

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

  IDisablesEventNotifications = interface
    ['{FD2954B8-2EBC-458C-8108-B20FC8BEF36A}']
    function NotificationsDisabled: Boolean;
  end;

  { TActionLabelPair }

  TActionLabelPair = class(TObject)
      FAction: TCustomAction;
      FLabel: TLabel;
    public
      constructor Create(aAction: TCustomAction; aLabel: TLabel);
      procedure CheckLabelEnabled;
      function DebugString: String;
  end;

  TActionLabelPairList = specialize TFPGList<TActionLabelPair>;

  { TFrameUtils }

  TFrameUtils = class(TObject)
    private
      FActionLabelPairs: TActionLabelPairList;
      FLeftMargin: Integer;
      FSpacerHeight: Integer;
      FToolbarHeight: Integer;
      FTopMargin: Integer;
      function DoAddToolbarToFrame(aFrame: ITreeToolbarFrame; aToolBar: TToolBar; aActions: TList; var aLabel: TLabel; images: TCustomImageList; alignment: TAlignment = taLeftJustify):TToolBar; overload;
    public
      PanelOpenNotify: TNotifyEvent;
      constructor Create(aToolbarHeight, aLeftMargin, aTopMargin, aSpacerHeight: Integer);
      destructor Destroy; override;
      procedure CheckLabelsEnabled;
      procedure InitFrame(aFrame: ITreeToolbarFrame; aParent: TWinControl; IsExpanded: Boolean);
      procedure DoToggleCollapsePanel(f: ITreeToolbarFrame);
      procedure PanelOnMouseEnter(aPanel: TObject);
      procedure PanelOnMouseLeave(aPanel: TObject);
      procedure ImageOnMouseEnter(aImage: TObject);
      procedure ImageOnMouseLeave(aImage: TObject);
      procedure PanelOnClick(aPanel: TObject);
      procedure CollapsExandImageOnClick(aImage: TObject);
      procedure AddActionLabel(aAction: TCustomAction; aLabel: TLabel);
      function AddToolbarToFrame(aFrame: ITreeToolbarFrame; aActions: TList; aLabel: String; images: TCustomImageList; alignment: TAlignment = taLeftJustify):TToolBar; overload;
      function AddToolbarToFrame(aFrame: ITreeToolbarFrame; aActions: TList; var aLabel: TLabel; images: TCustomImageList; alignment: TAlignment = taLeftJustify):TToolBar; overload;

      function AddSpacerToFrame(aFrame: ITreeToolbarFrame): Integer;
      function AddPanelToFrame(aFrame: ITreeToolbarFrame; aPanel: TPanel): Integer;
      function AddDividerToFrame(aFrame: ITreeToolbarFrame): Integer;
      procedure MouseEnterLabel(Sender: TObject);
      procedure MouseExitLabel(Sender: TObject);

      property ToolbarHeight: Integer read FToolbarHeight write FToolbarHeight;
      property TopMargin: Integer read FTopMargin write FTopMargin;
      property LeftMargin: Integer read FLeftMargin write FLeftMargin;
      property SpacerHeight: Integer read FSpacerHeight write FSpacerHeight;
  end;

var
  FrameUtils: TFrameUtils = nil;

implementation

uses
  {$IFDEF DEBUG}
  MegaUtils,
  {$ENDIF}
  MegaConsts, mimageform, mdeveloper_console, Toolwin;

{ TActionLabelPair }

constructor TActionLabelPair.Create(aAction: TCustomAction; aLabel: TLabel);
begin
  Assert(Assigned(aAction));
  Assert(Assigned(aLabel));
  FAction := aAction;
  FLabel := aLabel;
end;

procedure TActionLabelPair.CheckLabelEnabled;
begin
  try
    if Assigned(FAction) and Assigned(FLabel) then
      if FAction.Enabled <> FLabel.Enabled then
        FLabel.Enabled := FAction.Enabled;
  except
    on E:Exception do
    begin
      {$IFDEF DEBUG}
      WriteToDevConsole(Format('TActionLabelPair.CheckLabelEnabled error: %s', [E.Message]));
      WriteToDevConsole(ExceptionCallStack(E));
      {$ENDIF}
    end;
  end;
end;

function TActionLabelPair.DebugString: String;
begin
  try
    if Assigned(FAction) and Assigned(FLabel) then
      Result := Format('action=%s label=%s', [FAction.Caption, FLabel.Caption])
    else if Assigned(FAction) then
      Result := Format('action=%s label=nil', [FAction.Caption])
    else if Assigned(FLabel) then
      Result := Format('action=nil label=%s', [FLabel.Caption])
    else
      Result := 'action=nil label=nil';
  except
    on E:Exception do
    begin
      {$IFDEF DEBUG}
      WriteToDevConsole(Format('TActionLabelPair.DebugString error: %s', [E.Message]));
      WriteToDevConsole(ExceptionCallStack(E));
      {$ENDIF}
    end;
  end;
end;

procedure TFrameUtils.DoToggleCollapsePanel(f: ITreeToolbarFrame);
begin
  if not f.GetBodyPanel.Visible then { a panel is being opened}
  begin
    if Assigned(PanelOpenNotify) then
      PanelOpenNotify(f.GetBodyPanel.Parent);
  end;
  f.ToggleCollapseBodyPanel;
  if f.GetBodyPanel.Visible then
    ImageForm.GetSmallArrowIcons.GetBitmap(EXPANDED_ARROW_INDEX, f.GetExpandCollapseImage.Picture.Bitmap)
  else
    ImageForm.GetSmallArrowIcons.GetBitmap(COLLAPSED_ARROW_INDEX, f.GetExpandCollapseImage.Picture.Bitmap);
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
    ImageForm.GetSmallArrowIcons.GetBitmap(EXPANDED_ARROW_INDEX, aFrame.GetExpandCollapseImage.Picture.Bitmap);
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

procedure TFrameUtils.AddActionLabel(aAction: TCustomAction; aLabel: TLabel);
var
  aPair: TActionLabelPair = nil;
begin
  aPair := TActionLabelPair.Create(aAction, aLabel);
  FActionLabelPairs.Add(aPair);
end;

function TFrameUtils.AddToolbarToFrame(aFrame: ITreeToolbarFrame;
  aActions: TList; aLabel: String; images: TCustomImageList;
  alignment: TAlignment): TToolBar;
var
  aToolbar: TToolBar = nil;
  aLbl: TLabel = nil;
begin
  Assert(aActions.Count > 0);
  aToolBar := TToolBar.Create(aFrame.GetBodyPanel);
  if aLabel <> EmptyStr then
  begin
    aLbl := TLabel.Create(aFrame.GetBodyPanel);
    aLbl.Caption := StringReplace(aLabel, '&', '', []);
  end;
  Result := DoAddToolbarToFrame(aFrame, aToolBar, aActions, aLbl, images, alignment);
end;

function TFrameUtils.AddToolbarToFrame(aFrame: ITreeToolbarFrame;
  aActions: TList; var aLabel: TLabel; images: TCustomImageList;
  alignment: TAlignment): TToolBar;
var
  aToolBar: TToolBar = nil;
begin
  aToolBar := TToolBar.Create(aFrame.GetBodyPanel);
  aToolBar.EdgeInner := esNone;
  aToolBar.EdgeOuter := esNone;
  aToolBar.Indent := 0;
  Result := DoAddToolbarToFrame(aFrame, aToolBar, aActions, aLabel, images, alignment);
end;

function TFrameUtils.DoAddToolbarToFrame(aFrame: ITreeToolbarFrame; aToolBar: TToolBar; aActions: TList; var aLabel: TLabel; images: TCustomImageList; alignment: TAlignment): TToolBar;
var
  i: Integer = 0;
  aParent: TPanel = nil;
  aButton: TToolButton = nil;
  aEvent: TNotifyEvent = nil;
  aAction: TCustomAction = nil;
  aPanel: TPanel = nil;
  aPair: TActionLabelPair = nil;
begin
  Assert(aActions.Count > 0);

  aParent := aFrame.GetBodyPanel;
  aPanel := TPanel.Create(aParent);
  aPanel.Parent := aParent;
  aPanel.Align := alTop;
  aPanel.BorderStyle := bsNone;
  aPanel.BevelInner := bvNone;
  aPanel.BevelOuter := bvNone;

  Result := aToolBar;
  Result.Parent := aPanel;
  Result.Images := images;
  Result.Height := ToolbarHeight;
  Result.ButtonHeight := Result.Height - 2;
  Result.ButtonWidth := Result.ButtonHeight;
  for i := 0 to aActions.Count - 1 do
  begin
    aButton := TToolButton.Create(Result);
    aButton.Parent := Result;
    aButton.Action := TBasicAction(aActions[i]);
  end;

  Result.Align := alNone;
  Result.Top := TopMargin div 2;
  Result.Width := Result.ButtonWidth*Result.ButtonCount + Result.ButtonCount*2;
  Result.Wrapable := False;
  Result.EdgeInner := esNone;
  Result.EdgeOuter := esNone;
  Result.EdgeBorders := [ebTop, ebBottom, ebLeft, ebRight];
  case alignment of
    taLeftJustify: Result.Left := LeftMargin - 4;
    taRightJustify: Result.Left := aParent.Width - Result.Width - LeftMargin;
    taCenter:
      begin
        Result.Left := Round((aParent.Width - Result.Width)/2);
      end;
  end;

  aPanel.Height := Result.Height + 4;

  if Assigned(aLabel) then
  begin
    aLabel.Parent := aPanel;
    aLabel.Top := Round((Result.Height - aLabel.Height)/2);
    aLabel.Left := Result.Left + Result.Width + (LeftMargin div 2);
    if aActions.Count = 1 then
    begin
      aAction := TCustomAction(aActions[0]);
      aEvent := aAction.OnExecute;
      aLabel.OnClick := aEvent;
      aLabel.OnMouseEnter := @MouseEnterLabel;
      aLabel.OnMouseLeave := @MouseExitLabel;
      aPair := TActionLabelPair.Create(aAction, aLabel);
      FActionLabelPairs.Add(aPair);
    end;
  end;
end;

constructor TFrameUtils.Create(aToolbarHeight, aLeftMargin, aTopMargin, aSpacerHeight: Integer);
begin
  FLeftMargin := aLeftMargin;
  FTopMargin := aTopMargin;
  FSpacerHeight := aSpacerHeight;
  FToolbarHeight := aToolbarHeight;
  FActionLabelPairs := TActionLabelPairList.Create;
end;

destructor TFrameUtils.Destroy;
var
  i: Integer = -1;
begin
  if FActionLabelPairs.Count > 0 then
    for i := 0 to FActionLabelPairs.Count - 1 do
      FActionLabelPairs[i].Free;
  FActionLabelPairs.Free;
  inherited Destroy;
end;

procedure TFrameUtils.CheckLabelsEnabled;
var
  i: Integer = -1;
begin
  if FActionLabelPairs.Count > 0 then
    for i := 0 to FActionLabelPairs.Count - 1 do
      FActionLabelPairs[i].CheckLabelEnabled;
end;

function TFrameUtils.AddSpacerToFrame(aFrame: ITreeToolbarFrame): Integer;
var
  aPanel: TPanel = nil;
  aParent: TPanel = nil;
begin
  aParent := aFrame.GetBodyPanel;
  aPanel := TPanel.Create(aParent);
  aPanel.Parent := aParent;
  aPanel.Align := alTop;
  aPanel.BorderStyle := bsNone;
  aPanel.BevelInner := bvNone;
  aPanel.BevelOuter := bvNone;
  aPanel.Height := 10;
  Result := aPanel.Height;
end;

function TFrameUtils.AddPanelToFrame(aFrame: ITreeToolbarFrame; aPanel: TPanel): Integer;
var
  aParent: TPanel = nil;
begin
  aParent := aFrame.GetBodyPanel;
  aPanel.Parent := aParent;
  aPanel.Align := alTop;
  aPanel.BorderStyle := bsNone;
  aPanel.BevelInner := bvNone;
  aPanel.BevelOuter := bvNone;
  Result := aPanel.Height;
end;

function TFrameUtils.AddDividerToFrame(aFrame: ITreeToolbarFrame): Integer;
var
  aParent: TPanel = nil;
  p: TPanel = nil;
begin
  aParent := aFrame.GetBodyPanel;
  p := TPanel.Create(aParent);
  p.Parent := aParent;
  p.Align := alTop;
  p.BorderStyle := bsNone;
  p.BevelInner := bvNone;
  p.BevelOuter := bvNone;
  p.Height := 1;
  p.Color := $00dddddd;
  Result := p.Height;
end;

procedure TFrameUtils.MouseEnterLabel(Sender: TObject);
begin
  if Sender is TLabel then
    with Sender as TLabel do
    begin
      Font.Style := Font.Style + [fsUnderline];
      Invalidate;
    end;
end;

procedure TFrameUtils.MouseExitLabel(Sender: TObject);
begin
  if Sender is TLabel then
    with Sender as TLabel do
    begin
      Font.Style := Font.Style - [fsUnderline];
      Invalidate;
    end;
end;

end.

