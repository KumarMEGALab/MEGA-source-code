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

unit layout_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Menus,
  frame_utils, Graphics, ComCtrls, Spin, ActnList;

type

  { TLayoutFrame }

  TLayoutFrame = class(TFrame, ITreeToolbarFrame)
    Image1: TImage;
    Label1: TLabel;
    TopologyOnlyLabel: TLabel;
    RootOnMidpointLabel: TLabel;
    RootOnOutgroupLabel: TLabel;
    AutoSizeTreeLabel: TLabel;
    ManualResizeLabel: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FrameResize(Sender: TObject);
    procedure Panel1MouseEnter(Sender: TObject);
  private
    FWasAutoCollapsed: Boolean;
    FPanel2Height: Integer;
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    Toolbar: TToolBar;
    TreeStyleBtn: TToolButton;
    AlignTaxaNamesCbox: TCheckBox;
    EqualLengthBranchesCbox: TCheckBox;
    DisableEventNotifications: Boolean;
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);
    procedure UpdateCheckStates;
  end;

implementation

uses
  MegaConsts;

{$R *.lfm}

{ TLayoutFrame }

procedure TLayoutFrame.Panel1MouseEnter(Sender: TObject);
begin
  Panel1.Color := HOVER_PANEL_COLOR;
  Panel1.Font.Color := HOVER_PANEL_FONT;
end;

procedure TLayoutFrame.FrameResize(Sender: TObject);
begin
  if Assigned(ToolBar) then
  begin
    ToolBar.Left := Round((Width - ToolBar.Width)/2);
    Invalidate;
  end;
end;

procedure TLayoutFrame.SetEnabled(Value: Boolean);
begin
  if (not Value) and Panel2.Visible then
  begin
    ToggleCollapseBodyPanel;
    FWasAutoCollapsed := True;
  end
  else if Value and (not Panel2.Visible) and FWasAutoCollapsed then
  begin
    ToggleCollapseBodyPanel;
    FWasAutoCollapsed := False;
  end;

  inherited SetEnabled(Value);
end;

function TLayoutFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TLayoutFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TLayoutFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TLayoutFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TLayoutFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
  FWasAutoCollapsed := False;
end;

procedure TLayoutFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

procedure TLayoutFrame.UpdateCheckStates;
var
  i: Integer = 0;
  btn: TToolButton = nil;
begin
  if Toolbar.ButtonCount > 0 then
    for i := 0 to Toolbar.ButtonCount - 1 do
    begin
      btn := Toolbar.Buttons[i];
      if Assigned(btn.Action) and (btn.Action is TAction) then
        btn.Down := TAction(btn.Action).Checked;
    end;
end;

end.

