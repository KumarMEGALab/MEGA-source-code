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

unit compute_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin,
  frame_utils, ComCtrls;

type

  { TComputeFrame }

  TComputeFrame = class(TFrame, ITreeToolbarFrame)
    Image1: TImage;
    Label1: TLabel;
    ConsensusLabel: TLabel;
    CondensedLabel: TLabel;
    ConsensusLabel2: TLabel;
    CondensedLabel2: TLabel;
    OperationLabel: TLabel;
    MolecularClockLabel: TLabel;
    CorrtestLabel: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ConsensusTreeCutoffSpinEdit: TSpinEdit;
    CondensedTreeCutoffSpinEdit: TSpinEdit;
    ConsensusToolbar: TToolBar;
    ConsensusTreeBtn: TToolButton;
    CondensedToolbar: TToolBar;
    CondensedTreeBtn: TToolButton;
    OperationToolbar: TToolBar;
    CorrtestToolbar: TToolBar;
    MolecularClockToolbar: TToolBar;
    ComputeTimetreeBtn: TToolButton;
    MolecularClockBtn: TToolButton;
    CorrtestBtn: TToolButton;
  private
    FWasAutoCollapsed: Boolean;
    FPanel2Height: Integer;
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure ToggleCollapseBodyPanel;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);
  end;

implementation

{$R *.lfm}

{ TComputeFrame }

procedure TComputeFrame.SetEnabled(Value: Boolean);
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

function TComputeFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TComputeFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TComputeFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TComputeFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TComputeFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
  FWasAutoCollapsed := False;
end;

procedure TComputeFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

end.

