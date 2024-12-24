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

unit collapse_nodes_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, frame_utils;

type

  { TCollapseNodesFrame }

  TCollapseNodesFrame = class(TFrame, ITreeToolbarFrame)
    ShowClusterSizeCheckbox: TCheckBox;
    CollapseNodesChbx: TCheckBox;
    CollapseBranchesCutoffSpinEdit: TFloatSpinEdit;
    Image1: TImage;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    CollapseGroupsBtn: TRadioButton;
    CollapseSizeBtn: TRadioButton;
    CollapseByDiffBtn: TRadioButton;
    ClusterSizeSpinEdit: TSpinEdit;
  private
    FPanel2Height: Integer;
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

{ TCollapseNodesFrame }

function TCollapseNodesFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TCollapseNodesFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TCollapseNodesFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TCollapseNodesFrame.ToggleCollapseBodyPanel;
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TCollapseNodesFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TCollapseNodesFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

end.

