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

unit taxa_names_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Spin, frame_utils;

type

  { TTaxaNamesFrame }

  TTaxaNamesFrame = class(TFrame, ITreeToolbarFrame)
    TaxaNameFontBtn: TButton;
    EditMarkersBtn: TButton;
    ShowTaxaNamesCheckBx: TCheckBox;
    HideOverlappingTaxaCheckBx: TCheckBox;
    ShowNodeMarkersCheckBx: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    InputOrderRadioBtn: TRadioButton;
    BalancedShapeRadioBtn: TRadioButton;
    TaxaFontSizeSpinEdit: TSpinEdit;
    procedure Panel1Click(Sender: TObject);
  private
    FPanel2Height: Integer;
    procedure SetBodyPanelHeight(AValue: Integer);
  public
    procedure ToggleCollapseBodyPanel;
    function GetHeaderPanel: TPanel;
    function GetBodyPanel: TPanel;
    function GetExpandCollapseImage: TImage;
    procedure SetParentControl(aParent: TWinControl);
    procedure SetAlignVal(aAlign: TAlign);
    property BodyPanelHeight: Integer read FPanel2Height write SetBodyPanelHeight;
  end;

implementation

uses
  Graphics, MegaConsts;

{$R *.lfm}

{ TTaxaNamesFrame }

procedure TTaxaNamesFrame.Panel1Click(Sender: TObject);
begin
  if Panel2.Visible then FPanel2Height := Height;
  Panel2.Visible := not Panel2.Visible;
  if Panel2.Visible then
    Height := FPanel2Height
  else
    Height := Panel1.Height;
end;

procedure TTaxaNamesFrame.SetBodyPanelHeight(AValue: Integer);
begin
  if FPanel2Height = AValue then Exit;
  FPanel2Height := AValue;
  Invalidate;
end;

procedure TTaxaNamesFrame.ToggleCollapseBodyPanel;
begin
  Panel1Click(nil);
end;

function TTaxaNamesFrame.GetHeaderPanel: TPanel;
begin
  Result := Panel1;
end;

function TTaxaNamesFrame.GetBodyPanel: TPanel;
begin
  Result := Panel2;
end;

function TTaxaNamesFrame.GetExpandCollapseImage: TImage;
begin
  Result := Image1;
end;

procedure TTaxaNamesFrame.SetParentControl(aParent: TWinControl);
begin
  Parent := aParent;
end;

procedure TTaxaNamesFrame.SetAlignVal(aAlign: TAlign);
begin
  Align := aAlign;
end;

end.

