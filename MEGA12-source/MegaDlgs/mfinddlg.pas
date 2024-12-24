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

unit MFindDlg;

interface
{$IFDEF VISUAL_BUILD}
uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
  StdCtrls, ExtCtrls, ComCtrls, ActnList, MTreeBox, mimageform;
  type

  { TFindDlg }

  TFindDlg = class(TForm)
    CancelAction: TAction;
    NextAction: TAction;
    ActionList1: TActionList;
    ContainsTermBtn: TRadioButton;
    ImageList1: TImageList;
    ResultsInfoLabel: TLabel;
    StartsWithTermBtn: TRadioButton;
    TaxaNameEdit: TEdit;
    SearchLbl: TLabel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure DoSearchClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Search(AStr: String);
    procedure SearchBitBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure TaxaNameEditChange(Sender: TObject);
  private
    FIsNodeSearch: Boolean;
    FNumResults: Integer;
    FResultsFocusedIndex: Integer;
    procedure SetIsNodeSearch(AValue: Boolean);
  public
    PrevQuery   : String;
    Tree: TTreeCustomControl;
    property IsNodeSearch: Boolean read FIsNodeSearch write SetIsNodeSearch;
    property NumResults: Integer read FNumResults;
    property ResultsFocusedIndex: Integer read FResultsFocusedIndex;
  end;

{$ENDIF}
implementation

{$IFDEF VISUAL_BUILD}

{$R *.lfm}

procedure TFindDlg.FormCreate(Sender: TObject);
begin
  ResultsInfoLabel.Caption := EmptyStr;
end;

procedure TFindDlg.FormShow(Sender: TObject);
begin
  TaxaNameEdit.SetFocus;
  ContainsTermBtn.Top := ToolBar1.Top;
  StartsWithTermBtn.Top := ContainsTermBtn.Top;
end;

procedure TFindDlg.DoSearchClick(Sender: TObject);
begin
  Search(TaxaNameEdit.Text);
end;

procedure TFindDlg.Search(AStr: String);
var
  QueryChanged: Boolean = False;
  focusedIndex: Integer = -1;
begin
  QueryChanged := (CompareText(AStr, PrevQuery) <> 0);
  PrevQuery := AStr;
  if FIsNodeSearch then
    FNumResults := Tree.SearchLabelledNodes(AStr, ContainsTermBtn.Checked, True, QueryChanged, focusedIndex)
  else
    FNumResults := Tree.SearchTipNames(AStr, ContainsTermBtn.Checked, True, QueryChanged, focusedIndex);

  if FNumResults > 0 then
  begin
    FResultsFocusedIndex := focusedIndex;
    if FNumResults = 1 then
      ResultsInfoLabel.Caption := '1 match found'
    else
      ResultsInfoLabel.Caption := Format('Focused on #%.0n of %.0n matches found', [focusedIndex*1.0, FNumResults*1.0]);
  end
  else
  begin
    ResultsInfoLabel.Caption := EmptyStr;
    PrevQuery := EmptyStr;
  end;
end;

procedure TFindDlg.SearchBitBtnClick(Sender: TObject);
begin
  Search(TaxaNameEdit.Text);
end;

procedure TFindDlg.BitBtn1Click(Sender: TObject);
begin
  Search(EmptyStr);
  Close;
end;

procedure TFindDlg.TaxaNameEditChange(Sender: TObject);
begin
  ResultsInfoLabel.Caption := EmptyStr;
  if TaxaNameEdit.Text = EmptyStr then
  begin
    PrevQuery := EmptyStr;
    SearchBitBtnClick(Sender);
  end;
end;

procedure TFindDlg.SetIsNodeSearch(AValue: Boolean);
begin
  if FIsNodeSearch = AValue then Exit;
  FIsNodeSearch := AValue;
  if FIsNodeSearch then
    Caption := 'Find Labelled Node'
  else
    Caption := 'Find Taxon Name';
end;

{$ENDIF}
end.
