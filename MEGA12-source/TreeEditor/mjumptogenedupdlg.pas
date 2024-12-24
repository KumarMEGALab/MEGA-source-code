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

unit MJumpToGeneDupDlg;

interface
{$IFDEF VISUAL_BUILD}
uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList, ImgList, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, MTreeBox;

type
  TJumpToNodeCallback = procedure(NodeIndex: Integer) of object;

  { TJumpToGeneDupForm }

  TJumpToGeneDupForm = class(TForm)
    CloseAction: TAction;
    ActionList1: TActionList;
    ActionList2: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    GeneDupCountLbl: TLabel;
    GeneDupCurrentLbl: TLabel;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    NextGeneDupAction: TAction;
    PrevGeneDupAction: TAction;
    NextSpeciationAction: TAction;
    PrevSpeciationAction: TAction;
    SpeciationsCountLbl: TLabel;
    SpeciationsCurrentLbl: TLabel;
    StatusBar1: TStatusBar;
    QuitAction: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure CloseBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure NextGeneDupActionExecute(Sender: TObject);
    procedure PrevGeneDupActionExecute(Sender: TObject);
    procedure NextSpeciationActionExecute(Sender: TObject);
    procedure PrevSpeciationActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure QuitActionExecute(Sender: TObject);
  private
    procedure UpdateLabels;
    { Private declarations }
  public
    Index: Integer;
    NodeId: Integer;
    JumpCallback: TJumpToNodeCallback;
    Tree: TTreeCustomControl;
    { Public declarations }
  end;

var
  JumpToGeneDupForm: TJumpToGeneDupForm;
{$ENDIF}

implementation

{$IFDEF VISUAL_BUILD}
uses
  MegaConsts, mimageform;

{$R *.lfm}

procedure TJumpToGeneDupForm.FormShow(Sender: TObject);
var
  NumDups: Integer;
  NumSpeciations: Integer;
begin
  if Assigned(Tree) then
  begin
    NumDups := Tree.NumGeneDups;
    GeneDupCountLbl.Caption := IntToStr(NumDups);
    GeneDupCurrentLbl.Caption := IntToStr(Tree.GeneDupFocusedIndex(True));
    NumSpeciations := Tree.NumSpeciations;
    if NumSpeciations > 0 then
    begin
      SpeciationsCountLbl.Caption := IntToStr(NumSpeciations);
    end
    else
    begin
      SpeciationsCountLbl.Caption := '-';
      NextSpeciationAction.Enabled := False;
      PrevSpeciationAction.Enabled := False;
    end;
    UpdateLabels;
  end;
end;

procedure TJumpToGeneDupForm.NextGeneDupActionExecute(Sender: TObject);
begin
  if Tree.SearchGeneDup(Index, NodeId, sdForward, True) then
  begin
    GeneDupCurrentLbl.Caption := IntToStr(Index);
    UpdateLabels;
    if Assigned(JumpCallback) then
      JumpCallback(NodeId);
  end
  else
    ShowMessage('Oops! No gene duplications found in the tree')
end;

procedure TJumpToGeneDupForm.CloseBtnClick(Sender: TObject);
begin
  QuitActionExecute(Sender);
end;

procedure TJumpToGeneDupForm.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TJumpToGeneDupForm.FormResize(Sender: TObject);
begin
  if ClientWidth > ToolBar1.Width then
    ToolBar1.Left := Round((ClientWidth - ToolBar1.Width)/2);
end;

procedure TJumpToGeneDupForm.NextSpeciationActionExecute(Sender: TObject);
begin
  if Tree.SearchGeneDup(Index, NodeId, sdForward, False) then
  begin
    SpeciationsCurrentLbl.Caption := IntToStr(Index);
    UpdateLabels;
    if Assigned(JumpCallback) then
      JumpCallback(NodeId);
  end
  else
    ShowMessage('Oops! No speciation events found in the tree');
end;

procedure TJumpToGeneDupForm.PrevGeneDupActionExecute(Sender: TObject);
begin
  if Tree.SearchGeneDup(Index, NodeId, sdBackward, True) then
  begin
    GeneDupCurrentLbl.Caption := IntToStr(Index);
    UpdateLabels;
    if Assigned(JumpCallback) then
      JumpCallback(NodeId);
  end
  else
    ShowMessage('Oops! No gene duplication events found in the tree');
end;

procedure TJumpToGeneDupForm.PrevSpeciationActionExecute(Sender: TObject);
begin
  if Tree.SearchGeneDup(Index, NodeId, sdBackward, False) then
  begin
    SpeciationsCurrentLbl.Caption := IntToStr(Index);
    UpdateLabels;
    if Assigned(JumpCallback) then
      JumpCallback(NodeId);
  end
  else
    ShowMessage('Oops! No speciation events found in the tree');
end;

procedure TJumpToGeneDupForm.QuitActionExecute(Sender: TObject);
begin
  Self.Hide;
end;

procedure TJumpToGeneDupForm.UpdateLabels;
begin
  if Tree.FocusedOnGeneDup then
    StatusBar1.SimpleText := 'Focused on gene duplication event'
  else
    GeneDupCurrentLbl.Caption := '-';
  if Tree.FocusedOnSpeciation then
    StatusBar1.SimpleText := 'Focused on speciation event'
  else
    SpeciationsCurrentLbl.Caption := '-';

  if (not Tree.FocusedOnGeneDup) and (not Tree.FocusedOnSpeciation) then
    StatusBar1.SimpleText := EmptyStr;
end;
{$ENDIF}

end.
