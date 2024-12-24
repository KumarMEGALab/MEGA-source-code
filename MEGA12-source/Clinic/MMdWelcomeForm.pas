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

unit MMdWelcomeForm;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ActnList, ComCtrls;

type

  { TMdWelcomeForm }

  TMdWelcomeForm = class(TForm)
    FileAction: TAction;
    CancelAction: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    WizardAction: TAction;
    ActionList1: TActionList;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure DoInteractiveButtonClick(Sender: TObject);
    procedure LoadFileButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FWizardMsg: String;
    FFileMsg: String;
  public

  end;

var
  MdWelcomeForm: TMdWelcomeForm;

implementation

uses
  MutationExplorer, Mega_Main, mimageform, MegaUtils;

{$R *.lfm}

procedure TMdWelcomeForm.CancelButtonClick(Sender: TObject);
begin
  {$IFDEF MYPEG_ONLY}
  Application.Terminate;
  {$ELSE}
  ModalResult := mrOk;
  {$ENDIF}
end;

procedure TMdWelcomeForm.DoInteractiveButtonClick(Sender: TObject);
begin
  if not Assigned(MutationExplorerForm) then
  begin
    MutationExplorerForm := TMutationExplorerForm.Create(MegaForm);
    MutationExplorerForm.Show;
  end
  else
  begin
    if MutationExplorerForm.WindowState = wsMinimized then
      MutationExplorerForm.WindowState := wsNormal;
    MutationExplorerForm.Visible := True;
    MutationExplorerForm.BringToFront;
  end;
  MutationExplorerForm.GeneSearchPage.TabVisible := True;
  MutationExplorerForm.PageControl.TabIndex := GENE_SEARCH_PAGE_INDEX;
  MutationExplorerForm.Edit1.SetFocus;
  ModalResult := mrOk;
end;

procedure TMdWelcomeForm.FormCreate(Sender: TObject);
begin
  Color := clWhite;
  FWizardMsg := 'The wizard system allows you to first search for a gene by name, ' +
               'product, or RefSeq Id (mRNA Id or peptide Id) and then select an ' +
               'amino acid position and mutation(s) of interest using a graphical ' +
               'alignment explorer.';

  FFileMsg := 'When specifying mutations via text file, coordinate information for ' +
             'each variant of interest should be specified on its own line using the ' +
             'following format:' + LineEnding + LineEnding +
             '      Peptide_Id  ' + 'Amino_acid_Position  ' + 'Mutant_Amino_Acid' + LineEnding + LineEnding +
             'for example:' + LineEnding + LineEnding +
             '      NP_000252   76   K';
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  SpeedButton1.Font.Color := clBlue;
  SpeedButton2.Font.Color := clBlue;
  WizardAction.Hint := FWizardMsg;
  FileAction.Hint := FFileMsg;
end;

procedure TMdWelcomeForm.FormShow(Sender: TObject);
begin
  Toolbar1.Images := ImageForm.GetDialogButton2ImageList;
  Toolbar1.HotImages := ImageForm.GetDailogButton2HoverImageList;
  Toolbar1.ImagesWidth := Toolbar1.ButtonWidth;
end;

procedure TMdWelcomeForm.LoadFileButtonClick(Sender: TObject);
begin
  if not Assigned(MutationExplorerForm) then
    MutationExplorerForm := TMutationExplorerForm.Create(MegaForm);
  MutationExplorerForm.GeneSearchPage.TabVisible := False;
  MutationExplorerForm.PageControl.TabIndex := PREDICTION_DATA_PAGE_INDEX;
  MutationExplorerForm.ImportQueryDataFileActionExecute(nil);
  ModalResult := mrOk;
end;

procedure TMdWelcomeForm.SpeedButton1Click(Sender: TObject);
begin
  ShowMessage(FWizardMsg);
end;

procedure TMdWelcomeForm.SpeedButton2Click(Sender: TObject);
begin
  ShowMessage(FFileMsg);
end;

end.
