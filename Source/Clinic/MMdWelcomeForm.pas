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

unit MMdWelcomeForm;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, mimageform;

type

  { TMdWelcomeForm }

  TMdWelcomeForm = class(TForm)
    ButtonImages: TImageList;
    WizardBtn: TImage;
    FileBtn: TImage;
    CancelBtn: TImage;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label2: TLabel;
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure FileBtnMouseEnter(Sender: TObject);
    procedure FileBtnMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure InitImages;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure DoInteractiveButtonClick(Sender: TObject);
    procedure LoadFileButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure WizardBtnMouseEnter(Sender: TObject);
    procedure WizardBtnMouseLeave(Sender: TObject);
  private
    FWizardMsg: String;
    FFileMsg: String;
  public

  end;

var
  MdWelcomeForm: TMdWelcomeForm;

implementation

uses
  MutationExplorer, Mega_Main;

{$R *.lfm}

procedure TMdWelcomeForm.CancelButtonClick(Sender: TObject);
begin
  {$IFDEF MYPEG_ONLY}
  Application.Terminate;
  {$ELSE}
  ModalResult := mrOk;
  {$ENDIF}
end;

procedure TMdWelcomeForm.WizardBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(1, WizardBtn.Picture.Bitmap);
end;

procedure TMdWelcomeForm.WizardBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(0, WizardBtn.Picture.Bitmap);
end;

procedure TMdWelcomeForm.DoInteractiveButtonClick(Sender: TObject);
begin
  if not Assigned(MutationExplorerForm) then
    MutationExplorerForm := TMutationExplorerForm.Create(MegaForm);
  MutationExplorerForm.GeneSearchPage.TabVisible := True;
  MutationExplorerForm.PageControl.TabIndex := GENE_SEARCH_PAGE_INDEX;
  MutationExplorerForm.Show;
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
  InitImages;
  ImageForm.UpdateImgList(Self);
end;

procedure TMdWelcomeForm.InitImages;
begin
  {$IFDEF DARWIN}
  WizardBtn.Proportional:=True;
  FileBtn.Proportional:=True;
  CancelBtn.Proportional:=True;
  {$ENDIF}
end;

procedure TMdWelcomeForm.FileBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(3, FileBtn.Picture.Bitmap);
end;

procedure TMdWelcomeForm.CancelBtnMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(5, CancelBtn.Picture.Bitmap);
end;

procedure TMdWelcomeForm.CancelBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(4, CancelBtn.Picture.Bitmap);
end;

procedure TMdWelcomeForm.FileBtnMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(2, FileBtn.Picture.Bitmap);
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
