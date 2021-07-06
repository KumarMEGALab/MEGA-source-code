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

unit mega_info_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TMegaInfoForm }

  TMegaInfoForm = class(TForm)
    Image1: TImage;
    ImageList1: TImageList;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image1MouseEnter(Sender: TObject);
    procedure Image1MouseLeave(Sender: TObject);
  private

  public
    procedure SetInfoText(aInfo: String);
  end;

var
  MegaInfoForm: TMegaInfoForm;

implementation

uses
  MegaVerConsts, MMegaWindowInfo;

{$R *.lfm}

{ TMegaInfoForm }

procedure TMegaInfoForm.Image1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TMegaInfoForm.FormCreate(Sender: TObject);
begin
  Font.Height := 14;
  Font.Color := $00333333;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX;
  AddWindowToTray(Self);
end;

procedure TMegaInfoForm.FormDestroy(Sender: TObject);
begin
  RemoveWindowFromTray(Self);
end;

procedure TMegaInfoForm.Image1MouseEnter(Sender: TObject);
begin
  ImageList1.GetBitMap(1, Image1.Picture.Bitmap);
end;

procedure TMegaInfoForm.Image1MouseLeave(Sender: TObject);
begin
  ImageList1.GetBitmap(0, Image1.Picture.Bitmap);
end;

procedure TMegaInfoForm.SetInfoText(aInfo: String);
begin
  StaticText1.Caption := Uppercase(aInfo);
end;

end.

