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

unit maligneditwelcomedlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, mimageform;

type

  { TWelcomeForm }

  TWelcomeForm = class(TForm)
    CancelBtnImg: TImage;
    OkBtnImg: TImage;
    ImageList1: TImageList;
    Panel1: TPanel;
    rgAlignEditInitOptions: TRadioGroup;
    procedure CancelBtnImgMouseEnter(Sender: TObject);
    procedure CancelBtnImgMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CancelBtnImgClick(Sender: TObject);
    procedure OkBtnImgClick(Sender: TObject);
    procedure OkBtnImgMouseEnter(Sender: TObject);
    procedure OkBtnImgMouseLeave(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  WelcomeForm: TWelcomeForm;

implementation

uses
  MegaVerConsts;

{$R *.lfm}

{ TWelcomeForm }

procedure TWelcomeForm.FormCreate(Sender: TObject);
begin
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Alignment Editor';
  ImageList1.GetBitmap(0, OkBtnImg.Picture.Bitmap);
  ImageList1.GetBitmap(2, CancelBtnImg.Picture.Bitmap);
  {$IFDEF DARWIN}
  OkBtnImg.Proportional:=True;
  CancelBtnImg.Proportional:=True;
  {$ENDIF}
  ImageForm.UpdateImgList(Self);
end;

procedure TWelcomeForm.CancelBtnImgMouseEnter(Sender: TObject);
begin
  ImageList1.GetBitmap(3, CancelBtnImg.Picture.Bitmap);
end;

procedure TWelcomeForm.CancelBtnImgMouseLeave(Sender: TObject);
begin
  ImageList1.GetBitmap(2, CancelBtnImg.Picture.Bitmap);
end;

procedure TWelcomeForm.CancelBtnImgClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TWelcomeForm.OkBtnImgClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TWelcomeForm.OkBtnImgMouseEnter(Sender: TObject);
begin
  ImageList1.GetBitmap(1, OkBtnImg.Picture.Bitmap);
end;

procedure TWelcomeForm.OkBtnImgMouseLeave(Sender: TObject);
begin
  ImageList1.GetBitmap(0, OkBtnImg.Picture.Bitmap);
end;

end.

