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

unit mappdepot_dlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, cef3lcl, cef3types, cef3intf;

type

  { TAppDepotForm }

  TAppDepotForm = class(TForm)
    BottomPanel: TPanel;
    Chromium1: TChromium;
    HelpButton: TImage;
    CancelButton: TImage;
    OkButton: TImage;
    ButtonImages: TImageList;
    HelpButtonImages: TImageList;
    procedure CancelButtonClick(Sender: TObject);
    procedure CancelButtonMouseEnter(Sender: TObject);
    procedure CancelButtonMouseLeave(Sender: TObject);
    procedure Chromium1ProcessMessageReceived(Sender: TObject;
      const Browser: ICefBrowser; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure HelpButtonMouseEnter(Sender: TObject);
    procedure HelpButtonMouseLeave(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure OkButtonMouseEnter(Sender: TObject);
    procedure OkButtonMouseLeave(Sender: TObject);
  private
    FAppDepotUrl: String;
  public
    property AppDepotUrl: String read FAppDepotUrl write FAppDepotUrl;
  end;

var
  AppDepotForm: TAppDepotForm;

implementation

uses
  mbrowserutils, cef3ref, mappflows_form, mega_main;

{$R *.lfm}

{ TAppDepotForm }

procedure TAppDepotForm.FormCreate(Sender: TObject);
begin

  {$IFDEF DEBUG}
  FAppDepotUrl := 'http://www-dev4.cst.temple.edu/';
  Chromium1.Load(FAppDepotUrl);
  {$ELSE}

  {$ENDIF}
end;

procedure TAppDepotForm.HelpButtonClick(Sender: TObject);
begin
  ShowMessage('AppDepot help is not yet implemented');
end;

procedure TAppDepotForm.HelpButtonMouseEnter(Sender: TObject);
begin
  HelpButtonImages.GetBitmap(1, HelpButton.Picture.Bitmap);
end;

procedure TAppDepotForm.HelpButtonMouseLeave(Sender: TObject);
begin
  HelpButtonImages.GetBitmap(0, HelpButton.Picture.Bitmap);
end;

procedure TAppDepotForm.OkButtonClick(Sender: TObject);
begin
  if not Chromium1.Browser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(VISITDOMPROC_APPDEPOT_GET_SELECTIONS)) then
    ShowMessage('Failed to send message to render process');
end;

procedure TAppDepotForm.OkButtonMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(1, OkButton.Picture.Bitmap);
end;

procedure TAppDepotForm.OkButtonMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(0, OkButton.Picture.Bitmap);
end;

procedure TAppDepotForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAppDepotForm.CancelButtonMouseEnter(Sender: TObject);
begin
  ButtonImages.GetBitmap(3, CancelButton.Picture.Bitmap);
end;

procedure TAppDepotForm.CancelButtonMouseLeave(Sender: TObject);
begin
  ButtonImages.GetBitmap(2, CancelButton.Picture.Bitmap);
end;

procedure TAppDepotForm.Chromium1ProcessMessageReceived(Sender: TObject; const Browser: ICefBrowser; sourceProcess: TCefProcessId; const message: ICefProcessMessage; out Result: Boolean);
var
  selectedProcessTypes: ustring;
  i: Integer;
  wf: TWorkflowForm = nil;
  selections: TStringList = nil;
begin
  try
    selectedProcessTypes := '';
    selections := TStringList.Create;
    if message.Name = VISITDOMPROC_APPDEPOT_GET_SELECTIONS_COMPLETE then
    begin
      if message.ArgumentList.GetSize > 0 then
        for i := 0 to message.ArgumentList.GetSize - 1 do
        begin
          selectedProcessTypes := message.ArgumentList.GetString(i) + LineEnding;
          selections.Add(selectedProcessTypes);
        end;
      wf := GetWorkflowForm;
      wf.AddAppStoreSelections(selections);
      if MegaRunMode <> mrmWorkflow then
        MegaForm.SetRunMode(mrmWorkflow);
      wf.SetSequenceDataType(MegaForm.DataType, MegaForm.containsCodingNuc);
    end;
  finally
    if Assigned(selections) then
      selections.Free;
  end;
end;

end.

