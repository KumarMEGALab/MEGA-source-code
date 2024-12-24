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

unit MInnerForm;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynMemo, Menus, ActnList, SynEditMiscClasses,
  SynEditSearch, PrintersDlgs, Printers, LCLType, LCL, StdCtrls, IniPropStorage,
  MegaPrivateFiles, MegaUtils;

type
  TFilesDropNotify = procedure(aFiles: TStrings) of object;

  { TInnerForm }

  TInnerForm = class(TForm)
    IniPropStorage1: TIniPropStorage;

    SynMemo1: TSynMemo;
    //SynEditPrint1: TSynEditPrint;
    PrintDialog1: TPrintDialog;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SynMemo1DropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TStrings);
  private
    { Private declarations }
    FFileName: String;
    FCaption: String;  // the name of the source file
    procedure SetFileName(FileName: String); // Sets the file name as well as the caption
    procedure UpdateMaxScrollWidth;
  public
    FilesDropNotify: TFilesDropNotify;

    procedure OpenFile(FileName: String);
    procedure PrintFile;
    procedure SaveFile(FileName: String);
    procedure CloseFile;
    procedure AppendStrings(const Lines: TStringList);
    procedure SetLines(const Lines: TStringList);
    procedure SetLine(const Line: String; Row: Integer);

    function GetNeedsSaved: Boolean;
    function GetLine(Row: Integer): String;
    function GetLines: TStringList;

    property Modified: Boolean read GetNeedsSaved;
    property FileName: String read FFileName write SetFileName;
    property Memo: TSynMemo read SynMemo1;
    property Caption: String read FCaption write FCaption;
  end;

var
  InnerForm: TInnerForm;

implementation

{$R *.lfm}

{ TInnerForm }

procedure TInnerForm.SetLine(const Line: String; Row: Integer);
begin
  SynMemo1.Lines[Row] := Line;
  UpdateMaxScrollWidth;
end;

procedure TInnerForm.SetLines(const Lines: TStringList);
begin
  SynMemo1.Lines.Assign(Lines);
  UpdateMaxScrollWidth;
end;

procedure TInnerForm.SynMemo1DropFiles(Sender: TObject; X, Y: Integer; AFiles: TStrings);
var
  i: Integer;
begin
  if AFiles.Count = 0 then
    Exit;
  try
    if Assigned(FilesDropNotify) then
    begin
      FilesDropNotify(AFiles);
    end
    else
    begin
      for i := 0 to AFiles.Count - 1 do
        if FileExists(AFiles[i]) then
          OpenFile(AFiles[i]);
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TInnerForm.UpdateMaxScrollWidth;
var
  MaxWidth: Integer;
  AWidth: Integer;
  i: Integer;
begin
  Exit; { handled automagically now}
  if SynMemo1.Lines.Count > 0 then
  begin
    try
      SynMemo1.BeginUpdate;
      MaxWidth := 100;
      for i := 0 to SynMemo1.Lines.Count - 1 do
      begin
        AWidth := SynMemo1.Canvas.TextWidth(SynMemo1.Lines[i]);
        if AWidth > MaxWidth then
          MaxWidth := AWidth;
      end;
      //SynMemo1.MaxScrollWidth := MaxWidth + 20;
    finally
      SynMemo1.EndUpdate;
    end;
  end;
end;

function TInnerForm.GetLines: TStringList;
begin
  Result := TStringList.Create;
  Result.Assign(SynMemo1.Lines);
end;

function TInnerForm.GetLine(Row: Integer): String;
begin
  Result := SynMemo1.Lines[Row];
end;

procedure TInnerForm.AppendStrings(const Lines: TStringList);
begin
  SynMemo1.Lines.AddStrings(Lines);
  UpdateMaxScrollWidth;
end;

procedure TInnerForm.SetFileName(FileName: String);
begin
  FFileName := trim(FileName);
  FCaption := trim(ExtractFileName(FileName));
end;

procedure TInnerForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  DoSave: Integer;
begin
  if Memo.Modified then
  begin
    DoSave := MessageDlg('Save changes to ' + FileName + '?',mtConfirmation, mbOKCancel, 0);
    if DoSave = mrOk then
      SaveFile(FileName);
  end;
end;

procedure TInnerForm.FormCreate(Sender: TObject);
begin
  SynMemo1.KeyStrokes.ResetDefaults;
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

procedure TInnerForm.OpenFile(FileName: String);
begin
    SynMemo1.Lines.Clear;
    SynMemo1.Lines.LoadFromFile(FileName);
    SynMemo1.Modified := False;
    UpdateMaxScrollWidth;
end;

procedure TInnerForm.PrintFile;
var
  bmp: TBitmap;
  widthP, heightP: integer;
begin
  bmp:=TBitmap.Create;
  try
    if PrintDialog1.Execute then
    begin
      Printer.BeginDoc;
      widthP:=trunc(Printer.XDPI*Width / Screen.PixelsPerInch);
      heightP:=trunc(printer.YDPI*Height / Screen.PixelsPerInch);
      bmp.SetSize(Width, Height);
      SynMemo1.BorderStyle:=bsNone;
      SynMemo1.Gutter.Visible:=False;
      SynMemo1.ScrollBars:=ssNone;
      SynMemo1.PaintTo(bmp.Canvas, 0, 0);
      Printer.Canvas.StretchDraw(Rect(0, 0, widthP, heightP), bmp);
    Printer.EndDoc;
    end;
    SynMemo1.BorderStyle:=bsSingle;
    SynMemo1.Gutter.Visible:=True;
    SynMemo1.ScrollBars:=ssBoth;
  finally
    bmp.Free;
  end;
end;

procedure TInnerForm.SaveFile(FileName: String);
begin
  try
    SynMemo1.Lines.SaveToFile(FileName);
    SynMemo1.Modified := False;
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when saving the file: ' + E.Message);
  end;
end;

procedure TInnerForm.CloseFile;
begin
  try
    if Memo.Modified then
      if FileExists(FileName) then
        SaveFile(FileName);
  except
    on E:Exception do
      ShowMessage('Oh no! An error occurred when saving the file: ' + E.Message)
  end;
end;

function TInnerForm.GetNeedsSaved: Boolean;
begin
  Result := SynMemo1.Modified;
end;

end.
