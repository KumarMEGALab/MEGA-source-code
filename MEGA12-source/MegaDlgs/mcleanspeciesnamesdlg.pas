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

unit MCleanSpeciesNamesDlg;

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList, ImgList, StdCtrls, IniPropStorage,
  MegaUtils, MegaPrivateFiles;

type

  { TCleanSpNamesDlg }

  TCleanSpNamesDlg = class(TForm)
    ActionList1: TActionList;
    ImageList1: TImageList;
    ApplyAction: TAction;
    DoneAction: TAction;
    Button1: TButton;
    Button2: TButton;
    IniPropStorage1: TIniPropStorage;
    StripNumericTokensCheckbox: TCheckBox;
    StripNonAlphaNumericCheckBox: TCheckBox;
    StripTextCheckBox: TCheckBox;
    StripByRegexCheckBox: TCheckBox;
    StripByTextEdit: TEdit;
    StripByRegexEdit: TEdit;
    aLbl: TLabel;
    Label1: TLabel;
    RevertAction: TAction;
    Button3: TButton;
    MaxTokensCheckbox: TCheckBox;
    MaxTokensEdit: TEdit;
    Label2: TLabel;
    procedure ApplyActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure RevertActionExecute(Sender: TObject);
    procedure DoneActionExecute(Sender: TObject);
    procedure Button2Click(Sender: TObject);
//    procedure Button2Click(Sender: TObject);
  private
    function ValidateForm: Boolean;
    function GetIsNonAlphaNumericFilter: Boolean;
    function GetIsNumberFilter: Boolean;
    function GetIsRegexFilter: Boolean;
    function GetIsTextFilter: Boolean;
    function GetRegexString: String;
    function GetTextString: String;
    function GetIsMaxTokensFilter: Boolean;
    function GetMaxTokens: Integer;

    { Private declarations }
  public
    { Public declarations }
    ApplyChangesNotify: TNotifyEvent;
    RevertChangesNotify: TNotifyEvent;
    DoneNotify: TNotifyEvent;
    property TextString: String read GetTextString;
    property RegexString: String read GetRegexString;
    property IsNumberFilter: Boolean read GetIsNumberFilter;
    property IsNonAlphaNumericFilter: Boolean read GetIsNonAlphaNumericFilter;
    property IsTextFilter: Boolean read GetIsTextFilter;
    property IsRegexFilter: Boolean read GetIsRegexFilter;
    property IsMaxTokensFilter: Boolean read GetIsMaxTokensFilter;
    property MaxTokens: Integer read GetMaxTokens;
  end;

var
  CleanSpNamesDlg: TCleanSpNamesDlg;

implementation

{$R *.lfm}

procedure TCleanSpNamesDlg.ApplyActionExecute(Sender: TObject);
begin
  if not ValidateForm then
    Exit;
  Assert(Assigned(ApplyChangesNotify));
  ApplyChangesNotify(Self);
end;

procedure TCleanSpNamesDlg.Button2Click(Sender: TObject);
begin
  DoneActionExecute(Self);
end;

procedure TCleanSpNamesDlg.DoneActionExecute(Sender: TObject);
begin
  if Assigned(DoneNotify) then
    DoneNotify(Self);
end;

procedure TCleanSpNamesDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DoneActionExecute(Self);
end;

procedure TCleanSpNamesDlg.FormCreate(Sender: TObject);
begin
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

function TCleanSpNamesDlg.GetIsMaxTokensFilter: Boolean;
begin
  Result := MaxTokensCheckbox.Checked;
end;

function TCleanSpNamesDlg.GetIsNonAlphaNumericFilter: Boolean;
begin
  Result := StripNonAlphaNumericCheckBox.Checked;
end;

function TCleanSpNamesDlg.GetIsNumberFilter: Boolean;
begin
  Result := StripNumericTokensCheckbox.Checked;
end;

function TCleanSpNamesDlg.GetIsRegexFilter: Boolean;
begin
  Result := StripByRegexCheckBox.Checked;
end;

function TCleanSpNamesDlg.GetIsTextFilter: Boolean;
begin
  Result := StripTextCheckBox.Checked;
end;

function TCleanSpNamesDlg.GetMaxTokens: Integer;
begin
  Result := StrToInt(MaxTokensEdit.Text);
end;

function TCleanSpNamesDlg.GetRegexString: String;
begin
  Result := Trim(StripByRegexEdit.Text);
end;

function TCleanSpNamesDlg.GetTextString: String;
begin
  Result := Trim(StripByTextEdit.Text);
end;

procedure TCleanSpNamesDlg.RevertActionExecute(Sender: TObject);
begin
  Assert(Assigned(RevertChangesNotify));
  RevertChangesNotify(Self);
end;

function TCleanSpNamesDlg.ValidateForm: Boolean;
var
  TempInt: Integer;
begin
  Result := False;
  if StripTextCheckBox.Checked and (Trim(StripByTextEdit.Text) = EmptyStr) then
  begin
    ShowMessage('To use the text option, please provide some text');
    StripByTextEdit.SelectAll;
    StripByTextEdit.SetFocus;
    Exit;
  end;

  if StripByRegexCheckBox.Checked and (Trim(StripByRegexEdit.Text) = EmptyStr) then
  begin
    ShowMessage('To use the regex option, please provide a regular expression');
    StripByRegexEdit.SelectAll;
    StripByRegexEdit.SetFocus;
    Exit;
  end;

  if MaxTokensCheckbox.Checked then
  begin
    if Trim(MaxTokensEdit.Text) = EmptyStr then
    begin
      ShowMessage('To use the max tokens option, please provide a positive integer value');
      MaxTokensEdit.SelectAll;
      MaxTokensEdit.SetFocus;
      Exit;
    end
    else
    begin
      if (not TryStrToInt(MaxTokensEdit.Text, TempInt)) or (TempInt <= 0) then
      begin
        ShowMessage('To use the max tokens option, please provide a positive integer value');
        MaxTokensEdit.SelectAll;
        MaxTokensEdit.SetFocus;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

end.
