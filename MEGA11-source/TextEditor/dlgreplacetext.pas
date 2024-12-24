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

{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dlgReplaceText.pas, released 2000-06-23.

The Original Code is part of the SearchReplaceDemo project, written by
Michael Hieke for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: dlgReplaceText.pas,v 1.2 2000/11/22 08:37:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit dlgReplaceText;

//{$I SynEdit.inc}

interface

uses
  LCLIntF, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  textsearchdialog, StdCtrls, ExtCtrls, SynEdit, SynEditSearch,
  SynEditTypes;

type

  { TTextReplaceDialog }

  TTextReplaceDialog = class(TTextSearchDialog)
    cbReplaceAll: TCheckBox;
    Label2: TLabel;
    cbReplaceText: TComboBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    function GetReplaceText: string;
    function GetReplaceTextHistory: string;
    function GetReplaceAll: boolean;
    procedure SetReplaceText(Value: string);
    procedure SetReplaceTextHistory(Value: string);
    procedure SetReplaceAll(Value: boolean);
    procedure ReplaceHistoryAdd();
  public
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property ReplaceTextHistory: string read GetReplaceTextHistory
      write SetReplaceTextHistory;
    property ReplaceAll: boolean read GetReplaceAll write SetReplaceAll;
    function BuildOptions: TSynSearchOptions; override;
    procedure SaveOptions(); override;
  end;

implementation

{$R *.lfm}
uses MEditorForm;
{ TTextReplaceDialog }

function TTextReplaceDialog.GetReplaceText: string;
begin
  Result := cbReplaceText.Text;
end;

function TTextReplaceDialog.GetReplaceAll: boolean;
begin
  Result := cbReplaceAll.Checked;
end;

function TTextReplaceDialog.GetReplaceTextHistory: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to cbReplaceText.Items.Count - 1 do begin
    if i >= 10 then
      break;
    if i > 0 then
      Result := Result + #13#10;
    Result := Result + cbReplaceText.Items[i];
  end;
end;

procedure TTextReplaceDialog.SetReplaceText(Value: string);
begin
  cbReplaceText.Text := Value;
end;

procedure TTextReplaceDialog.SetReplaceTextHistory(Value: string);
begin
  cbReplaceText.Items.Text := Value;
end;

procedure TTextReplaceDialog.SetReplaceAll(Value: boolean);
begin
  cbReplaceAll.Checked := Value;
end;

procedure TTextReplaceDialog.ReplaceHistoryAdd();
var
  s: string;
  i: integer;
begin
  s := cbReplaceText.Text;
  if s <> '' then begin
    i := cbReplaceText.Items.IndexOf(s);
    if i > -1 then begin
      cbReplaceText.Items.Delete(i);
      cbReplaceText.Items.Insert(0,s);
      cbReplaceText.Text := s;
    end else
      cbReplaceText.Items.Insert(0,s);
  end;
end;

function TTextReplaceDialog.BuildOptions: TSynSearchOptions;
var
  Options: TSynSearchOptions;
begin;
  Options := inherited;
  Include(Options, ssoReplace);
  if ReplaceAll then
    Include(Options, ssoReplaceAll);
    Include(Options,ssoEntireScope);
  Result := Options;
end;

procedure TTextReplaceDialog.SaveOptions();
begin
  inherited;
  gsReplaceText := ReplaceText;
  gsReplaceTextHistory := ReplaceTextHistory;
  gbReplaceAll := ReplaceAll;
end;

procedure TTextReplaceDialog.btnOKClick(Sender: TObject);
var
  Options: TSynSearchOptions;
  FoundCount: Integer;
  msg: String;
begin
  Options := BuildOptions;
  FoundCount := Memo.SearchReplace(cbSearchText.Text,cbReplaceText.Text,Options);
  if FoundCount = 0 then
    ShowMessage('Search term "' + cbSearchText.Text + '" not found')
  else
  begin
    if FoundCount = 1 then
    begin
      CountStatusBar.SimpleText := EmptyStr;
      msg := '1 instance of "' + cbSearchText.Text + '" replaced with "' + cbReplaceText.Text + '"'
    end
    else
      msg := (IntToStr(FoundCount) + ' instances of "' + cbSearchText.Text + '" replaced with "' + cbReplaceText.Text + '"');
    CountStatusBar.SimpleText := msg;
  end;
  SearchHistoryAdd();
  ReplaceHistoryAdd();
end;

procedure TTextReplaceDialog.btnCancelClick(Sender: TObject);
begin
  SaveOptions();
  Close;
end;

procedure TTextReplaceDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveOptions();
  CloseAction := caFree;
end;



end.

 
