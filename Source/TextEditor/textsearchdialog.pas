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

The Original Code is: dlgSearchText.pas, released 2000-06-23.

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

$Id: dlgSearchText.pas,v 1.3 2002/08/01 05:44:05 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{  modified: 04/11/2019	by: Christina Grigoryev
 * added aTIniPropStorage component to the form to save previous options
   and saved the options to a .ini file in the private directory in the
   FormCreate procedure.
}

unit textsearchdialog;

interface

uses
  LCLIntF, LCLType, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IniPropStorage, ComCtrls, MegaUtils,
  MegaPrivateFiles, SynMemo, SynEdit, SynEditSearch, SynEditTypes;

type

  { TTextSearchDialog }

  TTextSearchDialog = class(TForm)
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    cbSearchText: TComboBox;
    rgSearchDirection: TRadioGroup;
    gbSearchOptions: TGroupBox;
    cbSearchCaseSensitive: TCheckBox;
    cbSearchWholeWords: TCheckBox;
    cbSearchFromStart: TCheckBox;
    cbSearchSelectedOnly: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    Memo : TSynMemo;
    CountStatusBar: TStatusBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    function GetSearchBackwards: boolean;
    function GetSearchCaseSensitive: boolean;
    function GetSearchFromStart: boolean;
    function GetSearchInSelection: boolean;
    function GetSearchText: string;
    function GetSearchTextHistory: string;
    function GetSearchWholeWords: boolean;
    procedure SetSearchBackwards(Value: boolean);
    procedure SetSearchCaseSensitive(Value: boolean);
    procedure SetSearchFromStart(Value: boolean);
    procedure SetSearchInSelection(Value: boolean);
    procedure SetSearchText(Value: string);
    procedure SetSearchTextHistory(Value: string);
    procedure SetSearchWholeWords(Value: boolean);

  public
    property SearchBackwards: boolean read GetSearchBackwards
      write SetSearchBackwards;
    property SearchCaseSensitive: boolean read GetSearchCaseSensitive
      write SetSearchCaseSensitive;
    property SearchFromStart: boolean read GetSearchFromStart
      write SetSearchFromStart;
    property SearchInSelectionOnly: boolean read GetSearchInSelection
      write SetSearchInSelection;
    property SearchText: string read GetSearchText write SetSearchText;
    property SearchTextHistory: string read GetSearchTextHistory
      write SetSearchTextHistory;
    property SearchWholeWords: boolean read GetSearchWholeWords
      write SetSearchWholeWords;
    function BuildOptions: TSynSearchOptions; virtual;
    procedure SaveOptions(); virtual;
    procedure SearchHistoryAdd();
    procedure CountWord();
  end;

implementation

{$R *.lfm}
uses MEditorForm;

{ TTextSearchDialog }


function TTextSearchDialog.GetSearchBackwards: boolean;
begin
  Result := rgSearchDirection.ItemIndex = 1;
end;

function TTextSearchDialog.GetSearchCaseSensitive: boolean;
begin
  Result := cbSearchCaseSensitive.Checked;
end;

function TTextSearchDialog.GetSearchFromStart: boolean;
begin
  Result := cbSearchFromStart.Checked;
end;

function TTextSearchDialog.GetSearchInSelection: boolean;
begin
  Result := cbSearchSelectedOnly.Checked;
end;

function TTextSearchDialog.GetSearchText: string;
begin
  Result := cbSearchText.Text;
end;

function TTextSearchDialog.GetSearchTextHistory: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to cbSearchText.Items.Count - 1 do begin
    if i >= 10 then
      break;
    if i > 0 then
      Result := Result + #13#10;
    Result := Result + cbSearchText.Items[i];
  end;
end;

function TTextSearchDialog.GetSearchWholeWords: boolean;
begin
  Result := cbSearchWholeWords.Checked;
end;


procedure TTextSearchDialog.SetSearchBackwards(Value: boolean);
begin
  rgSearchDirection.ItemIndex := Ord(Value);
end;

procedure TTextSearchDialog.SetSearchCaseSensitive(Value: boolean);
begin
  cbSearchCaseSensitive.Checked := Value;
end;

procedure TTextSearchDialog.SetSearchFromStart(Value: boolean);
begin
  cbSearchFromStart.Checked := Value;
end;

procedure TTextSearchDialog.SetSearchInSelection(Value: boolean);
begin
  cbSearchSelectedOnly.Checked := Value;
end;

procedure TTextSearchDialog.SetSearchText(Value: string);
begin
  cbSearchText.Text := Value;
end;

procedure TTextSearchDialog.SetSearchTextHistory(Value: string);
begin
  cbSearchText.Items.Text := Value;
end;

procedure TTextSearchDialog.SetSearchWholeWords(Value: boolean);
begin
  cbSearchWholeWords.Checked := Value;
end;

procedure TTextSearchDialog.SearchHistoryAdd();
var
  s: string;
  i: integer;
begin
  s := cbSearchText.Text;
  if s <> '' then begin
    i := cbSearchText.Items.IndexOf(s);
    if i > -1 then begin
      cbSearchText.Items.Delete(i);
      cbSearchText.Items.Insert(0,s);
      cbSearchText.Text := s;
    end else
       cbSearchText.Items.Insert(0,s);
  end;
end;

{ event handlers }


procedure TTextSearchDialog.btnOKClick(Sender: TObject);
var
  Options: TSynSearchOptions;
  FoundCount: Integer;
begin
  Options := BuildOptions();
  FoundCount := Memo.SearchReplace(cbSearchText.Text, '', Options);
  if FoundCount = 0 then
  begin
    ShowMessage('Search term "' + cbSearchText.Text + '" not found.');
    CountStatusBar.SimpleText := '';
  end
  else
    CountWord();
  SearchHistoryAdd();
end;

procedure TTextSearchDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveOptions();
  CloseAction := caFree;
end;


function TTextSearchDialog.BuildOptions: TSynSearchOptions;
var
  Options: TSynSearchOptions;
begin
  Options := [];
  if SearchBackwards then
    Include(Options, ssoBackwards);
  if SearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if SearchFromStart then
    Include(Options, ssoEntireScope);
  if SearchInSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if SearchWholeWords then
    Include(Options, ssoWholeWord);
  Result := Options;
end;

procedure TTextSearchDialog.SaveOptions();
begin
  gbSearchBackwards := SearchBackwards;
  gbSearchCaseSensitive := SearchCaseSensitive;
  gbSearchFromStart:= SearchFromStart;
  gbSearchSelectionOnly:= SearchInSelectionOnly;
  gbSearchWholeWords:= SearchWholeWords;
  gsSearchText := SearchText;
  gsSearchTextHistory := SearchTextHistory;
end;

procedure TTextSearchDialog.btnCancelClick(Sender: TObject);
begin
  SaveOptions();
  Close;
end;

procedure TTextSearchDialog.CountWord();
var
  count: integer;
  place: integer;
  blockBeginHold, blockEndHold, logicCaretHold: TPoint;
begin
  blockBeginHold := Memo.BlockBegin;
  blockEndHold := Memo.BlockEnd;
  logicCaretHold := Memo.LogicalCaretXY;
  Memo.LogicalCaretXY := Point(1,1);
  count := 0;
  place := 0;
  while Memo.SearchReplace(cbSearchText.Text,'',[]) <> 0 do begin
    count := count + 1;
    if ((Memo.BlockBegin.X = blockBeginHold.X) and (Memo.BlockBegin.Y = blockBeginHold.Y)) then
      place := count;
  end;
  Memo.BlockBegin := blockBeginHold;

  CountStatusBar.SimpleText := 'Found ' + IntToStr(place) + ' of ' + IntToStr(count);
  Memo.BlockEnd := blockEndHold;
  Memo.LogicalCaretXY := logicCaretHold;

end;


procedure TTextSearchDialog.FormCreate(Sender: TObject);
begin
  IniPropStorage1.IniFileName:=GetPrivateFile(MEGASessionFile);
end;

end.

 
