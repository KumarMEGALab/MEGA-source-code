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

unit mdefine_genetic_code_dlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TDefineNewGeneticCodeDlg }

  TDefineNewGeneticCodeDlg = class(TForm)
    ComboBox1: TComboBox;
    ComboBox10: TComboBox;
    ComboBox11: TComboBox;
    ComboBox12: TComboBox;
    ComboBox13: TComboBox;
    ComboBox14: TComboBox;
    ComboBox15: TComboBox;
    ComboBox16: TComboBox;
    ComboBox17: TComboBox;
    ComboBox18: TComboBox;
    ComboBox19: TComboBox;
    ComboBox2: TComboBox;
    ComboBox20: TComboBox;
    ComboBox21: TComboBox;
    ComboBox22: TComboBox;
    ComboBox23: TComboBox;
    ComboBox24: TComboBox;
    ComboBox25: TComboBox;
    ComboBox26: TComboBox;
    ComboBox27: TComboBox;
    ComboBox28: TComboBox;
    ComboBox29: TComboBox;
    ComboBox3: TComboBox;
    ComboBox30: TComboBox;
    ComboBox31: TComboBox;
    ComboBox32: TComboBox;
    ComboBox33: TComboBox;
    ComboBox34: TComboBox;
    ComboBox35: TComboBox;
    ComboBox36: TComboBox;
    ComboBox37: TComboBox;
    ComboBox38: TComboBox;
    ComboBox39: TComboBox;
    ComboBox4: TComboBox;
    ComboBox40: TComboBox;
    ComboBox41: TComboBox;
    ComboBox42: TComboBox;
    ComboBox43: TComboBox;
    ComboBox44: TComboBox;
    ComboBox45: TComboBox;
    ComboBox46: TComboBox;
    ComboBox47: TComboBox;
    ComboBox48: TComboBox;
    ComboBox49: TComboBox;
    ComboBox5: TComboBox;
    ComboBox50: TComboBox;
    ComboBox51: TComboBox;
    ComboBox52: TComboBox;
    ComboBox53: TComboBox;
    ComboBox54: TComboBox;
    ComboBox55: TComboBox;
    ComboBox56: TComboBox;
    ComboBox57: TComboBox;
    ComboBox58: TComboBox;
    ComboBox59: TComboBox;
    ComboBox6: TComboBox;
    ComboBox60: TComboBox;
    ComboBox61: TComboBox;
    ComboBox62: TComboBox;
    ComboBox63: TComboBox;
    ComboBox64: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    HelpBtnImages: TImageList;
    BtnImages: TImageList;
    NewTableNameEdit: TEdit;
    HelpBtn: TImage;
    CancelBtn: TImage;
    OkBtn: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    procedure CancelBtnClick(Sender: TObject);
    procedure CancelBtnMouseEnter(Sender: TObject);
    procedure CancelBtnMouseLeave(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure HelpBtnMouseEnter(Sender: TObject);
    procedure HelpBtnMouseLeave(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OkBtnMouseEnter(Sender: TObject);
    procedure OkBtnMouseLeave(Sender: TObject);
    procedure DropListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DropListBoxEnter(Sender: TObject);
  private
    DropListBxArray: array [1..64] of TComboBox;
    CurrentIndex : Integer; // keeps index for updating
    StartCodeTable: AnsiString;
    procedure SetReadOnlyTableName(Value: Boolean);
    procedure SetTableName(Value: AnsiString);
    function  GetTableName:AnsiString;
    procedure SetCodeTable(const Value: AnsiString);
    function  GetCodeTable: AnsiString;
    function IndexToAminoChar(x: Integer): AnsiChar;
    function AminoCharToIndex(x: AnsiChar): Integer;
  public
    property ReadOnlyTableName: Boolean write SetReadOnlyTableName;
    property TableName: AnsiString read GetTableName write SetTableName;
    property CodeTable: AnsiString read GetCodeTable write SetCodeTable;
  end;

var
  DefineNewGeneticCodeDlg: TDefineNewGeneticCodeDlg;

implementation

uses
  LCLIntF, LCLType, mhelpfiles, mhelpkeywords, MegaUtils, ContextHelp_HC, MegaConsts;

{$R *.lfm}

{ TDefineNewGeneticCodeDlg }

procedure TDefineNewGeneticCodeDlg.OkBtnMouseEnter(Sender: TObject);
begin
  BtnImages.GetBitmap(1, OkBtn.Picture.Bitmap);
end;

procedure TDefineNewGeneticCodeDlg.CancelBtnMouseEnter(Sender: TObject);
begin
  BtnImages.GetBitmap(3, CancelBtn.Picture.Bitmap);
end;

procedure TDefineNewGeneticCodeDlg.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TDefineNewGeneticCodeDlg.CancelBtnMouseLeave(Sender: TObject);
begin
  BtnImages.GetBitmap(2, CancelBtn.Picture.Bitmap);
end;

procedure TDefineNewGeneticCodeDlg.FormCreate(Sender: TObject);
var
  i, j: Integer;
  sortedIndices: array[1..64] of Integer = (1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16,17,21,25,29,18,22,26,30,19,23,27,31,20,24,28,32,33,37,41,45,34,38,42,46,35,39,43,47,36,40,44,48,49,53,57,61,50,54,58,62,51,55,59,63,52,56,60,64);
begin
  for i:= 1 to 64 do
    DropListBxArray[i] := FindComponent('ComboBox'+IntToStr(sortedIndices[i])) as TComboBox;

  for j:= 1 to 64 do
    with DropListBxArray[j] do
    begin
      for i:=0 to 20 do  // 20 amino + 1 stop
        Items.Add(GetThreeLetterCode(IndexToAminoChar(i)));
      DropListBxArray[j].OnEnter := @DropListBoxEnter;
      DropListBxArray[j].OnKeyDown := @DropListBoxKeyDown;
    end;
  StartCodeTable := '';
  HelpContext := HC_Code_Table_Editor;
end;

procedure TDefineNewGeneticCodeDlg.FormShow(Sender: TObject);
var
  i: Integer;
begin
  if Length(StartCodeTable) < 64 then
    Exit;
  for i := 1 to 64 do
    with DropListBxArray[i] do
      ItemIndex := AminoCharToIndex(StartCodeTable[i]);
end;

procedure TDefineNewGeneticCodeDlg.HelpBtnClick(Sender: TObject);
begin
  ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
end;

procedure TDefineNewGeneticCodeDlg.HelpBtnMouseEnter(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(1, HelpBtn.Picture.Bitmap);
end;

procedure TDefineNewGeneticCodeDlg.HelpBtnMouseLeave(Sender: TObject);
begin
  HelpBtnImages.GetBitmap(0, HelpBtn.Picture.Bitmap);
end;

procedure TDefineNewGeneticCodeDlg.OkBtnClick(Sender: TObject);
var
  Temp: AnsiString;
begin
  Temp := NewTableNameEdit.Text;
  TrimNameWithAllowAlphaNum(Temp, MMaxCodeTableNameLen);

  if Length(Temp) = 0 then
  begin
    MessageDlg('Invalid Codon Table Name.  Try again!', mtError, [mbOK], 0);
    ModalResult := mrNone;
    NewTableNameEdit.Text := Temp;
  end
  else
  begin
    NewTableNameEdit.Text := Temp;
    Temp := LowerCase(Temp);
    Temp[1] := UpCase(Temp[1]);
    ModalResult := mrOK;
  end;
end;

procedure TDefineNewGeneticCodeDlg.OkBtnMouseLeave(Sender: TObject);
begin
  BtnImages.GetBitmap(0, OkBtn.Picture.Bitmap);
end;

procedure TDefineNewGeneticCodeDlg.DropListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  with Sender as TComboBox do
    case key of
      VK_ESCAPE: ItemIndex := CurrentIndex;
      VK_RETURN: ItemIndex := ItemIndex;
    else
      inherited;
    end;
end;

procedure TDefineNewGeneticCodeDlg.DropListBoxEnter(Sender: TObject);
begin
  with Sender as TComboBox do
    CurrentIndex := ItemIndex;
end;

procedure TDefineNewGeneticCodeDlg.SetReadOnlyTableName(Value: Boolean);
begin
  NewTableNameEdit.ReadOnly := Value;
end;

procedure TDefineNewGeneticCodeDlg.SetTableName(Value: AnsiString);
begin
  NewTableNameEdit.Text := Value;
end;

function TDefineNewGeneticCodeDlg.GetTableName: AnsiString;
begin
  Result := NewTableNameEdit.Text;
end;

procedure TDefineNewGeneticCodeDlg.SetCodeTable(const Value: AnsiString);
begin
  if Length(Value) < 64 then
    Exit;
  StartCodeTable := Value;
end;

function TDefineNewGeneticCodeDlg.GetCodeTable: AnsiString;
var
  i: Integer;
begin
  Result := 'FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
  for i:= 1 to 64 do
    Result[i] := IndexToAminoChar(DropListBxArray[i].ItemIndex);
end;

function TDefineNewGeneticCodeDlg.IndexToAminoChar(x: Integer): AnsiChar;
begin
  Result := '*';
  case x of
     0: Result := '*';  1: Result := 'A';  2: Result := 'R';  3: Result := 'N';
     4: Result := 'D';  5: Result := 'C';  6: Result := 'Q';  7: Result := 'E';
     8: Result := 'G';  9: Result := 'H'; 10: Result := 'I'; 11: Result := 'L';
    12: Result := 'K'; 13: Result := 'M'; 14: Result := 'F'; 15: Result := 'P';
    16: Result := 'S'; 17: Result := 'T'; 18: Result := 'W'; 19: Result := 'Y';
    20: Result := 'V';
  end;
end;

function TDefineNewGeneticCodeDlg.AminoCharToIndex(x: AnsiChar): Integer;
begin
  Result := 0;
  case x of
    '*': Result := 0; 'A': Result := 1; 'R': Result := 2; 'N': Result := 3;
    'D': Result := 4; 'C': Result := 5; 'Q': Result := 6; 'E': Result := 7;
    'G': Result := 8; 'H': Result := 9; 'I': Result :=10; 'L': Result :=11;
    'K': Result :=12; 'M': Result :=13; 'F': Result :=14; 'P': Result :=15;
    'S': Result :=16; 'T': Result :=17; 'W': Result :=18; 'Y': Result :=19;
    'V': Result :=20;
  end;
end;

end.

