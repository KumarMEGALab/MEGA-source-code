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

unit MSeqNameOptionDlg;

interface

uses
  LCLType, LCLIntF, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,
  MD_Sequences;

type
  TSeqNameOptionDlgForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Panel2: TPanel;
    UseInitialPnl: TPanel;
    UseInitialCkBx: TCheckBox;
    GroupBox1: TGroupBox;
    SeqNameEdit: TEdit;
    GroupBox2: TGroupBox;
    InfoMemo: TMemo;
    FirstCBx: TComboBox;
    SecondCBx: TComboBox;
    ThirdCBx: TComboBox;
    FourthCBx: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormatOnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SeqNameEditChange(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SeqNameCBxChange(Sender: TObject);

  private
    { Private declarations }

    sl0,sl1: TStringList;
    CurIndex: array[0..3] of integer;

    FSequence: TSequence;
    FSequenceList: TSequenceList;

    procedure SetSequence(ASequence: TSequence);

    procedure ResetMenuItems;
    procedure ResetSeqNameEdit;
    procedure SetSeqNameEdit;

    function CheckSeqName: boolean;
  protected
    //procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }

    property Sequence: TSequence read FSequence write SetSequence;
    property SequenceList: TSequenceList read FSequenceList write FSequenceList;

    function SetSeqName: boolean;

  end;

var
  SeqNameOptionDlgForm: TSeqNameOptionDlgForm;

implementation

{$R *.lfm}

uses
  MAlignEditMainForm, MAlignGrid,
  //MegaIniStore,
  MegaVerConsts;

//procedure TSeqNameOptionDlgForm.CreateParams(var Params: TCreateParams);
//begin
//  inherited;
//  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
//end;

procedure TSeqNameOptionDlgForm.ResetMenuItems;

  function GetItemIndex(item: string): integer;
  var
    i: integer;
  begin
    result := 7;
    for i := 0 to FirstCBx.Items.Count-1 do
      if FirstCBx.Items[i] = item then
      begin
        result := i;
        break;
      end;
  end;

var
  i: integer;
  str: string;
begin
  sl0.Clear;
  with Sequence do
  begin
    UseInitialCkBx.Enabled := (length(species) > 1) and (Pos(' ', species) > 0);
    if UseInitialCkBx.Enabled and UseInitialCkBx.Checked then
      str := species[1]+'.' +Copy(species, Pos(' ', species), length(species))
    else
      str := species;
    sl0.Add(str);
    sl0.Add(subspecies);
    sl0.Add(strain);
    sl0.Add(host);
    sl0.Add(senotype);
    sl0.Add(gene);
    sl0.Add(allele);
    sl0.Add('gi'+UIDS);
  end;

  sl1.Clear;
  for i := 0 to 7 do
  begin
    if sl0[i] = '' then continue;
    sl1.Add(sl0[i]);
  end;

  FirstCBx.Clear;
  FirstCBx.Items.Assign(sl1);
  FirstCBx.Items.Add('');
  SecondCBx.Clear;
  SecondCBx.Items.Assign(sl1);
  SecondCBx.Items.Add('');
  ThirdCBx.Clear;
  ThirdCBx.Items.Assign(sl1);
  ThirdCBx.Items.Add('');
  FourthCBx.Clear;
  FourthCBx.Items.Assign(sl1);
  FourthCBx.Items.Add('');

  if (CurIndex[0] = 8) or (sl0[CurIndex[0]] = '') then
    FirstCBx.ItemIndex := FirstCBx.Items.Count-1
  else
    FirstCBx.ItemIndex := GetItemIndex(sl0[CurIndex[0]]);
  if (CurIndex[1] = 8) or (sl0[CurIndex[1]] = '') then
    SecondCBx.ItemIndex := SecondCBx.Items.Count-1
  else
    SecondCBx.ItemIndex := GetItemIndex(sl0[CurIndex[1]]);
  if (CurIndex[2] = 8) or (sl0[CurIndex[2]] = '') then
    ThirdCBx.ItemIndex := ThirdCBx.Items.Count-1
  else
    ThirdCBx.ItemIndex := GetItemIndex(sl0[CurIndex[2]]);
  if (CurIndex[3] = 8) or (sl0[CurIndex[3]] = '') then
    FourthCBx.ItemIndex := FourthCBx.Items.Count-1
  else
    FourthCBx.ItemIndex := GetItemIndex(sl0[CurIndex[3]]);

  if Sequence.SeqInfo <> '' then
    InfoMemo.Text :=  Sequence.SeqInfo
  else
    InfoMemo.Text := Sequence.SeqName;

  FormatOnClick(Self);
end;

procedure TSeqNameOptionDlgForm.ResetSeqNameEdit;
var
  str: string;
begin
  str := '';
  if FirstCBx.Items[FirstCBx.ItemIndex] <> '' then
    if str = '' then
      str := FirstCBx.Items[FirstCBx.ItemIndex]
    else
      str := str +' ' +FirstCBx.Items[FirstCBx.ItemIndex];
  if SecondCBx.Items[SecondCBx.ItemIndex] <> '' then
    if str = '' then
       str := SecondCBx.Items[SecondCBx.ItemIndex]
    else
      str := str +' ' +SecondCBx.Items[SecondCBx.ItemIndex];
  if ThirdCBx.Items[ThirdCBx.ItemIndex] <> '' then
    if str = '' then
       str := ThirdCBx.Items[ThirdCBx.ItemIndex]
    else
      str := str +' ' +ThirdCBx.Items[ThirdCBx.ItemIndex];
  if FourthCBx.Items[FourthCBx.ItemIndex] <> '' then
    if str = '' then
       str := FourthCBx.Items[FourthCBx.ItemIndex]
    else
      str := str +' ' +FourthCBx.Items[FourthCBx.ItemIndex];

  if str = '' then
    SeqNameEdit.Text := Sequence.SeqName
  else
    SeqNameEdit.Text := str;
end;

procedure TSeqNameOptionDlgForm.SetSequence(ASequence: TSequence);
begin
  if ASequence = nil then exit;
  FSequence := ASequence;
  SetSeqNameEdit;
end;

procedure TSeqNameOptionDlgForm.SetSeqNameEdit;
begin
  ResetMenuItems;
  ResetSeqNameEdit;
end;

procedure TSeqNameOptionDlgForm.FormatOnClick(Sender: TObject);
var
  str: string;
  i: array[0..3] of integer;
begin
  if FirstCBx.Items.Count = 0 then exit;

  i[0] := FirstCBx.ItemIndex;
  i[1] := SecondCBx.ItemIndex;
  i[2] := ThirdCBx.ItemIndex;
  i[3] := FourthCBx.ItemIndex;

  SeqNameEdit.Text := '';
  with Sequence do
    if UseInitialCkBx.Checked  then
      if (length(species) > 1) and (Pos(' ', species) > 0) then
        str := species[1]+'.' +Copy(species, Pos(' ', species), length(species))
      else
        str := species
    else
      str := species;

  FirstCBx.Items[0]  := str;
  SecondCBx.Items[0] := str;
  ThirdCBx.Items[0]  := str;
  FourthCBx.Items[0] := str;

  FirstCBx.ItemIndex  := i[0];
  SecondCBx.ItemIndex := i[1];
  ThirdCBx.ItemIndex  := i[2];
  FourthCBx.ItemIndex := i[3];

  ResetSeqNameEdit;
end;

procedure TSeqNameOptionDlgForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  //OvcFormState1.Storage := GetMegaIniFileStore;
  //OvcComponentState1.Storage := GetMegaIniFileStore;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Input Sequence Label';

  sl0 := TStringList.Create;
  sl1 := TStringList.Create;

  for i := 0 to 3 do
    CurIndex[i] := 8;
end;

procedure TSeqNameOptionDlgForm.FormDestroy(Sender: TObject);
begin
  sl0.Free;
  sl1.Free;
end;

procedure TSeqNameOptionDlgForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);

  function GetCurIndex(ItemIndex: integer): integer;
  var
    i,j: integer;
  begin
    result := 8;
    j := -1;
    for i := 0 to 7 do
    begin
      if sl0[i] = '' then
        continue;
      inc(j);
      if j = ItemIndex then
      begin
        result := i;
        break;
      end;
    end;
  end;

begin
  if ModalResult = mrCancel then exit;

  CurIndex[0] := GetCurIndex(FirstCBx.ItemIndex);
  CurIndex[1] := GetCurIndex(SecondCBx.ItemIndex);
  CurIndex[2] := GetCurIndex(ThirdCBx.ItemIndex);
  CurIndex[3] := GetCurIndex(FourthCBx.ItemIndex);
  CloseAction := caHide;
end;

procedure TSeqNameOptionDlgForm.SeqNameEditChange(Sender: TObject);
var
  str:string;
begin
  str := trim(SeqNameEdit.Text);
  if str = '' then
    OKBtn.Enabled := false
  else
    OKBtn.Enabled := true;
end;

function TSeqNameOptionDlgForm.CheckSeqName: boolean;
var
  i: integer;
  str: string;
begin
  result := true;
  str := trim(SeqNameEdit.Text);
  if SequenceList <> nil then
    for i := 0 to SequenceList.Count-1 do
      if str = SequenceList[i].SeqName then
      begin
        result := false;
        break;
      end;

  if result and (AlignEditMainForm <> nil) then
    for i := 0 to AlignEditMainForm.AlignGrid1.NoOfSeqs-1 do
      if str = AlignEditMainForm.AlignGrid1.Sequence[i].SeqName then
      begin
        result := false;
        break;
      end;
end;

function TSeqNameOptionDlgForm.SetSeqName: boolean;
begin
  result := CheckSeqName;
  if result then
    Sequence.SeqName := SeqNameEdit.Text;
end;

procedure TSeqNameOptionDlgForm.OKBtnClick(Sender: TObject);
var
  i: integer;
  flag: boolean;
begin
  flag := CheckSeqName;
  if flag then
    ModalResult := mrOK
  else
  begin
    i := MessageDlg('The specified sequence name already exists.', mtWarning, [mbRetry, mbCancel, mbIgnore], 0);

    if i = mrIgnore then
    begin
      SeqNameEdit.Text := Sequence.SeqName;
      ModalResult := mrOK;
    end
    else if i = mrCancel then
      ModalResult := mrCancel;
  end;
end;

procedure TSeqNameOptionDlgForm.SeqNameCBxChange(Sender: TObject);
begin
  SeqNameEdit.Text := '';
  ResetSeqNameEdit;
end;

end.
