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

unit mdata_collection_settings_form;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ActnList;

const
  YES_BUTTON = 0;
  NO_BUTTON = 1;

type

  { TDataCollectionSettingsForm }

  TDataCollectionSettingsForm = class(TForm)
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    PromptForUserTypeLabel: TLabel;
    ResearcherBtn: TRadioButton;
    StudentBtn: TRadioButton;
    InstructorBtn: TRadioButton;
    ProfessorBtn: TRadioButton;
    OtherBtn: TRadioButton;
    SaveAction: TAction;
    ActionList1: TActionList;
    Panel1: TPanel;
    Panel2: TPanel;
    AllowCollectionRadioGrp: TRadioGroup;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure AllowCollectionRadioGrpClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    FIgnoreEvents: Boolean;
    procedure UpdateServerWithPrefs(dataCollectionAllowed: Boolean);
    function UserTypeSelected: Boolean;
    procedure SetUserTypesEnabled(Value: Boolean);
    function UserTypeString: String;
  public
    procedure SetCurrentOptions(dataCollectionAllowed: Boolean; userType: String);
    procedure SetUserType(aType: String);
  end;

var
  DataCollectionSettingsForm: TDataCollectionSettingsForm;

const
  YES_BTN = 0;
  NO_BTN = 1;

implementation

{$R *.lfm}

uses
  MegaMainPreferences, MegaConsts, MUsageStatistics, MegaUtils, mimageform, MegaVerConsts;

{ TDataCollectionSettingsForm }

procedure TDataCollectionSettingsForm.AllowCollectionRadioGrpClick(Sender: TObject);
begin
  if FIgnoreEvents then Exit;
  if AllowCollectionRadioGrp.ItemIndex = YES_BTN then
  begin
    SetUserTypesEnabled(True);
  end
  else
  begin
    SetUserTypesEnabled(False);
  end;
end;

procedure TDataCollectionSettingsForm.FormActivate(Sender: TObject);
begin
  ToolBar1.Images := ImageForm.GetDialogButtonImageList;
  ToolBar1.HotImages := ImageForm.GetDailogButtonHoverImageList;
  ToolBar1.ImagesWidth := ToolBar1.ButtonWidth;
  Constraints.MinWidth := ToolBar1.Width + 20;
  if Panel2.Width > ToolBar1.Width then
    ToolBar1.Left := Round((Panel2.Width - ToolBar1.Width)/2);
end;

procedure TDataCollectionSettingsForm.FormCreate(Sender: TObject);
begin
  FIgnoreEvents := False;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Data Collection Settings';
end;

procedure TDataCollectionSettingsForm.SaveActionExecute(Sender: TObject);
var
  dataCollectionIsAllowed: Boolean = False;
  temp: String = '';
begin
  dataCollectionIsAllowed := (AllowCollectionRadioGrp.ItemIndex = YES_BUTTON);
  if dataCollectionIsAllowed and (not UserTypeSelected) then
  begin
    ShowMessage('Please select a user type or disable data collection');
    Exit;
  end;

  try
    SaveDataCollectPrefs(dataCollectionIsAllowed);
    temp := UserTypeString;
    if temp.StartsWith('Other') then
      USER_TYPE := temp.Substring(temp.IndexOf('=') + 1)
    else
      USER_TYPE := temp;
    SaveUserType(UserTypeString);
    { Update the preferences main menu before updating the server!}
    UpdatePreferencesMainMenu(UserPref_AllowCollectUsageData, dataCollectionIsAllowed, BoolToStr(dataCollectionIsAllowed, True));
    UpdateServerWithPrefs(dataCollectionIsAllowed);
  except
    on E:Exception do
      ShowMessage('Application error when saving user preferences: ' + E.Message);
  end;
  ModalResult := mrOk;
end;

procedure TDataCollectionSettingsForm.UpdateServerWithPrefs(dataCollectionAllowed: Boolean);
var
  prefsChanged: Boolean = False;
begin

  if dataCollectionAllowed and FileExists(GetMegaGlobalFile('') + 'optedOut.txt') then
  begin
    DeleteFile(GetMegaGlobalFile('') + 'optedOut.txt');
    prefsChanged := True;
  end;

  if (not dataCollectionAllowed) and FileExists(GetMegaGlobalFile('') + 'optedIn.txt') then
  begin
    DeleteFile(GetMegaGlobalFile('') + 'optedIn.txt');
    prefsChanged := True;
  end;

  if (not FileExists(GetMegaGlobalFile('') + 'optedOut.txt')) and (not FileExists(GetMegaGlobalFile('') + 'optedIn.txt')) then
    prefsChanged := True;

  if prefsChanged then
  begin
    if FileExists(GetMegaGlobalFile('') + 'prefsSaved.txt') then
      DeleteFile(GetMegaGlobalFile('') + 'prefsSaved.txt');
    UploadUserPref;
  end;
end;

function TDataCollectionSettingsForm.UserTypeSelected: Boolean;
begin
  Result := False;
  if ResearcherBtn.Checked then
    Result := True
  else if StudentBtn.Checked then
    Result := True
  else if InstructorBtn.Checked then
    Result := True
  else if ProfessorBtn.Checked then
    Result := True
  else if OtherBtn.Checked then
    Result := True;
end;

procedure TDataCollectionSettingsForm.SetUserTypesEnabled(Value: Boolean);
begin
  ResearcherBtn.Enabled := Value;
  StudentBtn.Enabled := Value;
  InstructorBtn.Enabled := Value;
  ProfessorBtn.Enabled := Value;
  OtherBtn.Enabled := Value;
  Edit1.Enabled := Value;
  PromptForUserTypeLabel.Enabled := Value;
  GroupBox1.Enabled := Value;
end;

function TDataCollectionSettingsForm.UserTypeString: String;
begin
  {$IFDEF DEBUG}
  Result := DEVELOPER_STR;
  Exit;
  {$ENDIF}
  Result := EmptyStr;

  if AllowCollectionRadioGrp.ItemIndex = NO_BUTTON then
  begin
    Result := 'OTHER_UNDEFINED';
    Exit;
  end;

  if ResearcherBtn.Checked then
    Result := RESEARCHER_STR
  else if StudentBtn.Checked then
    Result := STUDENT_STR
  else if InstructorBtn.Checked then
    Result := INSTRUCTOR_STR
  else if ProfessorBtn.Checked then
    Result := PROFESSOR_STR
  else if OtherBtn.Checked then
  begin
    if Trim(Edit1.Text) <> EmptyStr then
      Result := 'Other=' + Trim(Edit1.Text)
    else
      Result := 'OTHER_UNDEFINED';
  end
  else
    Result := 'OTHER_UNDEFINED';
end;

procedure TDataCollectionSettingsForm.SetCurrentOptions(dataCollectionAllowed: Boolean; userType: String);
begin
  try
    FIgnoreEvents := True;
    if dataCollectionAllowed then
      AllowCollectionRadioGrp.ItemIndex := YES_BUTTON
    else
      AllowCollectionRadioGrp.ItemIndex := NO_BUTTON;

    Edit1.Text := EmptyStr;
    if userType = DEVELOPER_STR then
    begin
      OtherBtn.Checked := True;
      Edit1.Text := DEVELOPER_STR;
    end
    else if userType = STUDENT_STR then
      StudentBtn.Checked := True
    else if userType = RESEARCHER_STR then
      ResearcherBtn.Checked := True
    else if userType = INSTRUCTOR_STR then
      InstructorBtn.Checked := True
    else if userType = PROFESSOR_STR then
      ProfessorBtn.Checked := True
    else
    begin
      OtherBtn.Checked := True;
      Edit1.Text := userType;
    end;
    SetUserTypesEnabled(dataCollectionAllowed);
  finally
    FIgnoreEvents := False;
  end;
end;

procedure TDataCollectionSettingsForm.SetUserType(aType: String);
begin
  try
    FIgnoreEvents := True;

    if aType = RESEARCHER_STR then
      ResearcherBtn.Checked := True
    else if aType = STUDENT_STR then
      StudentBtn.Checked := True
    else if aType = INSTRUCTOR_STR then
      InstructorBtn.Checked := True
    else if aType = PROFESSOR_STR then
      ProfessorBtn.Checked := True
    else
    begin
      OtherBtn.Checked := True;
      if aType <> EmptyStr then
        Edit1.Text := aType;
    end
  finally
    FIgnoreEvents := False;
  end;
end;

end.

