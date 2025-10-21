{
	Copyright 1992-2025 Sudhir Kumar and Koichiro Tamura

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
  ComCtrls, ActnList, jsonparser, fpjson;

const
  YES_BUTTON = 0;
  NO_BUTTON = 1;

type

  { TDataCollectionSettingsForm }

  TDataCollectionSettingsForm = class(TForm)
    BachelorsStudentCb: TCheckBox;
    DeclineReportingCb: TCheckBox;
    DisabilityCb: TCheckBox;
    DoctoralStudentCb: TCheckBox;
    FirstGenerationCb: TCheckBox;
    GroupBox1: TGroupBox;
    Instructor12Cb: TCheckBox;
    InstructorCollegeCb: TCheckBox;
    InUnitedStatesCb: TCheckBox;
    Label1: TLabel;
    LowIncomeCb: TCheckBox;
    MastersStudentCb: TCheckBox;
    Panel3: TPanel;
    PostdocCb: TCheckBox;
    PromptForUserTypeLabel: TLabel;
    ResearcherAcademiaCb: TCheckBox;
    ResearcherGovernmentCb: TCheckBox;
    ResearcherIndustryCb: TCheckBox;
    SaveAction: TAction;
    ActionList1: TActionList;
    Panel1: TPanel;
    Panel2: TPanel;
    AllowCollectionRadioGrp: TRadioGroup;
    SchoolStudentCb: TCheckBox;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    UnderRepresentedCb: TCheckBox;
    procedure AllowCollectionRadioGrpClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    FErrorMsg: String;
    FIgnoreEvents: Boolean;
    procedure UpdateServerWithPrefs(dataCollectionAllowed: Boolean);
    //procedure SetFeedbackEnabled(Value: Boolean); { removed in version 12.0.6 per Sudhir in order to force users to select atleast one item}
    function UserTypeString: String;
    function GetFeedbackJson: TJSONObject;
    procedure LoadSavedFeedback;
    function LoadFeedbackFromJson(jsonStr: String): Boolean;
    function SaveFeedbackJson: Boolean;
    function SelectionIsMade: Boolean;
  public
    procedure SetCurrentOptions(dataCollectionAllowed: Boolean);
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
  //if AllowCollectionRadioGrp.ItemIndex = YES_BTN then
  //begin
  //  SetFeedbackEnabled(True);
  //end
  //else
  //begin
  //  SetFeedbackEnabled(False);
  //end;
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
  FErrorMsg := EmptyStr;
  Caption := VER_MEGA_WIN_CAPTION_PREFIX + ': Data Collection Settings';
end;

procedure TDataCollectionSettingsForm.SaveActionExecute(Sender: TObject);
var
  dataCollectionIsAllowed: Boolean = False;
  temp: String = '';
begin
  if not SelectionIsMade then
  begin
    ShowMessage('Please select at least 1 "Demographics" item');
    Exit;
  end;
  dataCollectionIsAllowed := (AllowCollectionRadioGrp.ItemIndex = YES_BUTTON);

  try
    SaveDataCollectPrefs(dataCollectionIsAllowed);
    temp := UserTypeString;
    SaveUserType(UserTypeString);
    { Update the preferences main menu before updating the server!}
    UpdatePreferencesMainMenu(UserPref_AllowCollectUsageData, dataCollectionIsAllowed, BoolToStr(dataCollectionIsAllowed, True));
    UpdateServerWithPrefs(dataCollectionIsAllowed);
    SaveFeedbackJson;
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

//procedure TDataCollectionSettingsForm.SetFeedbackEnabled(Value: Boolean);
//begin
//  ResearcherAcademiaCb.Enabled := Value;
//  ResearcherIndustryCb.Enabled := Value;
//  ResearcherGovernmentCb.Enabled := Value;
//  Instructor12Cb.Enabled := Value;
//  InstructorCollegeCb.Enabled := Value;
//  DoctoralStudentCb.Enabled := Value;
//  PostdocCb.Enabled := Value;
//  MastersStudentCb.Enabled := Value;
//  BachelorsStudentCb.Enabled := Value;
//  SchoolStudentCb.Enabled := Value;
//  InUnitedStatesCb.Enabled := Value;
//  UnderRepresentedCb.Enabled := Value;
//  FirstGenerationCb.Enabled := Value;
//  DisabilityCb.Enabled := Value;
//  LowIncomeCb.Enabled := Value;
//  DeclineReportingCb.Enabled := Value;
//end;

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

  if ResearcherAcademiaCb.Checked or ResearcherIndustryCb.Checked or ResearcherGovernmentCb.Checked or PostdocCb.Checked then
    Result := RESEARCHER_STR
  else if DoctoralStudentCb.Checked or MastersStudentCb.Checked or BachelorsStudentCb.Checked or SchoolStudentCb.Checked then
    Result := STUDENT_STR
  else if InstructorCollegeCb.Checked or Instructor12Cb.Checked then
    Result := INSTRUCTOR_STR
  else
    Result := 'OTHER_UNDEFINED';
end;

procedure TDataCollectionSettingsForm.LoadSavedFeedback;
var
  aList: TStringList = nil;
begin
  try
    try
      if FileExists(GetMegaGlobalFile('') + DEMOGRAPHICS_FILE) then
      begin
        aList := TStringList.Create;
        aList.LoadFromFile(GetMegaGlobalFile('') + DEMOGRAPHICS_FILE);
        LoadFeedbackFromJson(aList.Text);
      end;
    except
      on E:Exception do
      begin
        FErrorMsg := E.Message;
      end;
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TDataCollectionSettingsForm.SaveFeedbackJson: Boolean;
var
  aFile: TextFile;
  filename: String = '';
  json: TJSONObject = nil;
begin
  Result := True;
  try
    try
      filename := GetMegaGlobalFile('') + DEMOGRAPHICS_FILE;
      AssignFile(aFile, filename);
      Rewrite(aFile);
      json := GetFeedbackJson;
      WriteLn(aFile, json.AsJSON);
      Result := FileExists(filename);
      UploadDemographics(json.AsJSON);
    except
      on E:Exception do
      begin
        FErrorMsg := E.Message;
        Result := False;
      end;
    end;
  finally
    CloseFile(aFile);
    if Assigned(json) then
      json.Free;
  end;
end;

function TDataCollectionSettingsForm.SelectionIsMade: Boolean;
begin
  Result := False;
  if ResearcherAcademiaCb.Checked then
    Exit(True);
  if ResearcherIndustryCb.Checked then
    Exit(True);
  if ResearcherGovernmentCb.Checked then
    Exit(True);
  if PostdocCb.Checked then
    Exit(True);
  if Instructor12Cb.Checked then
    Exit(True);
  if InstructorCollegeCb.Checked then
    Exit(True);
  if DoctoralStudentCb.Checked then
    Exit(True);
  if MastersStudentCb.Checked then
    Exit(True);
  if BachelorsStudentCb.Checked then
    Exit(True);
  if SchoolStudentCb.Checked then
    Exit(True);
  if InUnitedStatesCb.Checked then
    Exit(True);
  if UnderRepresentedCb.Checked then
    Exit(True);
  if FirstGenerationCb.Checked then
    Exit(True);
  if DisabilityCb.Checked then
    Exit(True);
  if LowIncomeCb.Checked then
    Exit(True);
  if DeclineReportingCb.Checked then
    Exit(True);
end;

function TDataCollectionSettingsForm.GetFeedbackJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('guid', GetGuidString);
  Result.Add(RESEARCHER_ACADEMIA_STR, ResearcherAcademiaCb.Checked);
  Result.Add(RESEARCHER_INDUSTRY_STR, ResearcherIndustryCb.Checked);
  Result.Add(RESEARCHER_GOVERNMENT_STR, ResearcherGovernmentCb.Checked);
  Result.Add(POSTDOC_STR, PostdocCb.Checked);
  Result.Add(INSTRUCTOR_12_STR, Instructor12Cb.Checked);
  Result.Add(INSTRUCTOR_COLLEGE, InstructorCollegeCb.Checked);
  Result.Add(DOCTORAL_STUDENT_STR, DoctoralStudentCb.Checked);
  Result.Add(MASTERS_STUDENT_STR, MastersStudentCb.Checked);
  Result.Add(BACHELORS_STUDENT_STR, BachelorsStudentCb.Checked);
  Result.Add(SCHOOL_STUDENT_STR, SchoolStudentCb.Checked);
  Result.Add(IN_US_STR, InUnitedStatesCb.Checked);
  Result.Add(UNDER_REPRESENTED_MINORITY_STR, UnderRepresentedCb.Checked);
  Result.Add(FIRST_GEN_STUDENT_STR, FirstGenerationCb.Checked);
  Result.Add(HAS_DISABILITY_STR, DisabilityCb.Checked);
  Result.Add(LOW_INCOME_BG_STR, LowIncomeCb.Checked);
  Result.Add(DECLINED, DeclineReportingCb.Checked);
end;

function TDataCollectionSettingsForm.LoadFeedbackFromJson(jsonStr: String): Boolean;
var
  parser: TJSONParser = nil;
  aData: TJSONData = nil;
  aJson: TJSONObject = nil;
begin
  Result := True;
  try
    try
      parser := TJSONParser.Create(jsonStr, []);
      aData := parser.Parse;
      if not Assigned(aData) then
        raise Exception.Create('corrupt demographics json');
      ajson := TJSONObject(aData);
      aData := ajson.Find(RESEARCHER_ACADEMIA_STR, jtBoolean);
      ResearcherAcademiaCb.Checked := aData.AsBoolean;
      aData := ajson.Find(RESEARCHER_INDUSTRY_STR, jtBoolean);
      ResearcherIndustryCb.Checked := aData.AsBoolean;
      aData := ajson.Find(RESEARCHER_GOVERNMENT_STR, jtBoolean);
      ResearcherGovernmentCb.Checked := aData.AsBoolean;
      aData := ajson.Find(INSTRUCTOR_12_STR, jtBoolean);
      Instructor12Cb.Checked := aData.AsBoolean;
      aData := ajson.Find(INSTRUCTOR_COLLEGE, jtBoolean);
      InstructorCollegeCb.Checked := aData.AsBoolean;
      aData := ajson.Find(DOCTORAL_STUDENT_STR, jtBoolean);
      DoctoralStudentCb.Checked := aData.AsBoolean;
      aData := ajson.Find(POSTDOC_STR, jtBoolean);
      PostdocCb.Checked := aData.AsBoolean;
      aData := ajson.Find(MASTERS_STUDENT_STR, jtBoolean);
      MastersStudentCb.Checked := aData.AsBoolean;
      aData := ajson.Find(BACHELORS_STUDENT_STR, jtBoolean);
      BachelorsStudentCb.Checked := aData.AsBoolean;
      aData := ajson.Find(SCHOOL_STUDENT_STR, jtBoolean);
      SchoolStudentCb.Checked := aData.AsBoolean;
      aData := ajson.Find(IN_US_STR, jtBoolean);
      InUnitedStatesCb.Checked := aData.AsBoolean;
      aData := ajson.Find(UNDER_REPRESENTED_MINORITY_STR, jtBoolean);
      UnderRepresentedCb.Checked := aData.AsBoolean;
      aData := ajson.Find(FIRST_GEN_STUDENT_STR, jtBoolean);
      FirstGenerationCb.Checked := aData.AsBoolean;
      aData := ajson.Find(HAS_DISABILITY_STR, jtBoolean);
      DisabilityCb.Checked := aData.AsBoolean;
      aData := ajson.Find(LOW_INCOME_BG_STR, jtBoolean);
      LowIncomeCb.Checked := aData.AsBoolean;
      aData := ajson.Find(DECLINED, jtBoolean);
      DeclineReportingCb.Checked := aData.AsBoolean;
    except
      on E:Exception do
      begin
        Result := False;
        FErrorMsg := E.Message;
      end;
    end;
  finally
    if Assigned(parser) then
      parser.Free;
  end;
end;

procedure TDataCollectionSettingsForm.SetCurrentOptions(dataCollectionAllowed: Boolean);
begin
  try
    FIgnoreEvents := True;
    if dataCollectionAllowed then
      AllowCollectionRadioGrp.ItemIndex := YES_BUTTON
    else
      AllowCollectionRadioGrp.ItemIndex := NO_BUTTON;
    LoadSavedFeedback;
    //SetFeedbackEnabled(dataCollectionAllowed);
  finally
    FIgnoreEvents := False;
  end;
end;

end.

