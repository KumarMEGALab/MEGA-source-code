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

unit applinkeroptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AppOptionsDlg, KeywordConsts,
  app_options_frame, manalysissettings;

type

  { TApplinkOptionsManager }

  TApplinkOptionsManager = class(TObject)
    private
      FIsDeveloperDefaultOptions: Boolean;
      procedure SetIsDeveloperDefaultOptions(AValue: Boolean);

    protected
      FDataType: TSnTokenCode;
      FDataTypeString: String;
      FMaoStrings: TStringList;
      FDefaultSettingsFile: String;
      FSavedSettingsFile: String; { user's previous setting selections}
      function SetupMuscleOptions: Boolean;
      function SetupClustalOptions: Boolean;
      function ShowMuscleOptions: Integer; { returns ModalResult}
      function ShowClustalOptions: Integer; { returns ModalResult}
      function ResetMuscleDefaultOptions(DataType: TSnTokenCode): Boolean;
      function ResetClustalDefaultOptions(DataType: TSnTokenCode): Boolean;
    public
      OptionsDlg: TAppOptions;
      FIsModelSel: Boolean;
      FIsTreeInf: Boolean;
      constructor Create;
      destructor Destroy; override;
      procedure GetMuscleMaoSettings(DataType: TSnTokenCode);
      procedure GetClustalMaoSettings(DataType: TSnTokenCode);
      function GetMuscleMaoStrings(var AList: TStringList): Boolean; overload;
      function GetMuscleMaoStrings(var AList: TStringList; DataType: TSnTokenCode): Boolean; overload;
      function GetMuscleOptionsDlg(DataType: TSnTokenCode): TAppOptions;
      function GetClustalMaoStrings(var AList: TStringList): Boolean; overload;
      function GetClustalMaoStrings(var AList: TStringList; const DataType: TSnTokenCode): Boolean; overload;
      function GetClustalOptionsDlg(DataType: TSnTokenCode): TAppOptions;
      function GetMuscleSettings(var Options: TStringList; const DataType: TSnTokenCode): Boolean;
      function SaveMuscleSettings: Boolean;
      property IsDeveloperDefaultOptions: Boolean read FIsDeveloperDefaultOptions write SetIsDeveloperDefaultOptions;
  end;

implementation

uses
  MegaPrivateFiles, MegaUtils, Controls, Dialogs, MProcessPack, AlnBuilder_HC, MegaConsts;

{ TApplinkOptionsManager }

procedure TApplinkOptionsManager.SetIsDeveloperDefaultOptions(AValue: Boolean);
begin
  if FIsDeveloperDefaultOptions = AValue then Exit;
  FIsDeveloperDefaultOptions := AValue;
end;

function TApplinkOptionsManager.SetupMuscleOptions: Boolean;
begin
  Result := False;
  if FDataType = snNucleotide then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfMuscleDnaJsonDefault, True);
    FSavedSettingsFile := GetPrivateOutputFile(mfMuscleDnaJsonSaved);
  end
  else if (FDataType = snCodingDna) or (FDataType = snCoding) then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfMuscleCodonsJsonDefault, True);
    FSavedSettingsFile := GetPrivateOutputFile(mfMuscleCodonsJsonSaved);
  end
  else if FDataType = snProtein then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfMuscleAminoJsonDefault, True);
    FSavedSettingsFile := GetPrivateOutputFile(mfMuscleAminoJsonSaved);
  end
  else
  begin
    Assert(False, 'Missing handler for data type. This is a bug!');
  end;
  if not FileExists(FDefaultSettingsFile) then
    raise Exception.Create('Missing configuration file needed for MUSCLE: ' + FDefaultSettingsFile);
  if FileExists(FSavedSettingsFile) then
    OptionsDlg.LoadJson(FSavedSettingsFile, FDataType)
  else
    OptionsDlg.LoadJson(FDefaultSettingsFile, FDataType);
  OptionsDlg.SizeFormToFitOptions;
  OptionsDlg.RestoreDefaultsCallback := @ResetMuscleDefaultOptions;
  Result := True;
end;

function TApplinkOptionsManager.SetupClustalOptions: Boolean;
begin
  Result := False;
  if FDataType = snNucleotide then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfClustalDnaJsonDefault, True);
    FSavedSettingsFile := GetPrivateOutputFile(mfClustalDnaJsonSaved);
  end
  else if FDataType = snCoding then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfClustalCodonsJsonDefault, True);
    FSavedSettingsFile := GetPrivateOutputFile(mfClustalCodonsJsonSaved);
  end
  else if FDataType = snProtein then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfClustalAminoJsonDefault, True);
    FSavedSettingsFile := GetPrivateOutputFile(mfClustalAminoJsonSaved);
  end
  else
  begin
    Assert(False, 'Missing handler for data type. This is a bug!');
  end;
  if not FileExists(FDefaultSettingsFile) then
    raise Exception.Create('Missing configuration file needed for MUSCLE: ' + FDefaultSettingsFile);
  if FileExists(FSavedSettingsFile) then
    OptionsDlg.LoadJson(FSavedSettingsFile, FDataType)
  else
    OptionsDlg.LoadJson(FDefaultSettingsFile, FDataType);
  OptionsDlg.SizeFormToFitOptions;
  OptionsDlg.RestoreDefaultsCallback := @ResetClustalDefaultOptions;
  Result := True;
end;


constructor TApplinkOptionsManager.Create;
begin
  FIsDeveloperDefaultOptions := False;
  OptionsDlg := TAppOptions.Create(nil);
  FMaoStrings := TStringList.Create;
  FDataType := snNoToken;
end;

destructor TApplinkOptionsManager.Destroy;
begin
  if Assigned(OptionsDlg) then
    OptionsDlg.Free;
  if Assigned(FMaoStrings) then
    FMaoStrings.Free;
  inherited Destroy;
end;

procedure TApplinkOptionsManager.GetMuscleMaoSettings(DataType: TSnTokenCode);
var
  SaveDlg: TSaveDialog;
  Response: Integer;
  MaoStrings: TStringList;
begin
  MaoStrings := nil;
  SaveDlg := nil;
  Assert((DataType = snNucleotide) or (DataType = snCoding) or(DataType = snProtein));
  try
    try
      FDataType := DataType;
      if (ShowMuscleOptions = mrOk) then
      begin
        MaoStrings := TStringList.Create;
        GetMuscleMaoStrings(MaoStrings);
        SaveDlg := TSaveDialog.Create(OptionsDlg);
        SaveDlg.InitialDir := GetCurrentDir;
        SaveDlg.Filter := 'MEGA Analysis Options file|*.mao';
        SaveDlg.DefaultExt := 'mao';
        SaveDlg.FileName := 'muscle_align_' + TokenCodeString(FDataType) + '.mao';
        if SaveDlg.Execute then
        begin
          SetCurrentDir(ExtractFileDir(SaveDlg.Filename));
          if FileExists(SaveDlg.Filename) then
          begin
            Response := MessageDlg('The specified file already exists.',  'Are you sure you want to overwrite it?', mtConfirmation, mbOKCancel, 0);
            if Response = mrOk then
            begin
              MaoStrings.SaveToFile(SaveDlg.Filename);
            end;
          end
          else
            MaoStrings.SaveToFile(SaveDlg.Filename);
        end;
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(MaoStrings) then
      MaoStrings.Free;
    if Assigned(SaveDlg) then
      SaveDlg.Free;
  end;
end;

procedure TApplinkOptionsManager.GetClustalMaoSettings(DataType: TSnTokenCode);
var
  SaveDlg: TSaveDialog;
  Response: Integer;
  MaoStrings: TStringList;
begin
  MaoStrings := nil;
  SaveDlg := nil;
  Assert((DataType = snNucleotide) or (DataType = snCoding) or(DataType = snProtein));
  try
    try
      FDataType := DataType;
      if (ShowClustalOptions = mrOk) then
      begin
        MaoStrings := TStringList.Create;
        GetClustalMaoStrings(MaoStrings);
        SaveDlg := TSaveDialog.Create(OptionsDlg);
        SaveDlg.InitialDir := GetCurrentDir;
        SaveDlg.Filter := 'MEGA Analysis Options file|*.mao';
        SaveDlg.DefaultExt := 'mao';
        SaveDlg.FileName := 'clustal_align_' + TokenCodeString(FDataType) + '.mao';
        if SaveDlg.Execute then
        begin
          if FileExists(SaveDlg.Filename) then
          begin
            Response := MessageDlg('The specified file already exists.',  'Are you sure you want to overwrite it?', mtConfirmation, mbOKCancel, 0);
            if Response = mrOk then
            begin
              MaoStrings.SaveToFile(SaveDlg.Filename);
            end;
          end
          else
            MaoStrings.SaveToFile(SaveDlg.Filename);
        end;
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(MaoStrings) then
      MaoStrings.Free;
    if Assigned(SaveDlg) then
      SaveDlg.Free;
  end;
end;

function TApplinkOptionsManager.ShowMuscleOptions: Integer;
var
  Response: Integer;
begin
  if not SetupMuscleOptions then
    raise Exception.Create('Failed to setup options dialog for MUSCLE alignment');
  OptionsDlg.Caption := 'MUSCLE Alignment Options';
  if FDataType = snProtein then
    OptionsDlg.HelpContext := MUSCLE_Options_Protein_
  else
    OptionsDlg.HelpContext := MUSCLE_Options_DNA_;
  if FIsDeveloperDefaultOptions then
  begin
    Response := mrOK;
  end
  else
  begin
    Response := OptionsDlg.ShowModal;
    while Response = mrRetry do { happens when the user resets to the defaults}
      Response := OptionsDlg.ShowModal;
    Result := Response;
    if Result = mrOK then
      SaveMuscleSettings;
  end;
end;

function TApplinkOptionsManager.ShowClustalOptions: Integer;
var
  Response: Integer;
begin
  if not SetupClustalOptions then
    raise Exception.Create('Failed to setup options dialog for Clustal alignment');
  OptionsDlg.Caption := 'ClustalW Alignment Options';
  if FIsDeveloperDefaultOptions then
  begin
    Response := mrOk;
  end
  else
  begin
    Response := OptionsDlg.ShowModal;
    while Response = mrRetry do { happens when the user resets to the defaults}
      Response := OptionsDlg.ShowModal;
  end;
  Result := Response;
end;


function TApplinkOptionsManager.ResetMuscleDefaultOptions(DataType: TSnTokenCode): Boolean;
begin
  Result := False;

  if DataType = snNucleotide then
    FDefaultSettingsFile := GetPrivateFile(mfMuscleDnaJsonDefault, True)
  else if (DataType = snCoding) or (DataType = snCodingDna) then
    FDefaultSettingsFile := GetPrivateFile(mfMuscleCodonsJsonDefault, True)
  else if DataType = snProtein then
    FDefaultSettingsFile := GetPrivateFile(mfMuscleAminoJsonDefault, True);

  if not FileExists(FDefaultSettingsFile) then
    raise Exception.Create('Missing configuration file needed for MUSCLE: ' + FDefaultSettingsFile);
  OptionsDlg.LoadJson(FDefaultSettingsFile, DataType);
  OptionsDlg.SizeFormToFitOptions;
  OptionsDlg.Visible := True;
  Result := True;
end;

function TApplinkOptionsManager.ResetClustalDefaultOptions(DataType: TSnTokenCode): Boolean;
begin
  Result := False;
  if DataType = snNucleotide then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfClustalDnaJsonDefault, True);
  end
  else if (DataType = snCoding) or (DataType = snCodingDna) then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfClustalCodonsJsonDefault, True);
  end
  else if DataType = snProtein then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfClustalAminoJsonDefault, True);
  end;
  if not FileExists(FDefaultSettingsFile) then
    raise Exception.Create('Missing configuration file needed for Clustal: ' + FDefaultSettingsFile);
  OptionsDlg.LoadJson(FDefaultSettingsFile, DataType);
  OptionsDlg.SizeFormToFitOptions;
  OptionsDlg.Visible := True;
end;


function TApplinkOptionsManager.GetMuscleMaoStrings(var AList: TStringList): Boolean;
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;

  try
    ProcessPack := TProcessPack.Create;
    ProcessPack.AddProcessType(ppAlign);
    ProcessPack.AddProcessType(ppMuscle);
    Result := OptionsDlg.GetMaoStrings(AList, ProcessPack);
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

function TApplinkOptionsManager.GetMuscleMaoStrings(var AList: TStringList; DataType: TSnTokenCode): Boolean;
begin
  Assert((DataType = snNucleotide) or (DataType = snCoding) or(DataType = snProtein));
  FDataType := DataType;
  if ShowMuscleOptions = mrOk then
    Result := GetMuscleMaoStrings(AList)
  else
    Result := False;
end;

function TApplinkOptionsManager.GetMuscleOptionsDlg(DataType: TSnTokenCode): TAppOptions;
begin
  FDataType := DataType;
  if not SetupMuscleOptions then
    raise Exception.Create('Failed to setup options dialog for MUSCLE alignment');
  OptionsDlg.Caption := 'MUSCLE Alignment Options';
  if FDataType = snProtein then
    OptionsDlg.HelpContext := MUSCLE_Options_Protein_
  else
    OptionsDlg.HelpContext := MUSCLE_Options_DNA_;
  Result := OptionsDlg;
  OptionsDlg := nil;
end;

function TApplinkOptionsManager.GetClustalMaoStrings(var AList: TStringList): Boolean;
var
  ProcessPack: TProcessPack;
begin
  ProcessPack := nil;

  try
    ProcessPack := TProcessPack.Create;
    ProcessPack.AddProcessType(ppAlign);
    ProcessPack.AddProcessType(ppClustalW);
    Result := OptionsDlg.GetMaoStrings(AList, ProcessPack);
  finally
    if Assigned(ProcessPack) then
      ProcessPack.Free;
  end;
end;

function TApplinkOptionsManager.GetClustalMaoStrings(var AList: TStringList; const DataType: TSnTokenCode): Boolean;
begin
  Assert((DataType = snNucleotide) or (DataType = snCoding) or(DataType = snProtein));
  FDataType := DataType;
  if ShowClustalOptions = mrOk then
    Result := GetClustalMaoStrings(AList)
  else
    Result := False;
end;

function TApplinkOptionsManager.GetClustalOptionsDlg(DataType: TSnTokenCode): TAppOptions;
begin
  FDataType := DataType;
  if FDataType = snNucleotide then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfClustalDnaJsonDefault, True);
    FSavedSettingsFile := GetPrivateOutputFile(mfClustalDnaJsonSaved);
  end
  else if FDataType = snCoding then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfClustalCodonsJsonDefault, True);
    FSavedSettingsFile := GetPrivateOutputFile(mfClustalCodonsJsonSaved);
  end
  else if FDataType = snProtein then
  begin
    FDefaultSettingsFile := GetPrivateFile(mfClustalAminoJsonDefault, True);
    FSavedSettingsFile := GetPrivateOutputFile(mfClustalAminoJsonSaved);
  end
  else
  begin
    Assert(False, 'Missing handler for data type. This is a bug!');
  end;

  if not FileExists(FDefaultSettingsFile) then
    raise Exception.Create('Missing configuration file needed for MUSCLE: ' + FDefaultSettingsFile);
  if FileExists(FSavedSettingsFile) then
    OptionsDlg.LoadJson(FSavedSettingsFile, FDataType)
  else
    OptionsDlg.LoadJson(FDefaultSettingsFile, FDataType);
  OptionsDlg.SizeFormToFitOptions;
  OptionsDlg.RestoreDefaultsCallback := @ResetClustalDefaultOptions;
  Result := OptionsDlg;
  OptionsDlg := nil;
end;

function TApplinkOptionsManager.GetMuscleSettings(var Options: TStringList; const DataType: TSnTokenCode): Boolean;
begin
  Result := False;
  Assert((DataType = snNucleotide) or (DataType = snCodingDna) or(DataType = snProtein));
  try
    FDataType := DataType;
    if (ShowMuscleOptions = mrOk) then
    begin
      OptionsDlg.GetCmdArgs(Options);
      Result := True;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

function TApplinkOptionsManager.SaveMuscleSettings: Boolean;
var
  settings: String = '';
  aFile: TextFile;
begin
  if Assigned(OptionsDlg) then
  begin
    try
      settings := OptionsDlg.GetJsonString;
      AssignFile(aFile, FSavedSettingsFile);
      Rewrite(aFile);
      WriteLn(aFile, settings);
    finally
      CloseFile(aFile);
    end;
  end;
end;


end.

