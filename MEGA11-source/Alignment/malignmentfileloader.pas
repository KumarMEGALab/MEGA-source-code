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

unit malignmentfileloader;

interface

uses
  Classes, SysUtils, MD_Sequences, MDataFileInfo, MRuntimeProgressDlg,
  MD_InputSeqData, MOtuInfo;

type

  { TAlignmentFileLoader }

  TAlignmentFileLoader = class(TObject)
    private
      function GetLogText: String;


    protected
      FLog: TStringList;
      FIsConcatenatingFiles: Boolean;
      FShowProgress: Boolean;
      FProgress: TRuntimeProgress;
      FSequenceList: TSequenceList;
      FFilename: String;
      FInputSeqData: TD_InputSeqData;
      FDataFileInfo: TDataFileInfo;
      FAllOtuInfo: TAllOtuInfo;
      procedure SetIsConcatenatingFiles(AValue: Boolean);
      procedure SetShowProgress(AValue: Boolean);
      function GetSequenceList: TSequenceList;
      procedure InitProgress;
      procedure InitInputSeqData; virtual; abstract;
      procedure InitOtuInfos; virtual; abstract;
      procedure ProcessMessages;
    public
      constructor Create(Filename: String; ShowProgress: Boolean=True);
      destructor Destroy; override;

      function TryGetSpecialChars(var gap: Char; var ident: Char; var miss: Char): Boolean; virtual;
      function LoadFile: Boolean; virtual; abstract;
      property SequenceList: TSequenceList read GetSequenceList;
      property Filename: String read FFilename;
      property ShowProgress: Boolean read FShowProgress write SetShowProgress;
      property IsConcatenatingFiles: Boolean read FIsConcatenatingFiles write SetIsConcatenatingFiles;
      property LogText: String read GetLogText;
  end;

implementation

uses
  Forms, MegaVerConsts;

{ TAlignmentFileLoader }

constructor TAlignmentFileLoader.Create(Filename: String; ShowProgress: Boolean=True);
begin
  FIsConcatenatingFiles := False;
  FSequenceList := TSequenceList.Create;
  FDataFileInfo := nil;
  FFilename := Filename;
  FShowProgress := ShowProgress;
  if FShowProgress then
    FProgress := TRuntimeProgress.Create(Application)
  else
    FProgress := nil;
  FInputSeqData := TD_InputSeqData.Create;
  FLog := TStringList.Create;
end;

destructor TAlignmentFileLoader.Destroy;
begin
  if Assigned(FDataFileInfo) then
    FDataFileInfo.Free;
  if Assigned(FProgress) then
    FProgress.Free;
  if Assigned(FInputSeqData) then
    FInputSeqData.Free;
  if Assigned(FSequenceList) then
    FSequenceList.Free;
  if Assigned(FLog) then
    FLog.Free;
  inherited;
end;

function TAlignmentFileLoader.TryGetSpecialChars(var gap: Char;
  var ident: Char; var miss: Char): Boolean;
begin
  Result := False;
end;

function TAlignmentFileLoader.GetLogText: String;
begin
  if Assigned(FLog) then
    Result := FLog.Text
  else
    Result := EmptyStr;
end;

procedure TAlignmentFileLoader.SetIsConcatenatingFiles(AValue: Boolean);
begin
  if FIsConcatenatingFiles=AValue then Exit;
  FIsConcatenatingFiles:=AValue;
end;

procedure TAlignmentFileLoader.SetShowProgress(AValue: Boolean);
begin
  if FShowProgress=AValue then Exit;
  FShowProgress:=AValue;
end;

function TAlignmentFileLoader.GetSequenceList: TSequenceList;
begin
  Result := TSequenceList.Create;
  if Assigned(FSequenceList) then
    Result.Assign(FSequenceList);
end;

procedure TAlignmentFileLoader.InitProgress;
begin
  if not FShowProgress then
    Exit;
  FProgress.Caption := VER_MEGA_WIN_CAPTION_PREFIX+ ': Reading Input Data File';
  FProgress.AddRunStatusInfo('Data file', Filename);
  FProgress.AddRunStatusInfo('Status', 'Reading header');
  FProgress.HideAnalysisOptions;
  FProgress.Show;
  Application.ProcessMessages;
end;

procedure TAlignmentFileLoader.ProcessMessages;
begin
  {$IFDEF VISUAL_BUILD}
  Application.ProcessMessages;
  {$ENDIF}
end;

end.
