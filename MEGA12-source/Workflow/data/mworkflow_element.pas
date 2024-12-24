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

unit mworkflow_element;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  {$IFDEF VISUAL_BUILD}
  Forms,
  {$ENDIF}
  Classes, SysUtils, fgl, MLongintList,
  MegaConsts;

type
  {$IFDEF VISUAL_BUILD}
  IFormObserver = interface
    ['{7C794128-CDD3-4ABF-9A86-1BDCA1DD7BB4}']
    function RemoveResultsForm(aForm: TForm): Integer;
    procedure AddResultForm(aForm: TForm);
  end;

  TFormObserverList = specialize TFPGList<IFormObserver>;

  IObservedForm = interface
    ['{6812C66B-698B-4648-BF9B-F2AA44D4BE17}']
    procedure  AddFormObserver(observer: IFormObserver);
  end;
  {$ENDIF}

  TWorkflowElement = class;
  TWorkflowElementList = specialize TFPGList<TWorkflowElement>;

  { TWorkflowElement }

  TWorkflowElement = class abstract(TObject{$IFDEF VISUAL_BUILD}, IFormObserver{$ENDIF})
    private
      FNodeIndex: Integer;
    protected
      FInputElementIDs: TLongintList;
      FErrorMsg: String;
      FIsSuccess: Boolean;
      FOnFinishedNotify: TNotifyEvent;
      FElementName: String;
      {$IFDEF VISUAL_BUILD}
      FResultsForms: TCustomFormList;
      procedure CheckResultsForms;
      {$ENDIF}
      function InputElementIDsDevString: String;
    public
      constructor Create(aElementName: String);
      destructor Destroy; override;
      procedure ElementFinishedCallback(elementOutput: TObject); virtual; abstract;
      function PerformOperation: Boolean; virtual; abstract;
      function DevToString: String;
      {$IFDEF VISUAL_BUILD}
      function DisplayData: Boolean; virtual;
      function HasDisplayData: Boolean; virtual;
      procedure AddResultForm(aForm: TForm);
      function RemoveResultsForm(aForm: TForm): Integer;
      procedure ClearResultsForms;
      {$ENDIF}
      property OnFinishedNotify: TNotifyEvent read FOnFinishedNotify write FOnFinishedNotify;
      property ElementName: String read FElementName write FElementName;
      property IsSuccess: Boolean read FIsSuccess write FIsSuccess;
      property ErrorMsg: String read FErrorMsg write FErrorMsg;
      property NodeIndex: Integer read FNodeIndex write FNodeIndex;
      property InputElementIDs: TLongintList read FInputElementIDs write FInputElementIDs;
  end;

implementation

{ TWorkflowElement }

{$IFDEF VISUAL_BUILD}
procedure TWorkflowElement.CheckResultsForms;
var
  i: Integer;
  aForm: TCustomForm = nil;
begin
  if Assigned(FResultsForms) and (FResultsForms.Count > 0) then
  begin
    for i := FResultsForms.Count - 1 downto 0 do
    begin
      aForm := FResultsForms[i];
      if not Assigned(aForm) then
        FResultsForms.Delete(i)
      else if (aForm is TForm) and (Screen.FormIndex(TForm(aForm)) < 0) then
        FResultsForms.Delete(i);
    end;
    FResultsForms.Pack;
  end;
end;

{$ENDIF}

function TWorkflowElement.InputElementIDsDevString: String;
var
  i: Integer;
begin
  Result := '{';
  if Assigned(FInputElementIDs) and (FInputElementIDs.Count > 0) then
    for i := 0 to FInputElementIDs.Count - 1 do
      Result += IntToStr(FInputElementIDs[i]) + ' ';
  Result := Trim(Result) + '}';
end;

constructor TWorkflowElement.Create(aElementName: String);
begin
  FNodeIndex := -1;
  FOnFinishedNotify := nil;
  FElementName := aElementName;
  FIsSuccess := True;
  FErrorMsg := EmptyStr;
  {$IFDEF VISUAL_BUILD}
  FResultsForms := TCustomFormList.Create;
  {$ENDIF}
end;

destructor TWorkflowElement.Destroy;
begin
  {$IFDEF VISUAL_BUILD}
  if Assigned(FResultsForms) then
    FResultsForms.Free;
  {$ENDIF}
  inherited Destroy;
end;

function TWorkflowElement.DevToString: String;
begin
  Result := Format('%d. %s %s', [NodeIndex, ElementName, InputElementIDsDevString]);
end;

{$IFDEF VISUAL_BUILD}
function TWorkflowElement.DisplayData: Boolean;
var
  i: Integer;
begin
  Result := False;
  if not HasDisplayData then
    exit;
  if FResultsForms.Count > 0 then
    for i := 0 to FResultsForms.Count - 1 do
      if Assigned(FResultsForms[i]) and (FResultsForms[i] is TForm) and (Screen.FormIndex(TForm(FResultsForms[i])) >= 0) then
      begin
        FResultsForms[i].Show;
        if not Result then
          Result := True;
      end;
  Result := True;
end;

function TWorkflowElement.HasDisplayData: Boolean;
begin
  CheckResultsForms;
  Result := (Assigned(FResultsForms) and (FResultsForms.Count > 0));
end;


procedure TWorkflowElement.AddResultForm(aForm: TForm);
begin
  if Assigned(aForm) then
  begin
    if FResultsForms.IndexOf(aForm) < 0 then
      FResultsForms.Add(aForm);
    if Supports(aForm, IObservedForm) then
      (aForm as IObservedForm).AddFormObserver(Self);
  end;
end;

function TWorkflowElement.RemoveResultsForm(aForm: TForm): Integer;
begin
  if FResultsForms.IndexOf(aForm) >= 0 then
    Result := FResultsForms.Remove(aForm);
end;

procedure TWorkflowElement.ClearResultsForms;
begin
  FResultsForms.Clear;
end;
{$ENDIF}

end.

