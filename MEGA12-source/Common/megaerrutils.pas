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

unit MegaErrUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


interface

uses
  {$IFNDEF VISUAL_BUILD}MegaUtils_NV,{$ENDIF}
  LCLIntf, LCLType, SysUtils, Dialogs, mmega_error_handler;

procedure ShowErrorMessage(E: Exception{$IFDEF VISUAL_BUILD}; MsgDlg: TMsgDlgType = mtError{$ENDIF});

procedure RaiseErrorMessage(AErrorId: LongInt; AddInfo: AnsiString; MsgDlg: TMsgDlgType = mtError);

procedure RaiseLimitationMessage(AddInfo: String);
// Rules, if a totally unexpected error occurred then you should send in a negative
// value for Error Id and an emptyString in the AddInfo
// Otherwise send the AddInfo wisely and ErrorId properly

procedure ShowWarningMessage(WarningTxt : AnsiString{$IFDEF VISUAL_BUILD}; HelpContext : integer = 0{$ENDIF});
procedure ReportThreadCrash(ExceptionName: String; ErrorMessage: String; StackTrace: String='');

var
  MegaErrorHandler: TMegaErrorHandler = nil;

implementation

uses
  ErrorMessages_HC,{$IFDEF VISUAL_BUILD}Controls, Mega_Main, Forms, mhelpfiles, mhelpkeywords,{$ENDIF} MegaConsts, MegaVerConsts;

function GetMegaErrorHandler: TMegaErrorHandler;
begin
  if not Assigned(MegaErrorHandler) then
  begin
    MegaErrorHandler := TMegaErrorHandler.Create;
    MegaErrorHandler.Initialize;
  end;
  Result := MegaErrorHandler;
end;

/// <summary>Display a message box to the user that informs them about why
/// a given analysis cannot be completed</summary>
/// <param name="ExceptionName">The name of the exception that was raised</param>
/// <param name="ErrorMessage">The error message given by the raised exception object</param>
/// <param name="StackTrace">The stack trace at the time the exception was raised. This is here
/// for future extension of the procedure. In Delphi XE this value is easy to get so when we
/// convert to XE we can add this.</param>
/// <note>MadExcept can also get us the stack trace, as well the thread name but it will require
/// an investment of time for learning MadExcept.</note>
procedure ReportThreadCrash(ExceptionName: String; ErrorMessage: String; StackTrace: String='');
Var
  MyMessage: String;
  {$IFDEF VISUAL_BUILD}
  Acknowledged: Integer;
  MyCaption: PChar;
  {$ENDIF}
begin
  MyMessage := '';

  if ExceptionName = 'EOutOfMemory' then
    MyMessage := MyMessage +
      'The current analysis cannot be completed because Mega is not able to ' + LineEnding +
      'allocate sufficient memory from the system. This is not an error but ' + LineEnding +
      'rather a limitation due the intensive memory requirements of the calculation ' + LineEnding +
      'requested and the data used.'
  else
    MyMessage := MyMessage +
      'An error has occured preventing the current action from being completed. ' + LineEnding +
      'Description: ' + ExceptionName + LineEnding +
      'Message: ' + ErrorMessage + LineEnding +
      'The authors would greatly appreciate your help in resolving this error. ' + LineEnding +
      'To help, please submit a bug report at ' + WEBSITE_URL + '/bugs';
  {$IFDEF VISUAL_BUILD}
  MyCaption := PChar('Oh no! MEGA encountered an error!');
  Acknowledged := MessageBox(MegaForm.Handle, PChar(MyMessage), MyCaption, MB_ICONINFORMATION);
  {$ELSE}
  Error_NV(MyMessage + ': ' + StackTrace);
  {$ENDIF}
end;
procedure ShowWarningMessage(WarningTxt : AnsiString{$IFDEF VISUAL_BUILD}; HelpContext : integer = 0{$ENDIF});
var
  response: Integer = -1;
begin
  {$IFDEF VISUAL_BUILD}
  if HelpContext = 0 then
    MessageDlg(WarningTxt, mtWarning, [mbOK], 0)
  else
  begin
    response := MessageDlg(WarningTxt, mtWarning, [mbOK, mbHelp], HelpContext);
    if response <> mrOk then
      ShowContextSensitiveHelp(MapHelpContextToKeyword(HelpContext));
  end;
  {$ELSE}
  MegaUtils_NV.warn_NV(WarningTxt);
  {$ENDIF}
end;

procedure ShowErrorMessage(E: Exception{$IFDEF VISUAL_BUILD}; MsgDlg: TMsgDlgType = mtError{$ENDIF});
{$IFDEF VISUAL_BUILD}
var
  buttons: TMsgDlgButtons;
{$ENDIF}
begin
  {$IFDEF VISUAL_BUILD}
  if E.message = EmptyStr then
    Exit; // There is NO reason to show a user an emtpy error message.
  buttons := [mbOK];
  if E.helpContext > 0 then
    buttons := buttons + [mbHelp];
  MessageDlg(E.message,
              mtError,
              buttons,
              E.HelpContext);
  {$ELSE}
  error_nv(E.message, E);
  {$ENDIF}
end;

procedure RaiseErrorMessage(AErrorId: LongInt; AddInfo: AnsiString; MsgDlg: TMsgDlgType = mtError);
begin
  MegaErrorHandler.Log.Add(AddInfo);
  MegaErrorHandler.LastErrorId := AErrorId;

  if AErrorId = HC_User_Stopped_Computation then
  begin
    MegaErrorHandler.Log.Add('user stopped computation');
    raise Exception.Create('');
  end;

  if AErrorId = HC_Unexpected_Error then
  begin
    AErrorId := -1;
  end;

  if MsgDlg = mtWarning then
  begin
    {$IFDEF VISUAL_BUILD}
    ShowWarningMessage(AddInfo, AErrorId);
    {$ELSE}
    warn_NV(AddInfo);
    {$ENDIF}
    AddInfo := EmptyStr;
  end;

  if AErrorID < 0 then
    raise Exception.Create(AddInfo) //MessageDlg(AddInfo, MsgDlg, buttons, AErrorId);
  else
    {$IFDEF VISUAL_BUILD}
    raise Exception.CreateHelp(AddInfo, AErrorId); // The exception still needs to propogate.
    {$ELSE}
    warn_nv(AddInfo);
    raise Exception.Create(AddInfo);
    {$ENDIF}
end;

procedure RaiseLimitationMessage(AddInfo: String);
begin
  {$IFDEF VISUAL_BUILD}
  RaiseErrorMessage(0, AddInfo, mtWarning);
  //raise Exception.CreateHelp(AddInfo, -1);
  {$ELSE}
  Error_NV(AddInfo);
  {$ENDIF}
end;

initialization
  MegaErrorHandler := GetMegaErrorHandler;

end.
