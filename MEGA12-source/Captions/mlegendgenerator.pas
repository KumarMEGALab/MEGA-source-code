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

unit MLegendGenerator;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ENDIF}
  FileUtil,
  Classes, Contnrs, SysUtils, Forms, MegaUtils, mcascadingstyles,
  MegaErrUtils, RegExpr;
{$M+}

const
  tokTagOpen = '{';
  tokTagClose = '}';
  tokSpace = ' ';
  tokBlank = '';
  padChar = '^';
  tcoEquals = '=';
  tcoNotEquals = '!';
  tcoLessThan = '<';
  tcoGreaterThan = '>';
  tcoFunctionPipe = ':';
  validConditionals: array [0 .. 3] of String = ('if', 'elseif', 'else', 'endif');
  validOperators: array [0 .. 3] of String = (tcoEquals, tcoNotEquals, tcoLessThan, tcoGreaterThan);
  HTML_FILE_EXT = '.htm';

type
  PTag = ^TTag;

  TTag = record
    Namespace: String;
    ValueField: String;
    Value: String;
    OriginalValue: String;
    Expression: String;
    IsConditional: Boolean;
    PositionBegin: Integer; // position in text
    PositionEnd: Integer;
    TerminatorBegin: Integer; // Starting Position of Endif for a conditional tag
    TerminatorEnd: Integer;
    ConditionalDirective: String; // probably too generic but means if, while, etc
    ConditionalResult: Boolean; // The result of the conditional evaulation
    ConditionalOperator: String;
    ConditionalValue: String; // The truth value being tested
    ContingentConditional: PTag; // this is the parent block enclosing the current block
    Next: PTag;
    ApplyFunction: Boolean;
    FunctionName: String;
  end;

type

  { TLegendGenerator }

  TLegendGenerator = Class
  private
    FTemplateConstants: TStringList;
    FStyleSheet: TStyleSheet;
    TemplateTags: TList;
    CitationList: TStringList;
    TemplateText: TStringList;
    function PostProcessCitations(tempResult: String): String;
    function GetPropertyValueAsString(Instance: TObject; PropName: String): String;
    function CollectTags: Boolean;
    function LoadTemplateText(TemplateFileName: String): Boolean;
    procedure NotifyCleaningTagCollection(Sender: TObject; const Item: PTag; Action: TCollectionNotification);
    procedure RefreshConditionals;
    function LoadConstants: Boolean;
    procedure HandleFunctionCall(Tag: PTag);
    procedure ExpandCitation(tag: PTag);
    function CreateTag: PTag;
    procedure DestroyTag(ATag: PTag);
    function isaOperator(Token: String): Boolean;
    function isaConditional(Token: String): Boolean;
    function isValidTerminalConditional(Token: String): Boolean;
    function getNextWord(Position: Integer; Text: String): String;
    function ExtractCitationName(tempStr: String): String;
    procedure ClearTags;
  public
    LastErrorMsg: String;
    TemplateFile: TFileName;
    constructor Create;
    destructor Destroy; override;
    procedure ClearCitations;
    procedure ClearAllCaptionData;
    function LoadStyleSheet: Boolean;
    function LoadCitationFromFile(HelpTopic: String): String;
    procedure AssignData(templateVariableName, Value: String);
    function HasData(templateVariableName: String): Boolean;
    // For assigning simple types (int, str, double, etc)
    procedure BindData(DataObject: TObject; CacheObject: Boolean = False);
    procedure FlushTemplate(DataObject: TObject);
    function LoadTemplateFromFile(TemplateFileName: TFileName): Boolean;
    function GenerateLegend: String;
    procedure GenerateLegendAsText(var StringList: TStringList);
    procedure GenerateNoteAsText(var StringList: TStringList);
{$IFNDEF VISUAL_BUILD}
    procedure SaveCaptionToFile(HTMLtoSave: String = '');
{$ENDIF}
    property StyleSheet: TStyleSheet read FStyleSheet;
  end;

procedure TagString(ATag: TTag; var AList: TStringList);

implementation

uses
  {$IFDEF VISUAL_BUILD}
  htmlview,
  {$ENDIF}
  StrUtils, TypInfo, Dialogs, MegaVerConsts,
  Variants, MegaConsts, MegaPrivateFiles;

procedure TagString(ATag: TTag; var AList: TStringList);
begin
  AList.Add('Namespace: ' + ATag.Namespace); // : AnsiString;
  AList.Add('ValueField: ' + ATag.ValueField);
  // : AnsiString; // Default namespace for simple AssignData vars is just self (TLegendGenerator)
  AList.Add('Value: ' + ATag.Value); // : AnsiString;
  AList.Add('OriginalValue: ' + ATag.OriginalValue); // : AnsiString;
  AList.Add('Expression: ' + ATag.Expression); // : AnsiString;
  AList.Add('IsConditional: ' + BoolToStr(ATag.IsConditional, True));
  // : Boolean;
  AList.Add('PositionBegin: ' + IntToStr(ATag.PositionBegin));
  // : Integer;  // position in text
  AList.Add('PositionEnd: ' + IntToStr(ATag.PositionEnd)); // : Integer;
  AList.Add('TerminatorBegin: ' + IntToStr(ATag.TerminatorBegin));
  // : Integer; // Starting Position of Endif for a conditional tag
  AList.Add('TerminatorEnd: ' + IntToStr(ATag.TerminatorEnd)); // : Integer;
  AList.Add('ConditionalDirective: ' + ATag.ConditionalDirective);
  // : AnsiString; // probably too generic but means if, while, etc
  AList.Add('ConditionalResult: ' + BoolToStr(ATag.ConditionalResult, True));
  // : Boolean; // The result of the conditional evaulation
  AList.Add('ConditionalOperator: ' + ATag.ConditionalOperator);
  // : AnsiString;
  AList.Add('ConditionalValue: ' + ATag.ConditionalValue);
  // : AnsiString;  // The truth value being tested
  // AList.Add('ContingentConditional: ' + (ATag.ContingentConditional)^.ConditionalValue);  // : PTag;   // this is the parent block enclosing the current block
  AList.Add('ApplyFunction: ' + BoolToStr(ATag.ApplyFunction, True));
  // : Boolean; // tacked on to handle Kumar's preferred method of citing at bottom
  AList.Add('FunctionName: ' + ATag.FunctionName); // : AnsiString;
end;

constructor TLegendGenerator.Create;
begin
  FTemplateConstants := TStringList.Create;
  TemplateTags := TList.Create;
  CitationList := TStringList.Create;
  TemplateText := TStringList.Create;
  FStyleSheet := TStyleSheet.Create;
end;

destructor TLegendGenerator.Destroy;
begin
  if Assigned(FTemplateConstants) then
    FTemplateConstants.Free;
  if Assigned(FStyleSheet) then
    FStyleSheet.Free;
  if Assigned(TemplateTags) then
  begin
    ClearTags;
    TemplateTags.Free;
  end;
  if Assigned(CitationList) then
    CitationList.Free;
  if Assigned(TemplateText) then
    TemplateText.Free;
end;

procedure TLegendGenerator.ClearCitations;
begin
  if Assigned(CitationList) then
    CitationList.Clear;
end;

procedure TLegendGenerator.ClearAllCaptionData;
begin
  ClearTags;
  ClearCitations;
  TemplateText.Clear;
end;

procedure TLegendGenerator.DestroyTag(ATag: PTag);
begin
  if Assigned(ATag) then
  begin
    Dispose(ATag);
    ATag := nil;
  end;
end;

procedure TLegendGenerator.BindData(DataObject: TObject; CacheObject: Boolean = False);
var
  ClassName: String;
  TagRecord: PTag;
  PropInfo: PPropInfo;
  testValueInteger, i: Integer;
  testValueDouble: Double;
  cleanStr: String = '';
begin
  try
    if DataObject = nil then
      Exit;
    // See if these are the droids we are looking for
    ClassName := DataObject.ClassName;
    for i := 0 to TemplateTags.Count - 1 do
    begin
      TagRecord := TemplateTags[i];
      if TagRecord.Namespace = ClassName then
        if IsPublishedProp(DataObject, TagRecord.ValueField) then
        begin
          // Do we want to do any checking?
          TagRecord.Value := GetPropertyValueAsString(DataObject, TagRecord.ValueField);
          TagRecord.OriginalValue := TagRecord.Value;
          if TagRecord.IsConditional then
          begin
            cleanStr := StringReplace(TagRecord.Value, ',', '', [rfReplaceAll]);
            // evaluate the conditional
            if TagRecord.ConditionalOperator = tcoEquals then
            begin
              // Changed to be non case sensitive
              // if TagRecord.ConditionalValue = TagRecord.Value then
              if SameText(TagRecord.ConditionalValue, cleanStr) then
                TagRecord.ConditionalResult := True;
            end
            else if TagRecord.ConditionalOperator = tcoNotEquals then
            begin
              // if TagRecord.ConditionalValue <> TagRecord.Value then
              if StrIComp(PChar(TagRecord.ConditionalValue), PChar(cleanStr)) <> 0 then
                TagRecord.ConditionalResult := True;
            end
            // The following can only support ordinals for now!
            else if TagRecord.ConditionalOperator = tcoGreaterThan then
            begin
              if TryStrToInt(TagRecord.ConditionalValue, testValueInteger) and
                TryStrToInt(cleanStr, testValueInteger) then
              begin
                // we have an integer of some sort
                if StrToInt(cleanStr) >
                  StrToInt(TagRecord.ConditionalValue) then
                  TagRecord.ConditionalResult := True;
              end
              else if TryStrToFloat(TagRecord.ConditionalValue, testValueDouble)
                and TryStrToFloat(cleanStr, testValueDouble) then
              begin
                if StrToFloat(cleanStr) >
                  StrToFloat(TagRecord.ConditionalValue) then
                  TagRecord.ConditionalResult := True;
              end
              else
              begin
                // we have an enumerated type or garbage
                PropInfo := GetPropInfo(DataObject, TagRecord.ValueField);
                if StrToInt(cleanStr) > GetEnumValue(PropInfo.PropType, TagRecord.ConditionalValue) then
                  TagRecord.ConditionalResult := True;
              end;
            end
            else if TagRecord.ConditionalOperator = tcoLessThan then
            begin
              if TryStrToInt(TagRecord.ConditionalValue, testValueInteger) and
                TryStrToInt(cleanStr, testValueInteger) then
              begin
                // we have an integer of some sort
                if StrToInt(cleanStr) <
                  StrToInt(TagRecord.ConditionalValue) then
                  TagRecord.ConditionalResult := True;
              end
              else if TryStrToFloat(TagRecord.ConditionalValue, testValueDouble)
                and TryStrToFloat(cleanStr, testValueDouble) then
              begin
                if StrToFloat(cleanStr) <
                  StrToFloat(TagRecord.ConditionalValue) then
                  TagRecord.ConditionalResult := True;
              end
              else
              begin
                // we have an enumerated type or garbage
                PropInfo := GetPropInfo(DataObject, TagRecord.ValueField);
                if StrToInt(cleanStr) < GetEnumValue(PropInfo.PropType, TagRecord.ConditionalValue) then
                  TagRecord.ConditionalResult := True;
              end;
            end
            else
              ShowMessage('WARNING: Unsupported conditional operator: ' +
                TagRecord.ConditionalOperator);
          end;
        end;
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

function TLegendGenerator.CollectTags: Boolean;
type
  TParseState = (psCollectTag, psNext, psQuit);
var
  Parser: TParser = nil;
  TagStack: TStack = nil;
  ParseState: TParseState;
  IntermediaryStream: TStringStream = nil;
  CurTag, PopTag, PeekTag: PTag;
  CollectBeginPos: Integer;
begin
  Result := False;
  TagStack := TStack.Create;
  try
    try
    if TemplateText.Count > 0 then
    begin
      IntermediaryStream := TStringStream.Create(TemplateText.Text);
      Parser := TParser.Create(IntermediaryStream);  // Using the undocumented TParser class that Lazarus/Delphi use for streaming form files (e.g .lfm)
      ParseState := psNext;
      while (Parser.Token <> toEOF) and (ParseState <> psQuit) do
      begin
        if ParseState = psNext then
        begin
          if Parser.TokenString = tokTagOpen then
          begin
            ParseState := psCollectTag;
            Continue;
          end
          else
            Parser.NextToken;
        end
        else if ParseState = psCollectTag then
        begin
          // Get the namespace or conditional
          CollectBeginPos := Parser.SourcePos;
          Parser.NextToken;
          //CollectBeginPos := Parser.SourcePos;
          if isaConditional(Parser.TokenString) then
          begin
            if isValidTerminalConditional(Parser.TokenString) then
            begin
              // wind forward so we know the position of the closing token.
              Parser.NextToken;
              // make sure we have a conditional on the stack to terminate
              if TagStack.Count > 0 then
              begin
                // clear the stack to the first 'if'
                while TagStack.Count > 0 do
                begin
                  PopTag := TagStack.Pop;
                  PopTag.TerminatorBegin := CollectBeginPos;
                  PopTag.TerminatorEnd := Parser.SourcePos + 1;
                  if PopTag.ConditionalDirective = 'if' then
                    Break;
                end;
                ParseState := psNext;
                Continue;
              end
              else
                raise Exception.Create('Unexpected endif!');
            end
            else
            begin
              // start of a new non-terminal conditional
              CurTag := CreateTag();
              CurTag.PositionBegin := CollectBeginPos;
              CurTag.IsConditional := True;
              CurTag.ConditionalDirective := Parser.TokenString;
              Parser.NextToken;
              if CurTag.ConditionalDirective = 'else' then
              begin
                CurTag.ConditionalResult := True; // default for 'else' only
                CurTag.ContingentConditional := PTag(TagStack.Peek);
                CurTag.Expression := tokTagOpen + CurTag.ConditionalDirective + tokTagClose;
                ParseState := psNext;
                CurTag.PositionEnd := Parser.SourcePos;
                if TagStack.Count > 0 then
                begin
                  PeekTag := TagStack.Peek;
                  PeekTag.Next := CurTag;
                end;
                TagStack.Push(CurTag);
              end
              else
              begin
                // != else
                CurTag.Namespace := Parser.TokenString;
                Parser.NextToken;
                if Parser.TokenString <> '.' then // This token should be a period. Check to make sure and throw parse error if not
                  raise Exception.Create('Expecting "." on line ' + IntToStr(Parser.SourceLine) + ' position ' + IntToStr(Parser.SourcePos));
                // Get the value field
                Parser.NextToken;
                CurTag.ValueField := Parser.TokenString;
                // next token should be a valid operator
                Parser.NextToken;
                if isaOperator(Parser.TokenString) then
                begin
                  CurTag.ConditionalOperator := Parser.TokenString;
                  // get the test condition
                  Parser.NextToken;
                  CurTag.ConditionalValue := Parser.TokenString;
                  // the next token should be a closing
                  Parser.NextToken;
                  CurTag.PositionEnd := Parser.SourcePos;
                  ParseState := psNext;
                  // Continue;
                end
                else
                  raise Exception.Create('Expecting valid operator on line ' + IntToStr(Parser.SourceLine) + ' position ' + IntToStr(Parser.SourcePos));

                CurTag.Expression := tokTagOpen + CurTag.ConditionalDirective +
                                     ' ' + CurTag.Namespace + '.' + CurTag.ValueField + ' ' +
                                     CurTag.ConditionalOperator + ' ' + CurTag.ConditionalValue +
                                     tokTagClose;
                if CurTag.ConditionalDirective = 'if' then
                begin
                  if TagStack.Count > 0 then
                    CurTag.ContingentConditional := PTag(TagStack.Peek);
                end
                else
                begin
                  if TagStack.Count > 0 then
                  begin
                    PeekTag := TagStack.Peek;
                    PeekTag.Next := CurTag;
                    TagStack.Push(CurTag);
                  end
                  else
                  begin
                    raise Exception.Create('Expected "if" but found "' + CurTag.ConditionalDirective + '"');
                  end;
                end;
                TagStack.Push(CurTag);
              end;
            end;
          end
          else
          begin
            // Regular old value tag
            CurTag := CreateTag();
            CurTag.PositionBegin := CollectBeginPos;
            CurTag.Namespace := Parser.TokenString;
            // This token should be a period check to make sure and throw parse error if not
            Parser.NextToken;
            // Get the value field
            Parser.NextToken;
            CurTag.ValueField := Parser.TokenString;
            // This token should be the closing tag
            Parser.NextToken;
            if Parser.TokenString = tcoFunctionPipe then
            begin
              // Get the function name
              Parser.NextToken;
              CurTag.ApplyFunction := True;
              CurTag.FunctionName := Parser.TokenString;
              Parser.NextToken;
              CurTag.Expression := tokTagOpen + CurTag.Namespace + '.' +
                CurTag.ValueField + tcoFunctionPipe + CurTag.FunctionName +
                tokTagClose;
            end
            else
              CurTag.Expression := tokTagOpen + CurTag.Namespace + '.' +
                CurTag.ValueField + tokTagClose;
            CurTag.PositionEnd := Parser.SourcePos;
            ParseState := psNext;
            if TagStack.Count > 0 then
              CurTag.ContingentConditional := PTag(TagStack.Peek);
            // TemplateTags.Add(CurTag);
          end;
          // if TagStack.Count > 0 then CurTag.ContingentConditional := PTag(TagStack.Peek);
          TemplateTags.Add(CurTag);
        end
        else
          ParseState := psNext;
      end;
      Result := True;
    end;
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
      ShowMessage('An application error has occurred while generating a caption: ' + E.Message);
      {$ELSE}
      warn_nv('An error has occurred while generating a caption: ' + E.Message);
      {$ENDIF}
    end;
  finally
    if Assigned(Parser) then
      Parser.Free;
    if Assigned(IntermediaryStream) then
      IntermediaryStream.Free;
    if Assigned(TagStack) then
      TagStack.Free;
  end;
end;

{$IFNDEF VISUAL_BUILD}

procedure TLegendGenerator.SaveCaptionToFile(HTMLtoSave: String = '');
var
  SaveStrList: TStringList;
begin
  SaveStrList := nil;
  try
    SaveStrList := TStringList.Create;
    if HTMLtoSave = EmptyStr then
      SaveStrList.Add(GenerateLegend)
    else
      SaveStrList.Add(HTMLtoSave);

    SaveStrList.SaveToFile(NextAvailableFilenameNV('_caption.html'));
  except
    on E: Exception do
      Raise Exception.Create('Unable to save caption file Error: ' + E.Message);
  end;

end;
{$ENDIF}

function TLegendGenerator.GenerateLegend: String;

  procedure _DeleteTag(var templateString: String;
    PosBegin, PosEnd: Integer);
  var
    i: Integer;
  begin
    if (PosBegin = 0) and (PosEnd = 0) then
      Exit;

    if (PosBegin <= 0) or (PosEnd <= 0) then
    begin
      ShowMessage(IntToStr(PosBegin) + ' ' + IntToStr(PosEnd) + ' ' +
        templateString);
      Raise Exception.Create('Invalid Position in _DeleteTag: begin = ' + IntToStr(PosBegin) + ' , end = ' + IntToStr(PosEnd));
    end;
    for i := PosBegin to PosEnd do
    begin
      if (i > 0) and (i <= Length(templateString)) then
        templateString[i] := padChar;
    end;
  end;

  procedure _StringReplace(var BaseText: String;
    FromString, ToString: String);
  var
    i: Integer;
    slTemp: TStringList;
  begin
    slTemp := TStringList.Create;
    slTemp.Text := BaseText;
    try
      for i := 0 to slTemp.Count - 1 do
      begin
        slTemp.Strings[i] := ReplaceStr(slTemp.Strings[i], FromString, ToString);
      end;
      BaseText := slTemp.Text;
    finally
      slTemp.Free;
    end;
  end;

var
  CurTag, ContingentTag, NextTag: PTag;
  i, j: Integer;
  SnippetLoader: TStringList = nil;
begin
  try
    RefreshConditionals; // Need to take care of the conditionals first otherwise the positions move when you

    Result := TemplateText.Text;
    for j := 0 to TemplateTags.Count - 1 do
    begin
      CurTag := TemplateTags.Items[j];
      if CurTag.IsConditional then
      begin
        if CurTag.ConditionalResult then
        begin
          // Conditional evaluated true. remove the conditional but keep the content
          _DeleteTag(Result, CurTag.PositionBegin, CurTag.PositionEnd);
        end
        else
        begin
          // Conditional evaluated false. remove the conditional and its content
          if CurTag.Next <> nil then
          begin
            NextTag := CurTag.Next;
            for i := CurTag.PositionBegin to NextTag.PositionBegin - 1 do
              if (i > 0) and (i <= Length(Result)) then
                Result[i] := padChar;
          end
          else
          begin
            for i := CurTag.PositionBegin to CurTag.TerminatorBegin do
              if (i > 0) and (i <= Length(Result)) then
                Result[i] := padChar;
          end;
        end;
        // if it's 'if' take care of the endif
        if CurTag.ConditionalDirective = 'if' then
          _DeleteTag(Result, CurTag.TerminatorBegin, CurTag.TerminatorEnd);
      end
    end;


    // for CurTag in TemplateTags do
    for j := 0 to TemplateTags.Count - 1 do
    begin
      CurTag := TemplateTags.Items[j];
      if CurTag.ApplyFunction then
        HandleFunctionCall(CurTag);
      if CurTag.IsConditional = False then
      begin
        // Replace value tag
        if CurTag.ContingentConditional <> nil then
        begin
          ContingentTag := CurTag.ContingentConditional;
          if ContingentTag.ConditionalResult then
            if Pos(CurTag.Expression, Result) > 0 then
              // Result := AnsiReplaceStr(Result,CurTag.Expression,CurTag.Value);
              _StringReplace(Result, CurTag.Expression, CurTag.Value);
        end
        else
          // Result := AnsiReplaceStr(Result,CurTag.Expression,CurTag.Value);
          _StringReplace(Result, CurTag.Expression, CurTag.Value);
      end;
    end;
    // remove the padChars. This is a bit hokey but I don't want to deal with changing Pos if I delete a char
    // Result := AnsiReplaceStr(Result,padChar,'');
    _StringReplace(Result, padChar, '');
    _StringReplace(Result, #$D#$A, '');
    _StringReplace(Result, #9, '');

    Result := PostProcessCitations(Result);
    // if we have citations insert them at the end
    if CitationList.Count > 0 then
    begin
      for i := 0 to CitationList.Count - 1 do
        // Result := Result + CitationList.Strings[i];
        Result := Result + StringReplace(CitationList.Strings[i], '$refNumber',
          IntToStr(i + 1), []);
    end;
    // tag the citation on to the end. Dunno if this should be in the Delphi code, but it works for now
    try
      SnippetLoader := TStringList.Create;
      SnippetLoader.LoadFromFile
        (GetPrivateFile('Private' + PathDelim + 'Templates' + PathDelim + 'disclaimer.snippet'));
      // changed for consistency of file access and compatiblity with RES packing.
      Result := Result + SnippetLoader.Text;
    finally
      if Assigned(SnippetLoader) then
        SnippetLoader.Free;
    end;
  except
    on EOutOfMemory do
      ShowMessage('Ran out of memory!');
    on E: Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

function TLegendGenerator.LoadTemplateFromFile(TemplateFileName: TFileName): Boolean;
begin
  Result := False;
  try
    if LoadTemplateText(TemplateFileName) then
      if CollectTags then
        if LoadConstants then
          if LoadStyleSheet then
            Result := True;
  except
    on E:Exception do
      {$IFDEF VISUAL_BUILD}
      ShowMessage('An application error occurred while generating a caption: ' + E.Message);
      {$ELSE}
      warn_nv('An error occurred while generating a caption: ' + E.Message);
      {$ENDIF}
  end;
end;

function TLegendGenerator.LoadTemplateText(TemplateFileName: String): Boolean;
var
  ExternFileImport: TStringList = nil;
  ExternFile: String;
  TempTag: String;
  AFile: String;
  posExternalFileTag: Integer;
  aMsg: String;
begin
  Result := False;
  try
    try
      AFile := GetPrivateFile('Private' + PathDelim + 'Templates' + PathDelim + TemplateFileName);
      if FileExists(AFile) then
      begin
        ExternFileImport := TStringList.Create;
        TemplateFile := AFile;
        if TemplateText.Count > 0 then
          TemplateText.Clear;
        TemplateText.LoadFromFile(TemplateFile);
        posExternalFileTag := Pos('IncludeExternalFile', TemplateText.Text);
        while posExternalFileTag > 0 do
        begin
          TempTag := copy(TemplateText.Text, posExternalFileTag - 1, length(TemplateText.Text) - posExternalFileTag);
          TempTag := copy(TempTag, 0, Pos('}', TempTag));
          ExternFile := copy(TempTag, Pos('=', TempTag) + 1, length(TempTag));
          ExternFile := copy(ExternFile, 0, length(ExternFile) - 1);
          ExternFileImport.Clear;
          ExternFileImport.LoadFromFile(GetPrivateFile('Private' + PathDelim + 'Templates' + PathDelim + ExternFile));
          TemplateText.Text := ReplaceStr(TemplateText.Text, TempTag, Trim(ExternFileImport.Text));
          posExternalFileTag := Pos('IncludeExternalFile', TemplateText.Text);
        end;
        Result := True;
      end
      else
        raise Exception.Create('Missing citation template: ' + AFile);
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        {$IFDEF DEBUG}
        aMsg := ExceptionCallStack(E);
        {$ELSE}
        aMsg := E.Message;
        {$ENDIF}
        ShowMessage('Application Error occurred while generating a caption: ' + aMsg + ' '+ TemplateFileName);
        {$ELSE}
        warn_nv('An error occurred while generating a caption: ' + E.Message);
        {$ENDIF}
      end;

    end;
  finally
    if Assigned(ExternFileImport) then
      ExternFileImport.Free;
  end;
end;

procedure TLegendGenerator.NotifyCleaningTagCollection(Sender: TObject; const Item: PTag; Action: TCollectionNotification);
begin
  if Assigned(Item) then
    DestroyTag(Item);
end;

// Should be moved to CollectTags() scope
function TLegendGenerator.CreateTag: PTag;
begin
  New(Result);
  Result.IsConditional := False;
  Result.ConditionalResult := False;
  Result.ContingentConditional := nil;
  Result.Next := nil;
  Result.PositionBegin := 0;
  Result.PositionEnd := 0;
  Result.TerminatorBegin := 0;
  Result.Namespace := 'TLegendGenerator'; // default for AssignData
  Result.ApplyFunction := False;
  Result.FunctionName := EmptyStr;

  Result.ValueField := EmptyStr;
  Result.Value := EmptyStr;
  Result.OriginalValue := EmptyStr;
  Result.Expression := EmptyStr;
  Result.TerminatorEnd := 0;
  Result.ConditionalDirective := EmptyStr;
  Result.ConditionalOperator := EmptyStr;
  Result.ConditionalValue := EmptyStr;
end;

// Should be moved to CollectTags() scope
function TLegendGenerator.isaOperator(Token: String): Boolean;
var
  operatorString: String;
  i: Integer;
begin
  Result := False;
  // for operatorString in validOperators do
  for i := 0 to High(validOperators) do
  begin
    operatorString := validOperators[i];
    if Token = operatorString then
    begin
      Result := True;
      Break;
    end;
  end;
end;

// Should be moved to CollectTags() scope
function TLegendGenerator.isaConditional(Token: String): Boolean;
var
  conditionalString: String;
  i: Integer;
begin
  Result := False;
  // for conditionalString in validConditionals do
  for i := 0 to High(validConditionals) do
  begin
    conditionalString := validConditionals[i];
    if Token = conditionalString then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TLegendGenerator.isValidTerminalConditional(Token: String) : Boolean;
begin
  Result := False;
  // expand this section when needed. for now hard code
  if (Token = 'endif') then
    Result := True;
end;

function TLegendGenerator.getNextWord(Position: Integer; Text: String): String;
var
  i: Integer;
begin
  i := Position;
  Result := '';
  while (Text[i] <> tokTagClose) and (Text[i] <> tokSpace) and (i <= Length(Text)) do
  begin
    Result := Result + Text[i];
    Inc(i);
  end;
end;

function TLegendGenerator.ExtractCitationName(tempStr: String): String;
var
  index: Integer = -1;
  aBegin: Integer = 1;
  aEnd: Integer = 1;
begin
  Result := EmptyStr;
  while (tempStr[aBegin] <> ':') and (aBegin < Length(tempStr)) do
    inc(aBegin);
  inc(aBegin);
  aEnd := aBegin;
  while (tempStr[aEnd] <> '}') and (aEnd < Length(tempStr)) do
    inc(aEnd);
  dec(aEnd);
  if (aEnd > aBegin) and (aEnd <= Length(tempStr)) then
    Result := Copy(tempStr, aBegin, aEnd - aBegin + 1);
  index := FTemplateConstants.IndexOfName(Result);
  if index >= 0 then
    Result := FTemplateConstants.ValueFromIndex[index];
end;

procedure TLegendGenerator.ClearTags;
var
  i: Integer = -1;
begin
  if Assigned(TemplateTags) then
  begin
    if TemplateTags.Count > 0 then
      for i := 0 to TemplateTags.Count - 1 do
        DestroyTag(TemplateTags[i]);
    TemplateTags.Clear;
  end;
end;

procedure TLegendGenerator.FlushTemplate(DataObject: TObject);
var
  CurTag: PTag;
  ClassName: String;
  i: Integer;
begin
  if (not Assigned(DataObject)) or (TemplateTags.Count = 0) then
    Exit;

  ClassName := DataObject.ClassName;
  // clear out all the values so we can bind new data
  for i := 0 to TemplateTags.Count - 1 do
  begin
    CurTag := TemplateTags[i];
    if CurTag.IsConditional and (CurTag.Namespace = ClassName) then
      CurTag.ConditionalResult := False;
  end;
  LoadConstants;
end;

procedure TLegendGenerator.RefreshConditionals;
var
  CurTag: PTag;
  ContingentTag: PTag;
  BlockTag: PTag;
  i: Integer;

  procedure RefreshBlock(var ABlockTag: PTag);
  var
    NextTag: PTag;
  begin
    while ABlockTag.Next <> nil do
    begin
      if ABlockTag.ConditionalResult = True then
      begin
        // the rest are false
        NextTag := ABlockTag.Next;
        while NextTag <> nil do
        begin
          NextTag.ConditionalResult := False;
          NextTag := NextTag.Next;
        end;
        Break;
      end
      else
        ABlockTag := ABlockTag.Next;
    end;
  end;

begin
  try
    if TemplateTags.Count = 0 then
      Exit;
    // reset all the {else}
    for i := 0 to TemplateTags.Count - 1 do
    begin
      CurTag := TemplateTags.Items[i];
      if CurTag.IsConditional and (CurTag.ConditionalDirective = 'else') then
        CurTag.ConditionalResult := True;
    end;

    for i := 0 to TemplateTags.Count - 1 do
    begin
      CurTag := TemplateTags.Items[i];
      if CurTag.IsConditional and (CurTag.ConditionalDirective = 'if') then
      begin
        if CurTag.ContingentConditional <> nil then
        begin
          ContingentTag := CurTag.ContingentConditional;
          if ContingentTag.ConditionalResult = False then
          begin
            CurTag.ConditionalResult := False;
            // null this whole set
            BlockTag := CurTag;
            while BlockTag <> nil do
            begin
              BlockTag.ConditionalResult := False;
              BlockTag := BlockTag.Next;
            end;
          end
          else
            RefreshBlock(CurTag);
        end
        else
        begin // no contingent conditional
          RefreshBlock(CurTag);
        end;
      end;
    end;
  except
    on E:Exception do
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Oh no! An error occurred while generating a caption: ' + E.Message);
      {$ELSE}
      warn_nv('An error occurred while generating a caption: ' + E.Message);
      {$ENDIF}
  end;
end;

procedure TLegendGenerator.AssignData(templateVariableName, Value: String);
var
  i: Integer;
  CurTag: PTag;
begin
  for i := 0 to TemplateTags.Count - 1 do
  begin
    CurTag := TemplateTags.Items[i];
    if (CurTag.Namespace = ClassName) and (CurTag.ValueField = templateVariableName) then
    begin
      CurTag.Value := Value;
      CurTag.OriginalValue := CurTag.Value;
      // if it's a conditional do a simple string comparison and set the conditional result
      if CurTag.IsConditional then
        if CurTag.ConditionalValue = CurTag.Value then
          CurTag.ConditionalResult := True
        else if (SameText(templateVariableName, DISP_DIV_TIMES_STR)) or
          (SameText(templateVariableName, DISP_ERR_BARS_STR)) then
        begin
          { temporary fix until I have time to make sure I don't break the system - GS }
          if SameText(Value, 'False') then
            CurTag.ConditionalResult := False
          else
            CurTag.ConditionalResult := True;
        end;
    end;
  end;
end;

function TLegendGenerator.HasData(templateVariableName: String): Boolean;
var
  i: Integer = -1;
  CurTag: PTag = nil;
begin
  Result := False;
  if (not Assigned(TemplateTags)) or (TemplateTags.Count = 0) then
    Exit;

  for i := 0 to TemplateTags.Count - 1 do
  begin
    CurTag := TemplateTags[i];
    if SameText(CurTag.ValueField, templateVariableName) then
      Exit(True);
  end;
end;

{ IMPORTANT: if you are looking for MegaCitation it is defined in MTemplateConstants.ini}
function TLegendGenerator.LoadCitationFromFile(HelpTopic: String): String;
var
  CitationHTML: TStringList = nil;
  ReferenceHTML: String = '';
  FullCitationFilePath: String = '';
  PosBegin, PosEnd, i, dupePos: Integer;
  isDupe: Boolean = False;
begin
  Result := EmptyStr;
  CitationHTML := TStringList.Create;
  FullCitationFilePath := GetPrivateFile(mfCitationDir + HelpTopic + HTML_FILE_EXT);
  try
    if FileExists(FullCitationFilePath) then
    begin
      CitationHTML.LoadFromFile(FullCitationFilePath);
      PosBegin := Pos('<P class="reference" >', CitationHTML.Text);
      if PosBegin = 0 then
        PosBegin := Pos('<P>', CitationHTML.Text);
      PosEnd := Pos('</P>', CitationHTML.Text);
      ReferenceHTML := copy(CitationHTML.Text, PosBegin, PosEnd - PosBegin);

      // need to rewrap it to get the superscript integrated
      PosBegin := Pos('>', ReferenceHTML);
      ReferenceHTML := copy(ReferenceHTML, PosBegin + 1, length(ReferenceHTML) -
        PosBegin);
      ReferenceHTML := '<P class="reference" >$refNumber. </i> ' +
        ReferenceHTML + '</P>';

      // check if this citation already exists in the list
      isDupe := False;
      for i := 0 to CitationList.Count - 1 do
      begin
        if CitationList[i] = ReferenceHTML then
        begin
          isDupe := True;
          dupePos := i;
          Break;
        end;
      end;

      if isDupe then
        Result := IntToStr(dupePos + 1)
      else
      begin
        CitationList.Add(ReferenceHTML);
        Result := IntToStr(CitationList.Count);
      end;
    end
    else
    begin
      if ExtractFileName(FullCitationFilePath) = '.htm' then
        raise Exception.Create('Your MEGA installation is missing files required to properly generate citations for captions. Please report this bug to the MEGA authors.')
      else
        raise Exception.Create('Your computer appears to be missing files required to properly generate citations for captions. Missing file is ' + FullCitationFilePath + '. Please report this bug to the MEGA authors.');
    end;
  finally
    if Assigned(CitationHTML) then
      CitationHTML.Free;
  end;
end;

procedure TLegendGenerator.HandleFunctionCall(Tag: PTag);
var
  testInteger: Integer;
begin
  if Tag.FunctionName = 'ExpandCitation' then
  begin
    try
      ExpandCitation(Tag);
    except
      on E: Exception do
        ShowWarningMessage('Warning: Unable to load citation for ' +
          Tag.ValueField + ' : ' + Tag.Value);
    end;
  end
  else if Tag.FunctionName = 'HumanizeArrayIndex' then
  begin
    if Tag.IsConditional or Assigned(Tag.ContingentConditional) then
      if Tag.ContingentConditional.ConditionalResult <> True then
        Exit;

    if TryStrToInt(Tag.Value, testInteger) then
    begin
      // This might be incorrect if we allow multiple functions in the future
      if (Tag.Value = Tag.OriginalValue) then
        Tag.Value := IntToStr(StrToInt(Tag.Value) + 1);
    end;
  end
  else if Tag.FunctionName = 'ExtractCaptionInfo' then
  begin
    if Pos(VER_MEGA_WIN_CAPTION_PREFIX + ':', Tag.Value) > 0 then
      Tag.Value := copy(Tag.Value, Pos(VER_MEGA_WIN_CAPTION_PREFIX + ':',
        Tag.Value) + 4, 255);
  end
  else
    ShowWarningMessage('Warning: Caption Expert could not call the function "' +
      Tag.FunctionName + '" on NameSpace:' + Tag.Namespace +
      ' , Original Value: ' + Tag.OriginalValue);
end;

procedure TLegendGenerator.ExpandCitation(tag: PTag);
begin
  if Tag.Value <> Tag.OriginalValue then
    Tag.Value := Tag.OriginalValue;
  if Tag.IsConditional or Assigned(Tag.ContingentConditional) then
  begin
    if Tag.ContingentConditional.ConditionalResult <> True then
      Exit
    else
      Tag.Value := LoadCitationFromFile(Tag.Value);
  end
  else
    Tag.Value := LoadCitationFromFile(Tag.Value);
end;

function TLegendGenerator.LoadConstants: Boolean;
var
  Filename: String = '';
  i: Integer = -1;
begin
  Result := False;
  try
    // Load the constant template values from the ini store
    if not Assigned(FTemplateConstants) then
      FTemplateConstants := TStringList.Create
    else
      FTemplateConstants.Clear;
    FileName := GetPrivateFile(mfTemplateConstants);
    if FileExists(FileName) then
    begin
      FTemplateConstants.LoadFromFile(Filename);
      for i := 0 to FTemplateConstants.Count - 1 do
        AssignData(FTemplateConstants.Names[i], FTemplateConstants.Values[FTemplateConstants.Names[i]]);
      AssignData('InDevelopment',
        '<h2>This template is still under development and the results can not be considered to be accurate.  This template will be corrected by the next release.</h2>');
      Result := True;
    end
    else
      raise Exception.Create('missing caption template file');
  except
    on E:Exception do
      {$IFDEF VISUAL_BUILD}
       ShowMessage('Oh no! An error occurred while generating a caption: ' + E.Message);
      {$ELSE}
       warn_nv('An error occurred while generating a caption: ' + E.Message);
      {$ENDIF}
  end;
end;

function TLegendGenerator.LoadStyleSheet: Boolean;
var
  StyleContainer: TStringList;
begin
  StyleContainer := nil;
  Result := False;
  try
    try
      StyleContainer := FStyleSheet.GetStyleSheet;
      AssignData('Style', StyleContainer.Text);
      Result := True;
    except
      on E:Exception do
        {$IFDEF VISUAL_BUILD}
        ShowMessage('Oh no! An error occurred while generating a caption: ' + E.Message);
        {$ELSE}
        warn_nv('An error occurred while generating a caption: ' + E.Message);
        {$ENDIF}
    end;
  finally
    if Assigned(StyleContainer) then
      StyleContainer.Free;
  end;
end;

procedure TLegendGenerator.GenerateLegendAsText(var StringList: TStringList);
begin
  GenerateNoteAsText(StringList);
end;

procedure TLegendGenerator.GenerateNoteAsText(var StringList: TStringList);
var
  ExtractedText: AnsiString = '';
  GeneratedLegend: AnsiString = '';
  {$IFDEF VISUAL_BUILD}
  HTMLViewer: THTMLViewer = nil;
  {$ENDIF}
  BeforeTable, AfterTable, LowerCaseGeneratedLegend: String;
begin
  if StringList = nil then
    Exit;

  try
    GeneratedLegend := GenerateLegend;
    LowerCaseGeneratedLegend := LowerCase(GeneratedLegend);
    BeforeTable := copy(GeneratedLegend, 0, Pos('<table', LowerCaseGeneratedLegend) - 1);
    AfterTable := copy(GeneratedLegend, Pos('</table>', LowerCaseGeneratedLegend) + 8, length(GeneratedLegend));
    GeneratedLegend := BeforeTable + AfterTable;
    {$IFDEF VISUAL_BUILD}
    HTMLViewer := THTMLViewer.CreateParented(Application.MainForm.Handle);
    HTMLViewer.Visible := False;
    HtmlViewer.LoadFromString(GeneratedLegend);
    ExtractedText := HTMLViewer.GetTextByIndices(0, 999999999);
    splitStr(ExtractedText, #$D#$A, StringList);
    {$ELSE}
    splitStr(GeneratedLegend, #$D#$A, StringList);
    {$ENDIF}
  finally
    {$IFDEF VISUAL_BUILD}
    if Assigned(HTMLViewer) then
      HTMLViewer.Free;
    {$ENDIF}
  end;
end;

function TLegendGenerator.PostProcessCitations(tempResult: String): String;
var
  toReplace: String = '';
  newText: String = '';
  aBegin: Integer = -1;
  aEnd: Integer = -1;
  aCitation: String = '';
begin
  Result := tempResult;
  while Pos(POST_PROCESS_CITATION, Result) > 0 do
  begin
    aBegin := Pos(POST_PROCESS_CITATION, Result) - 1;
    aEnd := aBegin;
    while (Result[aEnd] <> '}') and (aEnd < Length(Result)) do
      inc(aEnd);
    toReplace := Copy(Result, aBegin, aEnd - aBegin + 1);
    aCitation := ExtractCitationName(toReplace);
    newText := LoadCitationFromFile(aCitation);
    Result := StringReplace(Result, toReplace, newText, []);
  end;
end;

function TLegendGenerator.GetPropertyValueAsString(Instance: TObject; PropName: String): String;
var
  PropInfo: PPropInfo;
  tempInt: Integer = -1;
begin
  PropInfo := GetPropInfo(Instance, PropName);
  if PropInfo <> nil then
  begin
    case PropInfo^.PropType^.Kind of
      tkBool:
        begin
          Result := BoolToStr(GetPropValue(Instance, PropName), True);
        end;
      tkInteger, tkInt64:
        begin
          if TryStrToInt(GetPropValue(Instance, PropName), tempInt) then
            Result := Format('%.0n', [tempInt*1.0])
          else
            Result := String(GetPropValue(Instance, PropName));
        end
      else
      begin
        Result := String(GetPropValue(Instance, PropName));
      end;
    end;
  end;
end;

end.
