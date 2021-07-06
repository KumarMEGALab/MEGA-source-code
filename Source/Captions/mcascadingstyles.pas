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

unit mcascadingstyles;

interface

uses
  SysUtils, Classes;

type
  TStyleType = (stGlobal, stClass, stId);

  TStyle = class(TObject)
  private
    FName: String;
    FType: TStyleType;
    procedure SetName(const Value: String);
    procedure SetStyleType(const Value: TStyleType);
    protected

    public
      Values: TStringList;
      constructor Create(AName: String; AType: TStyleType = stGlobal);
      destructor Destroy; override;
      procedure Add(AValue: String);
      property StyleType: TStyleType read FType write SetStyleType;
      property Name: String read FName write SetName;

      function StyleString: String;
  end;


  TStyleSheet = class(TObject)
  private
    FbodyFontSize: Integer;
    FStyles: TList;
    FFontName: String;
    FReferenceFontSize: Integer;
    FFigureTitleFontSize: Integer;
    FNoteFontSize: Integer;
    procedure SetFontName(const Value: String);
    procedure SetBodyFontSize(const Value: Integer);
    function GetStyle(Index: Integer): TStyle;
    procedure SetFigureTitleFontSize(const Value: Integer);
    procedure SetNoteFontSize(const Value: Integer);
    procedure SetReferenceFontSize(const Value: Integer);
    protected

    public
      constructor Create;
      destructor Destroy; override;
      procedure ClearStyles;
      procedure RefreshStyleSheet;
      function HasStyle(StyleName: String): Boolean;
      function GetStyleByName(StyleName: String): TStyle;
      function GetStyleSheet: TStringList;
      procedure IncrementFontSizes;
      procedure DecrementFontSize;
      property FontName: String read FFontName write SetFontName;
      property BodyFontSize: Integer read FbodyFontSize write SetBodyFontSize;
      property ReferenceFontSize: Integer read FReferenceFontSize write SetReferenceFontSize;
      property NoteFontSize: Integer read FNoteFontSize write SetNoteFontSize;
      property FigureTitleFontSize: Integer read FFigureTitleFontSize write SetFigureTitleFontSize;
      property Items[Index: Integer]: TStyle read GetStyle; default;


  end;
implementation

{ TStyle }

procedure TStyle.Add(AValue: String);
var
  aVal: String;
begin
  if not (AValue[Length(AValue)] = ';') then
    aVal := AValue + ';'
  else
    aVal := AValue;
  Values.Add(aVal);
end;

constructor TStyle.Create(AName: String; AType: TStyleType = stGlobal);
begin
  Values := TStringList.Create;
  StyleType := AType;
  FName := AName;
end;

destructor TStyle.Destroy;
begin
  if Assigned(Values) then
    Values.Free;
  inherited;
end;

procedure TStyle.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TStyle.SetStyleType(const Value: TStyleType);
begin
  FType := Value;
end;

function TStyle.StyleString: String;
var
  i: Integer;
begin
  case FType of
    stGlobal: Result := EmptyStr;
    stClass: Result := '.';
    stId: Result := '#';
  end;
  Result := Result + FName +' {';
  if Values.Count > 0 then
    for i := 0 to Values.Count - 1 do
      Result := Result + Values[i];
  Result := Result + '}';
end;

{ TStyleSheet }

procedure TStyleSheet.ClearStyles;
var
  i: Integer;
begin
  if FStyles.Count > 0 then
    for i := 0 to FStyles.Count - 1 do
    begin
      TStyle(FStyles[i]).Free;
    end;
  FStyles.Clear;
end;

constructor TStyleSheet.Create;
begin
  FStyles := TList.Create;
  FbodyFontSize := 12;
  FNoteFontSize := 14;
  FFigureTitleFontSize := 16;
  FReferenceFontSize := 12;
  FFontName := 'Arial, Helvetica, sans-serif';
  RefreshStyleSheet;
end;

procedure TStyleSheet.DecrementFontSize;
begin
  if FBodyFontSize = 6 then
    Exit;
  dec(FbodyFontSize);
  dec(FReferenceFontSize);
  dec(FNoteFontSize);
  dec(FFigureTitleFontSize);
  RefreshStyleSheet;
end;

destructor TStyleSheet.Destroy;
begin
  if Assigned(FStyles) then
  begin
    ClearStyles;
    FStyles.Free;
  end;
  inherited;
end;

function TStyleSheet.GetStyle(Index: Integer): TStyle;
begin
  if (Index >= 0) and (Index < FStyles.Count) then
    Result := TStyle(FStyles[Index])
  else
    Result := nil;
end;

function TStyleSheet.GetStyleByName(StyleName: String): TStyle;
var
  i: Integer;
begin
  Result := nil;
  if FStyles.Count > 0 then
    for i := 0 to FStyles.Count - 1 do
    begin
      if TStyle(FStyles[i]).Name = StyleName then
      begin
        Result := TStyle(FStyles[i]);
        Exit;
      end;
    end;
end;

function TStyleSheet.GetStyleSheet: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if FStyles.Count > 0 then
    for i := 0 to FStyles.Count - 1 do
      Result.Add(TStyle(FStyles[i]).StyleString);
end;

function TStyleSheet.HasStyle(StyleName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FStyles.Count > 0 then
    for i := 0 to FStyles.Count - 1 do
    begin
      if TStyle(FStyles[i]).Name = StyleName then
      begin
        Result := True;
        Exit;
      end;
    end;
end;

procedure TStyleSheet.IncrementFontSizes;
begin
  if FBodyFontSize = 28 then
    Exit;
  inc(FbodyFontSize);
  inc(FReferenceFontSize);
  inc(FNoteFontSize);
  inc(FFigureTitleFontSize);
  RefreshStyleSheet;
end;

procedure TStyleSheet.RefreshStyleSheet;
var
  aStyle: TStyle;
begin
  ClearStyles;
  aStyle := TStyle.Create('body');
  aStyle.Add(Format('font-family: %s;', [FFontName]));
  aStyle.Add('font-size: ' + IntToStr(FbodyFontSize) + 'px;');
  aStyle.Add('background-color: #ffffff;');
  aStyle.Add('margin-left: 10px;');
  aStyle.Add('margin-right: 10px;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('th');
  aStyle.Add('border-top: 2px solid black;');
  aStyle.Add('border-bottom: 1px solid black;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('td');
  aStyle.Add('padding: 5px;');
  aStyle.Add('text-align: center;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('table');
  aStyle.Add('border-collapse: collapse;');
  aStyle.Add('width: 480px;');
  aStyle.Add('font-family: ' + FFontName + ';');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('content', stId);
  aStyle.Add('text-align: justify;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('reference', stClass);
  aStyle.Add('font-size: ' + IntToStr(FReferenceFontSize) + 'px;');
  aStyle.Add('margin: 2px 0px 2px 0px;');
  aStyle.Add('text-indent: -1em;');
  aStyle.Add('padding-left: 1em;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('rowHeader', stClass);
  aStyle.Add('font-weight: bold;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('lastRow td', stClass);
  aStyle.Add('border-bottom: 2px solid black;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('figureTitle', stClass);
  aStyle.Add('font-weight: bold;');
  aStyle.Add('font-family: ' + FFontName + ';');
  aStyle.Add('font-size: ' + IntToStr(FFigureTitleFontSize) + 'px;');
  aStyle.Add('padding: 2px;');
  aStyle.Add('text-align: left;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('note', stClass);
  aStyle.Add('font-size: ' + IntToStr(FNoteFontSize) + 'px;');
  aStyle.Add('padding: 2px;');
  aStyle.Add('text-align: justify;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('highlighted', stClass);
  aStyle.Add('font-weight:bold;');
  FStyles.Add(aStyle);

  aStyle := TStyle.Create('preventwrap td', stClass);
  aStyle.Add('white-space:nowrap;');
  FStyles.Add(aStyle);
end;

procedure TStyleSheet.SetFigureTitleFontSize(const Value: Integer);
begin
  FFigureTitleFontSize := Value;
end;

procedure TStyleSheet.SetFontName(const Value: String);
begin
  FFontName := Value;
end;

procedure TStyleSheet.SetNoteFontSize(const Value: Integer);
begin
  FNoteFontSize := Value;
end;

procedure TStyleSheet.SetReferenceFontSize(const Value: Integer);
begin
  FReferenceFontSize := Value;
end;

procedure TStyleSheet.SetBodyFontSize(const Value: Integer);
begin
  FbodyFontSize := Value;
end;

end.
