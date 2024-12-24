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

unit nexustreeconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nexusfile, nexusblock;

type

  { TNexusTreeConverter }

  TNexusTreeConverter = class(TObject)
    private

    protected
      FLogStrings: TStringList;
      FNexusFile: TNexusFile;
      function GetTranslatedNewick(const treeBlock: TTreeBlock; var newick: TStringList): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function ConvertNexusToNewick(const Filename: String; var NewickStrings: TStringList): Boolean;

      property LogStrings: TStringList read FLogStrings;
  end;

implementation

uses
  MTreeList;

{ TNexusTreeConverter }

function TNexusTreeConverter.GetTranslatedNewick(const treeBlock: TTreeBlock; var newick: TStringList): Boolean;
var
  aTreeList: TTreeList = nil;
  i: Integer;
  temp, tempNewick: String;
  translation: TStringList = nil;
begin
  Result := False;
  tempNewick := EmptyStr;
  if (treeBlock.NumTrees = 0) or (not treeBlock.HasTranslation) then
    Exit;

  try
    Result := True;
    aTreeList := TTreeList.Create;
    for i := 0 to treeBlock.NumTrees - 1 do
    begin
      if treeBlock.GetNewickString(i, temp) then
        tempNewick := Trim(tempNewick + LineEnding + temp)
      else
        Result := False;
    end;
    aTreeList.ImportFromNewick(tempNewick, nil, False);
    if Result then
    begin
      translation := treeBlock.Translation;
      if aTreeList.TranslateOtuNames(translation) then
      begin
        for i := 0 to aTreeList.NoOfTrees - 1 do
        begin
          temp := aTreeList.OutputNewickTree(i, True, True, 0.0);
          newick.Add(temp);
        end;
      end;
    end;
  finally
    if Assigned(aTreeList) then
      aTreeList.Free;
    if Assigned(translation) then
      translation.Free;
  end;
end;

constructor TNexusTreeConverter.Create;
begin
  FLogStrings := TStringList.Create;
  FNexusFile := TNexusFile.Create(False);
end;

destructor TNexusTreeConverter.Destroy;
begin
  if Assigned(FLogStrings) then
    FLogStrings.Free;
  if Assigned(FNexusFile) then
    FNexusFile.Free;
  inherited Destroy;
end;

function TNexusTreeConverter.ConvertNexusToNewick(const Filename: String; var NewickStrings: TStringList): Boolean;
var
  i, j: Integer;
  newick: String;
  aTreeList: TTreeList = nil;
  treeBlock: TTreeBlock = nil;
begin
  Result := False;
  NewickStrings.Clear;
  if not FileExists(Filename) then
  begin
    FLogStrings.Add('input file not found: ' + Filename);
    Exit;
  end;

  FNexusFile.Filename := Filename;
  if not FNexusFile.Parse then
  begin
    FLogStrings.Add('failed to parse NEXUS file');
    FLogStrings.Add(FNexusFile.ErrorString);
    Exit;
  end;

  if FNexusFile.NumTrees = 0 then
  begin
    FLogStrings.Add('no trees found in NEXUS file');
    Exit;
  end;

  Result := True;
  for i := 0 to FNexusFile.NumTreeBlocks -1 do
  begin
    treeBlock := FNexusFile.GetTreeBlock(i);
    if treeBlock.HasTranslation then
      Result := Result and GetTranslatedNewick(treeBlock, NewickStrings)
    else
      for j := 0 to treeBlock.NumTrees - 1 do
      begin
        if treeBlock.GetNewickString(j, newick) then
          NewickStrings.Add(newick);
      end;
  end;
  Result := Result and (NewickStrings.Count > 0);
end;

end.

