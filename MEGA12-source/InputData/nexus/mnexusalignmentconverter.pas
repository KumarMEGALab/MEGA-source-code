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

unit mnexusalignmentconverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nexusfile;

type

  { TNexusAlignmentConverter }

  TNexusAlignmentConverter = class(TObject)
    private
      FLogStrings: TStringList;

    protected
      FNexusFile: TNexusFile;
    public
      constructor Create;
      destructor Destroy; override;

      function ConvertNexusToMegaAlignment(const Filename: String; var MegaAlignment: TStringList): Boolean;
      property LogStrings: TStringList read FLogStrings;
  end;

implementation

uses
  nexusblock;

{ TNexusAlignmentConverter }

constructor TNexusAlignmentConverter.Create;
begin
  FNexusFile := TNexusFile.Create(False);
  FLogStrings := TStringList.Create;
end;

destructor TNexusAlignmentConverter.Destroy;
begin
  if Assigned(FNexusFile) then
    FNexusFile.Free;
  if Assigned(FLogStrings) then
    FLogStrings.Free;
  inherited Destroy;
end;

function TNexusAlignmentConverter.ConvertNexusToMegaAlignment(const Filename: String; var MegaAlignment: TStringList): Boolean;
var
  aBlock: TCharactersBlock = nil;
  tempStrings: TStringList = nil;
begin
  Result := False;
  FNexusFile.Filename := Filename;
  if not FNexusFile.Parse then
  begin
    FLogStrings.Add(FNexusFile.ErrorString);
    Exit;
  end;
  if FNexusFile.NumCharacterBlocks = 0 then
  begin
    FLogStrings.Add('alignment data not found in NEXUS file');
    Exit;
  end
  else if FNexusFile.NumCharacterBlocks > 1 then
  begin
    FLogStrings.Add(Format('multiple sequence data blocks are not supported but %d CHARACTER/DATA blocks were found', [FNexusFile.NumCharacterBlocks]));
    Exit;
  end;
  try
    MegaAlignment.Clear;
    aBlock := FNexusFile.GetCharacterBlock(0);
    tempStrings := aBlock.GetMegaSequenceAlignmentFileStrings;
    MegaAlignment.Assign(tempStrings);
    Result := True;
  finally
    if Assigned(tempStrings) then
      tempStrings.Free;
  end;
end;

end.

