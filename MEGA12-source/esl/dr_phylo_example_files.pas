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

unit dr_phylo_example_files;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Zipper;

type

  { TDrPhyloExampleFiles }

  TDrPhyloExampleFiles = class(TThread)

    protected
      FUnzipper: TUnZipper;
      procedure Execute; override;

    public
      IsSuccess: Boolean;
      ErrMsg: String;
      constructor Create;
      destructor Destroy; override;
  end;

implementation

uses
  MegaUtils, MExampleFiles, Forms;

{ TDrPhyloExampleFiles }

procedure TDrPhyloExampleFiles.Execute;
var
  sourceArchive: String = '';
  expectedFile: String = '';
  destinationFolder: String = '';
begin
  IsSuccess := False;
  expectedFile := GetExampleFilesFolderName + PathDelim + 'DrPhylo' + PathDelim + 'Fungi_example_slow' + PathDelim + 'Rooted_Fungi_Tree.nwk'; // if this file exists, assume all files already extracted
  if FileExists(expectedFile) then
  begin
    IsSuccess := True;
    Exit;
  end;
  {$IFDEF LINUX}
    {$IFDEF DEBUG}
    sourceArchive := GetPrivateFile('Private' + PathDelim + 'DrPhylo' + PathDelim + 'DrPhylo_Example.zip', False);
    {$ELSE}
    sourceArchive := '/usr/lib/mega/DrPhylo_Example.zip';
    {$ENDIF}
  {$ELSE}
    { see TEslLinker.ExtractDrPhyloArchive for an explanation of why this is different on Windows}
    {$IFDEF MSWINDOWS}
      {$IFDEF DEBUG}
      sourceArchive := GetPrivateFile('Private' + PathDelim + 'DrPhyloExample' + PathDelim + 'DrPhylo_Example.zip', False);
      {$ELSE}
      sourceArchive := ExtractFilePath(Application.ExeName) + 'DrPhylo_Example.zip';
      {$ENDIF}


    {$ELSE}
    sourceArchive := GetPrivateFile('Private' + PathDelim + 'DrPhyloExample' + PathDelim + 'DrPhylo_Example.zip', False);
    {$ENDIF}
  {$ENDIF}

  if FileExists(sourceArchive) then
  begin
    destinationFolder := GetExampleFilesFolderName;
    FUnzipper.Filename := sourceArchive;
    FUnzipper.OutputPath := destinationFolder;
    FUnzipper.Examine;
    FUnzipper.UnZipAllFiles;
    IsSuccess := FileExists(expectedFile);
    if not IsSuccess then
      ErrMsg := Format('failed to extract DrPhylo example files sourceArchive: %s', [sourceArchive]);
  end
  else
    ErrMsg := Format('missing DrPhylo example files sourceArchive: %s', [sourceArchive]);
end;

constructor TDrPhyloExampleFiles.Create;
begin
  inherited Create(True);
  IsSuccess := False;
  FUnzipper := TUnZipper.Create;
end;

destructor TDrPhyloExampleFiles.Destroy;
begin
  if Assigned(FUnzipper) then
    FUnzipper.Free;
  inherited Destroy;
end;

end.

