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

unit MExampleFiles;

interface

uses
  Classes, SysUtils;

type
  TExampleFiles = class(TObject)
    private
      BookFilesDirectory: String;
      procedure InitExampleFilesList;
      procedure InitBookFilesList;
      function CheckExampleFilesFolder: Boolean;
      function BookFilesFolderName: String;
      function SystemExampleFilename(Basename: String): String;
      function SystemBookFilename(Basename: String): String;
      function ResExampleFilename(Basename: String): String;
      function ResBookFilename(Basename: String): String;
    public
      ExampleFilesList: TStringList;
      BookFilesList: TStringList;
      constructor Create;
      destructor Destroy; override;
      function ExampleFilesFolderName: String;
      function CheckExampleFiles: Boolean;
  end;

  function GetExampleFilesFolderName: String;

implementation

uses
  MegaUtils, Dialogs, Forms, FileUtil;

function GetExampleFilesFolderName: String;
begin
  {$IFDEF DEBUG}
    {$IFDEF DARWIN}
     Result := GetHomeDirectory + 'MEGA X' + PathDelim + 'Examples';
    {$ELSE}
    Result := ExtractFileDir(Application.ExeName) + PathDelim + '..' + PathDelim + 'MEGA7_Install' + PathDelim + 'Private' + PathDelim + 'Examples';
    Result := ExpandFileName(Result);
    {$ENDIF}
  {$ELSE}
  Result := GetHomeDirectory + 'MEGA X' + PathDelim + 'Examples';
  {$ENDIF}
end;

{ TExampleFiles }

function TExampleFiles.BookFilesFolderName: String;
begin
  Result := ExampleFilesFolderName + PathDelim + BookFilesDirectory;
end;

function TExampleFiles.CheckExampleFiles: Boolean;
var
  i: Integer;
  Filename: String;
begin
  Result := True;
  try
    if CheckExampleFilesFolder then { will try to create it if it does not exist}
    begin
      if ExampleFilesList.Count > 0 then { check files that go in the Examples folder}
      begin
        for i := 0 to ExampleFilesList.Count - 1 do
        begin
          Filename := SystemExampleFilename(ExampleFilesList[i]);
          if not FileExists(Filename) then
          begin
            CopyFileFromRES(ResExampleFilename(ExampleFilesList[i]), Filename, True); { extracts the file from the compiled resources and writes it to disk}
            Result := Result and FileExists(Filename);
          end;
        end;
      end;

      if BookFilesList.Count > 0 then { check files that go in the Examples\NeiKumar2000 folder}
      begin
        for i := 0 to BookFilesList.Count - 1 do
        begin
          Filename := SystemBookFilename(BookFilesList[i]);
          if not FileExists(Filename) then
          begin
            CopyFileFromRES(ResBookFilename(BookFilesList[i]), Filename, True); { extracts the file from the compiled resources and writes it to disk}
            Result := Result and FileExists(Filename);
          end;
        end;
      end;
    end
    else
      Result := False;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to copy example files: ' + E.Message);
    end;
  end;
end;

function TExampleFiles.CheckExampleFilesFolder: Boolean;
var
  FilesDir: String;
begin
  Result := False;
  try
    FilesDir := BookFilesFolderName;
    if not DirectoryExists(FilesDir) then
      Result := ForceDirectories(FilesDir)
    else
      Result := True;
  except
    on E:Exception do
      ShowMessage('Failed to copy example files: ' + E.Message);
  end;
end;

constructor TExampleFiles.Create;
begin
  BookFilesDirectory := 'NeiKumar2000';
  InitExampleFilesList;
  InitBookFilesList;
end;

destructor TExampleFiles.Destroy;
begin
  if Assigned(ExampleFilesList) then
    ExampleFilesList.Free;
  if Assigned(BookFilesList) then
    BookFilesList.Free;
  inherited;
end;

function TExampleFiles.ExampleFilesFolderName: String;
begin
  Result := GetExampleFilesFolderName;
end;

procedure TExampleFiles.InitBookFilesList;
begin
  BookFilesList := TStringList.Create;
  BookFilesList.Add('Pg024_Chap_02_Exp_01_Data.meg');
  BookFilesList.Add('Pg027_Chap_02_Exp_02_Data.meg');
  BookFilesList.Add('Pg041_Chap_03_Exp_01_Data.meg');
  BookFilesList.Add('Pg059_Chap_04_Exp_01_Data.meg');
  BookFilesList.Add('Pg066_Chap_04_Exp_02_Data.meg');
  BookFilesList.Add('Pg067_Chap_04_Exp_03_Data.meg');
  BookFilesList.Add('Pg068_Chap_04_Fig_02_Data.meg');
  BookFilesList.Add('Pg079_Chap_05_Fig_05_Data.meg');
  BookFilesList.Add('Pg090_Chap_06_Exp_01_Data.meg');
  BookFilesList.Add('Pg092_Chap_06_Fig_02_Data.meg');
  BookFilesList.Add('Pg102_Chap_06_Exp_02_Data.meg');
  BookFilesList.Add('Pg109_Chap_06_Exp_03_Data.meg');
  BookFilesList.Add('Pg117_Chap_07_Fig_01_Data.meg');
  BookFilesList.Add('Pg117_Chap_07_Fig_01_Tree.nwk');
  BookFilesList.Add('Pg121_Chap_07_Exp_01_Data.meg');
  BookFilesList.Add('Pg134_Chap_07_Exp_02_Data.meg');
  BookFilesList.Add('Pg136_Chap_07_Exp_03_Data.meg');
  BookFilesList.Add('Pg138_Chap_07_Exp_04_Data.meg');
  BookFilesList.Add('Pg151_Chap_08_Exp_01_Data.meg');
  BookFilesList.Add('Pg156_Chap_08_Exp_02_Data.meg');
  BookFilesList.Add('Pg158_Chap_08_Exp_03_Data.meg');
  BookFilesList.Add('Pg174_Chap_09_Exp_01_Data.meg');
  BookFilesList.Add('Pg183_Chap_09_Fig_04_Data.meg');
  BookFilesList.Add('Pg189_Chap_10_Fig_01_Data.meg');
  BookFilesList.Add('Pg195_Chap_10_Exp_01_Data.meg');
  BookFilesList.Add('Pg200_Chap_10_Exp_02_Data.meg');
  BookFilesList.Add('Pg204_Chap_10_Exp_03_Data.meg');
  BookFilesList.Add('Pg212_Chap_11_Exp_01_Data.GZF');
  BookFilesList.Add('Pg212_Chap_11_Exp_01_Data.meg');
  BookFilesList.Add('Pg212_Chap_11_Exp_01_Tree.nwk');
  BookFilesList.Add('Pg219_Chap_11_Exp_02_Data.GZF');
  BookFilesList.Add('Pg219_Chap_11_Exp_02_Data.meg');
  BookFilesList.Add('Pg219_Chap_11_Exp_02_Data.nwk');
  BookFilesList.Add('Pg226_Chap_11_Exp_03_Data.meg');
  BookFilesList.Add('Pg226_Chap_11_Exp_03_Tree.nwk');
  BookFilesList.Add('Pg253_Chap_12_Exp_01_Data.meg');
  BookFilesList.Add('Pg258_Chap_12_Exp_02_Data.meg');
  BookFilesList.Add('Pg273_Chap_13_Exp_01_Data.dat');
  BookFilesList.Add('Pg282_Chap_13_Exp_02_Ppa.dat');
  BookFilesList.Add('Pg282_Chap_13_Exp_02_Ptr.dat');
  BookFilesList.Add('README.txt');
end;

procedure TExampleFiles.InitExampleFilesList;
begin
  ExampleFilesList := TStringList.Create;
  ExampleFilesList.Add('ABI01.abi');
  ExampleFilesList.Add('Chloroplast_Martin.meg');
  ExampleFilesList.Add('Contigs.meg');
  ExampleFilesList.Add('Crab_rRNA.meg');
  ExampleFilesList.Add('D-loop_Vigilant.meg');
  ExampleFilesList.Add('Distance_Data.meg');
  ExampleFilesList.Add('Drosophila_Adh.meg');
  ExampleFilesList.Add('HLA-3Seq.meg');
  ExampleFilesList.Add('hsp20.fas');
  ExampleFilesList.Add('hsp20.meg');
  ExampleFilesList.Add('Hum_Dist.meg');
  ExampleFilesList.Add('mtCDNA.meg');
  ExampleFilesList.Add('mtCDNA.nwk');
  ExampleFilesList.Add('mtCDNACalibration.txt');
  ExampleFilesList.Add('mtCDNACalibrationDensities.txt');
  ExampleFilesList.Add('mtCDNAOutgroup.txt');
  ExampleFilesList.Add('NewickTree.nwk');
  ExampleFilesList.Add('gene_tree.nwk');
  ExampleFilesList.Add('species_tree.nwk');
  ExampleFilesList.Add('taxa_to_species_map.txt');
  ExampleFilesList.Add('command_statements.meg');
  ExampleFilesList.Add('command_statements_dists.meg');
  ExampleFilesList.Add('reltime_with_dated_tips_RTDT_alignment.meg');
  ExampleFilesList.Add('reltime_with_dated_tips_RTDT_tree.nwk');
  ExampleFilesList.Add('reltime_with_dated_tips_RTDT_outgroup.txt');
  ExampleFilesList.Add('reltime_with_dated_tips_RTDT_sample_times.txt');
end;

function TExampleFiles.ResBookFilename(Basename: String): String;
begin
  Result := 'Private' + PathDelim + 'Examples' + PathDelim + BookFilesDirectory + PathDelim + Basename;
end;

function TExampleFiles.ResExampleFilename(Basename: String): String;
begin
  Result := 'Private' + PathDelim + 'Examples' + PathDelim + Basename;
end;

function TExampleFiles.SystemBookFilename(Basename: String): String;
begin
  Result := BookFilesFolderName + PathDelim + Basename;
end;

function TExampleFiles.SystemExampleFilename(Basename: String): String;
begin
  Result := ExampleFilesFolderName + PathDelim + Basename;
end;

end.
