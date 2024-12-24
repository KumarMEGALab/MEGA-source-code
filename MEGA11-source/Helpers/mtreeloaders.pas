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

unit mtreeloaders;

interface

uses
  Classes, SysUtils, MTreeList, ITimeTree, MAnalysisInfo;

type
  TGuiTreeLoader = class(TInterfacedObject, ITreeLoader)
    private
      FMsg: String;
    public
      constructor Create;
      destructor Destroy; override;
      function LoadTree(var AnalysisInfo: TAnalysisInfo; const FileName: String=''): Boolean;
      function GetMsg: String;
  end;

implementation

uses
  Dialogs, MegaUtils, MegaConsts, MOtuInfo, MTreePack, MNewickExportOptions;

{ TGuiTreeLoader }

constructor TGuiTreeLoader.Create;
begin
  FMsg := EmptyStr;
end;

destructor TGuiTreeLoader.Destroy;
begin
  inherited;
end;

function TGuiTreeLoader.GetMsg: String;
begin
  Result := FMsg;
end;

function TGuiTreeLoader.LoadTree(var AnalysisInfo: TAnalysisInfo; const FileName: String=''): Boolean;
var
  TreeFile: AnsiString;
  OpenDlg: TOpenDialog;
  TempNames: TStringList;
  AName: String;
  i: Integer;
  HasNodeLabels: Boolean;
  UsingNames: Boolean;
  Options: TNewickExportOptions;
begin
  OpenDlg := nil;
  Result := False;
  TreeFile := EmptyStr;
  TempNames := nil;

  if IsGlenDevMode then
    TreeFile := GlenTreeFile
  else
  begin
    try
      OpenDlg := TOpenDialog.Create(nil);
      OpenDlg.Title := 'Please select the newick tree file to load';
      OpenDlg.DefaultExt := 'NWK';
      OpenDlg.Filter := NewickFilesFilter;
      OpenDlg.FileName := '';
      OpenDlg.InitialDir := GetCurrentDir;
      if OpenDlg.Execute then
        TreeFile := OpenDlg.FileName;
    finally
      if Assigned(OpenDlg) then
        OpenDlg.Free;
    end;
  end;

  if TreeFile = EmptyStr then
  begin
    FMsg := 'No tree file specified' + LineEnding;
    Exit;
  end;

  try
    try
      if not FileExists(TreeFile) then
        raise Exception.Create('The specified tree file does not exist');
      UsingNames := (AnalysisInfo.MyOtuNames.Count > 0);
      if UsingNames then
      begin
        TempNames := TStringList.Create;
        for i  := 0 to AnalysisInfo.MyOtuNames.Count - 1 do
        begin
          AName := AnalysisInfo.MyOtuNames[i];
          TrimTaxaName2(AName);
          TempNames.Add(AName);
        end;
      end;

      if not AnalysisInfo.MyOriTreeList.ImportFromNewickFile(TreeFile, TempNames) then
      begin
        if UsingNames then
          raise Exception.Create('Failed to load the tree file. Please check that the file is formatted correctly and taxa names match those in the sequence data')
        else
          raise Exception.Create('Failed to load the tree file. Please check that the file is formatted correctly');
      end;
      Options.BranchLengths := AnalysisInfo.MyOriTreeList.isBLen;
      Options.BootstrapVals := AnalysisInfo.MyOriTreeList.isStats;
      Options.NodeLabels := True;
      AnalysisInfo.MyUserNewickTree := AnalysisInfo.MyOriTreeList.OutputNewickTree(0, Options, 0.0);

      if UsingNames then
      begin
        { handle the case where extra taxa in the tree need to be pruned}
        TempNames.Clear;
        for i := 0 to AnalysisInfo.MyUsedOtuInfos.Count - 1 do { build the name list with only the taxa being used}
        begin
          AName := TOtuInfo(AnalysisInfo.MyUsedOtuInfos[i]).Name;
          TrimTaxaName2(AName);
          TempNames.Add(AName);
        end;
        if AnalysisInfo.MyOriTreeList.HasUnwantedTaxa(TempNames) then { then we need to prune the tree}
        begin
          HasNodeLabels := AnalysisInfo.MyOriTreeList.HasInternalNodeLbls;
          if not AnalysisInfo.GetUserTreeInput(AnalysisInfo.InitialUsrOperation, TreeFile,
                                      AnalysisInfo.ARP,
                                      nil,
                                      ttML,
                                      True) then
            raise Exception.Create('failed to parse tree file');
          AnalysisInfo.MyOriTreeList.ImportFromNewick(AnalysisInfo.MyUserNewickTree, AnalysisInfo.MyOtuNames);
          if HasNodeLabels then
            FMsg := FMsg + 'Note - Unused taxa were pruned from the tree. During this process, all internal node labels in the tree were lost' + LineEnding;
        end;
      end;

      AnalysisInfo.MyUserTreeFile := TreeFile;
      Result := True;
    except
      on E: Exception do
      begin
        FMsg := FMsg + 'Oh no! An error has occurred: ' + E.Message + LineEnding;
      end;
    end;
  finally
    if Assigned(TempNames) then
      TempNames.Free;
  end;
end;

end.
