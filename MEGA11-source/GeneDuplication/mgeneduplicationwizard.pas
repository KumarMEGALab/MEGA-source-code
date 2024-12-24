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

unit MGeneDuplicationWizard;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  MAnalysisInfo, ExtCtrls, MDistPack, MSpeciesMapDlg, Classes,
  MSimpleTreeNode, MTreeData, ShowTrees, MTreeList, MTreeSearchThread,
  MProcessPack, Dialogs;

type

  TGeneDupThreadTerminator = class(TObject)
    public
      procedure OnGeneDupThreadDone(Thread: TObject);
  end;

  TGeneDuplicationsWizard = class(TObject)
    private
      FMapList: TStringList;
      FIsCancelled: Boolean;
      FSpeciesTreeFile: AnsiString;
      FSpeciesTreeList: TTreeList;
      FGeneTree: TSimpleTreeNodeArray;
      FSpeciesTree: TSimpleTreeNodeArray;
      FGeneTreeList: TTreeList;
      FGeneTreeIsRooted: Boolean;
      FSpeciesTreeIsRooted: Boolean;
      //function PrepareRootedTrees: TSimpleTreeNodeArrayArray; { generate an array of gene trees where each tree is rooted on a different branch; needed for finding rooting(s) that give the fewest number of gene duplications }
      function GetSpeciesNames: TStringList;
      procedure SetSpeciesNames(SpNames: TStringList);
      function GetSpeciesNamesListForGeneTree: TStringList;
      function GetUsingSpeciesTree: Boolean;
      function UpdateMapList: Boolean;
      procedure SetUsingSpeciesTree(const Value: Boolean);
      procedure SetGeneTreeRooted(const Value: Boolean);
      procedure UpdateRootIndex(const Value: Integer);
      procedure SetSpeciesTreeRooted(const Value: Boolean);
      function GetNumSpecies: Integer;
      function ValidateSpeciesTree(var Msg: String): Boolean;
      procedure SetTreeData(IsGeneTree: Boolean; NumOtus: Integer);
      procedure ResetNodes(IsGeneTree: Boolean; NumOtus: Integer);
      procedure UpdateSpNamesInGeneTree;
    public
      RootIndex: Integer; { gives the node that a tree is rooted on - we need this because by default ttreebox roots on the midpoint}
      LaunchedFrom: TStartGeneDupsSearchFrom;
      AnalysisInfo: TAnalysisInfo;
      UsrOperation: TDistTreeDlgOption;
      TreeExplorer: TObject;
      ProcessPack: TProcessPack;
      Owner: TComponent;
      constructor Create;
      destructor Destroy; override;
      procedure LaunchGeneDuplicationThread;
      procedure InitGeneTree;
      procedure InitSpeciesTree;
      procedure InitGeneTreeList;
      procedure InitSpeciesTreeList;
      procedure InitFromTreeExplorer(AInfo: TAnalysisInfo; TE: TObject); overload;
      function LoadGeneTree: Boolean;
      function LoadSpeciesTree: Boolean;
      function MapSpeciesNames: Boolean;
      function RootATree(IsGeneTree: Boolean): Boolean; { sets the root for either the gene tree or species tree}
      property SpeciesNames: TStringList read GetSpeciesNames;
      property UsingSpeciesTree: Boolean read GetUsingSpeciesTree write SetUsingSpeciesTree;
      property GeneTreeIsRooted: Boolean read FGeneTreeIsRooted write SetGeneTreeRooted;
      property SpeciesTreeIsRooted: Boolean read FSpeciesTreeIsRooted write SetSpeciesTreeRooted;
      property NumSpecies: Integer read GetNumSpecies;
  end;

  function GetInputDataFileName: String;
  function GetInputDataTitle: String;

implementation

uses
  {$IFDEF VISUAL_BUILD}Mega_Main,{$ELSE}MD_MegaMain,{$ENDIF} LCLIntf, LCLType,
  SysUtils, MegaUtils_NV, MRuntimeProgressDlg, MTreePack,
  Controls, MTreeViewForm, MFileUtils, Forms, MegaUtils;

{ TGeneDuplicationsWizard }

constructor TGeneDuplicationsWizard.Create;
begin
  FIsCancelled := False;
  FSpeciesTreeList := nil;
  FMapList := TStringList.Create;
  AnalysisInfo := nil;
  SpeciesMapDlg := nil;
  FGeneTreeIsRooted := False;
  FSpeciesTreeIsRooted := False;
  TreeExplorer := nil;
  RootIndex := -1;
end;

destructor TGeneDuplicationsWizard.Destroy;
var
  i: Integer;
begin
  TreeExplorer := nil;
  if Assigned(FMapList) then
    FMapList.Free;
  if Assigned(FGeneTree) and (Length(FGeneTree) > 0) then { then the user cancelled part of the way through so we need to clean it up}
  begin
    for i := 0 to Length(FGeneTree) - 1 do
      if Assigned(FGeneTree[i]) then
        FreeAndNil(FGeneTree[i]);
     SetLength(FGeneTree, 0);
  end;
  if Assigned(FGeneTreeList) then
    FGeneTreeList.Free;
  if Assigned(FSpeciesTree) and (Length(FSpeciesTree) > 0) then
  begin
    for i := 0 to Length(FSpeciesTree) - 1 do
      if Assigned(FSpeciesTree[i]) then
        FreeAndNil(FSpeciesTree[i]);
    SetLength(FSpeciesTree, 0);
  end;
  if Assigned(FSpeciesTreeList) then
    FreeAndNil(FSpeciesTreeList);
  if Assigned(AnalysisInfo) then
    FreeAndNil(AnalysisInfo);
  if Assigned(ProcessPack) then
    FreeAndNil(ProcessPack);
  inherited;
end;

function TGeneDuplicationsWizard.GetNumSpecies: Integer;
begin
  if Assigned(FSpeciesTreeList) and (FSpeciesTreeList.Count > 0) then
    Result := FSpeciesTreeList[0].NoOfOTUs
  else
    Result := 0;
end;

function TGeneDuplicationsWizard.GetSpeciesNames: TStringList;
var
  i: Integer;
  AStr: String;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  for i := 0 to FMapList.Count - 1 do
  begin
    AStr := FMapList.ValueFromIndex[i];
    TrimTaxaName2(AStr);
    Result.Add(AStr);
  end;
end;

function TGeneDuplicationsWizard.GetSpeciesNamesListForGeneTree: TStringList;
var
  i: Integer;
  TempStr: AnsiString;
begin
  Result := TStringList.Create;

  for i := 0 to FGeneTreeList.OTUNameList.Count - 1 do
  begin
    TempStr := FGeneTreeList.OTUNameList[i];
    if Trim(TempStr) = EmptyStr then
      raise Exception.Create('Missing species name for ' + #39 + FGeneTreeList.OtuNameList[i] + #39);
    Result.Add(FMapList.Values[TempStr]);
  end;
end;

function TGeneDuplicationsWizard.GetUsingSpeciesTree: Boolean;
begin
  Result := AnalysisInfo.MyGeneDupsInfo.UsedSpeciesTree;
end;

procedure TGeneDuplicationsWizard.InitGeneTree;
var
  Index: Integer;
  ANode: TSimpleTreeNode;
begin
  SetLength(FGeneTree, AnalysisInfo.MyOriTreeList.NoOfOtus * 2 - 1);
  Index := 0;
  while Index < Length(FGeneTree) do
  begin
    ANode := TSimpleTreeNode.Create;
    ANode.NodeIndex := Index;
    FGeneTree[Index] := ANode;
    inc(Index);
  end;
end;

procedure TGeneDuplicationsWizard.InitGeneTreeList;
begin
  FGeneTreeList := TTreeList.Create;
end;

procedure TGeneDuplicationsWizard.InitSpeciesTree;
var
  Index: Integer;
  ANode: TSimpleTreeNode;
begin
  SetLength(FSpeciesTree, NumSpecies * 2 - 1);
  Index := 0;
  while Index < Length(FSpeciesTree) do
  begin
    ANode := TSimpleTreeNode.Create;
    ANode.NodeIndex := Index;
    FSpeciesTree[Index] := ANode;
    inc(Index);
  end;
end;

procedure TGeneDuplicationsWizard.InitSpeciesTreeList;
begin
  FSpeciesTreeList := TTreeList.Create;
end;

procedure TGeneDuplicationsWizard.LaunchGeneDuplicationThread;
var
  GDThread: TGeneDuplicationThread;
  Terminator: TGeneDupThreadTerminator;
  Progress: TRuntimeProgress;
  ATreeViewer: TTreeViewForm;
begin
  try
    Progress := TRuntimeProgress.Create(Application.MainForm);
    Progress.DataFileName  :=  GetInputDataFileName;
    Progress.DataTitle     :=  GetInputDataTitle;
    AnalysisInfo.ARP := Progress;
    AnalysisInfo.SetIsGeneDups(True);
    Progress.FMAI := AnalysisInfo;
    GDThread := TGeneDuplicationThread.Create(LaunchedFrom, nil);

    if LaunchedFrom = gdsTreeExplorer then
    begin
      ATreeViewer := TTreeViewForm(TreeExplorer);
      if FGeneTreeIsRooted then
        GDThread.DrawGeneDupsEvent := ATreeViewer.SetGeneDuplications { instead of creating a new instance of TTreeViewForm, just update the existing one}
      else
      begin
        ATreeViewer.FormIsClosing := True;
        FreeAndNil(ATreeViewer); { in this case we will call ShowTrees.ShowGeneDupTree since we may have multiple trees}
      end;
    end
    else
      AnalysisInfo.MyUsrOperation := dtdoGeneDupInference;

    GDThread.ProgressDlg := Progress;
    GDThread.ShowProgress := True;
    GDThread.GTNoOfOtus := FGeneTreeList[0].NoOfOTUs;
    GDThread.GeneTreeIsRooted := FGeneTreeIsRooted;

    if UsingSpeciesTree then
    begin
      GDThread.UsingSpeciesTree := True;
      GDThread.Initialize(FGeneTree, FSpeciesTree, FGeneTreeList, FSpeciesTreeList);
    end
    else
    begin
      GDThread.UsingSpeciesTree := False;
      GDThread.Initialize(FGeneTree, nil, FGeneTreeList, nil);
    end;
    GDThread.FreeOnTerminate := True;
    Terminator := TGeneDupThreadTerminator.Create;
    GDThread.OnTerminate := Terminator.OnGeneDupThreadDone;
    Progress.Thread := GDThread;
    AnalysisInfo := nil; { relinquish control }
    FGeneTreeList := nil;
    FGeneTree := nil;
    FSpeciesTreeList := nil;
    FSpeciesTree := nil;
    Progress.Show;
    GDThread.Start; { launch it!}
  except
    on E: Exception do
    begin
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  end;
end;

function TGeneDuplicationsWizard.LoadGeneTree: Boolean;
var
  GeneTreeFile: AnsiString;
  OpenDlg: TOpenDialog;
begin
  OpenDlg := nil;
  Result := False;
  GeneTreeFile := EmptyStr;

  try
    OpenDlg := TOpenDialog.Create(nil);
    OpenDlg.Title := 'Please select the gene tree file in which to search for gene duplications';
    OpenDlg.DefaultExt := 'NWK';
    OpenDlg.Filter := NewickFilesFilter;
    OpenDlg.FileName := '';
    OpenDlg.InitialDir := GetCurrentDir;
    if OpenDlg.Execute then
      GeneTreeFile := OpenDlg.FileName;
  finally
    if Assigned(OpenDlg) then
      OpenDlg.Free;
  end;
  if GeneTreeFile = EmptyStr then
    Exit;

  try
    if not FileExists(GeneTreeFile) then
      raise Exception.Create('The specified gene tree file does not exist');

    AnalysisInfo := TAnalysisInfo.Create;
    AnalysisInfo.DataFilename := MegaForm.DataFilename;
    AnalysisInfo.MyGeneDupsInfo := TGeneDupsInfo.Create;
    AnalysisInfo.MyProcessPack := ProcessPack;
    AnalysisInfo.MyTreePack := TTreePack.Create;
    AnalysisInfo.InitialUsrOperation := dtdoGeneDupInference;
    AnalysisInfo.MyOriTreeList := TTreeList.Create;
    if not AnalysisInfo.MyOriTreeList.ImportFromNewickFile(GeneTreeFile, nil) then
      raise Exception.Create('failed to parse gene tree');
    AnalysisInfo.MyOtuNames := TStringList.Create;
    AnalysisInfo.MyOtuNames.AddStrings(AnalysisInfo.MyOriTreeList.OTUNameList);
    LaunchedFrom := gdsMegaMain;
    UsrOperation := dtdoGeneDupInference;
    InitGeneTree;
    InitGeneTreeList;
    FGeneTreeList.Assign(AnalysisInfo.MyOriTreeList);
    if FGeneTreeList.IsRooted then
    begin
      SetTreeData(True, FGeneTreeList.NoOfOTUs); { can set it now since we know the root will not change}
      SetGeneTreeRooted(True); { to ensure that the user is not prompted to root the tree}
    end;
    SwitchDirectory(GeneTreeFile); { sets current directory to the same one that the tree file is located in}
    Result := True;
  except
    on E: Exception do
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
      {$ELSE}
      Error_NV(E.Message);
      {$ENDIF}
    end;
  end;
end;

function TGeneDuplicationsWizard.LoadSpeciesTree: Boolean;
var
  OpenDlg: TOpenDialog;
  SpNames: TStringList;
  Msg: String = '';
begin
  Result := False;
  FSpeciesTreeFile := EmptyStr;
  SetUsingSpeciesTree(False); { assume that the user may back out}
  try
    OpenDlg := TOpenDialog.Create(Owner);
    OpenDlg.Title := 'Please select the species tree newick file to use';
    OpenDlg.DefaultExt := 'NWK';
    OpenDlg.Filter := NewickFilesFilter;
    OpenDlg.FileName := '';
    OpenDlg.InitialDir := GetCurrentDir;
    if OpenDlg.Execute then
      FSpeciesTreeFile := OpenDlg.FileName;
  finally
    if Assigned(OpenDlg) then
      OpenDlg.Free;
  end;

  if FSpeciesTreeFile = EmptyStr then
    Exit;

  try
    try
      SetUsingSpeciesTree(True); { handles it for TAnalysisInfo and TTreeViewForm if needed}
      InitSpeciesTreeList;
      SpNames := GetSpeciesNames;
      if not FSpeciesTreeList.ImportFromNewickFile(FSpeciesTreeFile, SpNames) then
        raise Exception.Create('Failed to parse the species tree. Either it is not formatted correctly or the provided species names do not match those in the tree file');
      if ValidateSpeciesTree(Msg) then { makes sure that all species in the gene tree are also represented in the species tree}
      begin
        InitSpeciesTree; { requires that FSpeciesTreeList is setup first}
        FSpeciesTreeList.AssignSpeciesNames(FSpeciesTreeList.OTUNameList);
        if FSpeciesTreeList.isRooted then
        begin
          SetTreeData(False, FSpeciesTreeList.NoOfOTUs); { can set it up now since we know it won't change}
          SetSpeciesTreeRooted(True); { so the user does not get prompted to root it}
        end;
        SwitchDirectory(FSpeciesTreeFile); { sets current directory to the directory that the tree file is in}
        Result := True;
      end
      else
        ShowMessage('Failed to validate the provided species tree: ' + Msg);
    except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(SpNames) then
      SpNames.Free;
  end;
end;

function TGeneDuplicationsWizard.MapSpeciesNames: Boolean;
var
  SpeciesMapDlg: TSpeciesMapDlg;
  ModalResult: Integer;
  i: Integer;
  Key, Value: String;
begin
  Result := False;
  try
    try
      SpeciesMapDlg := TSpeciesMapDlg.Create(MegaForm);
      SpeciesMapDlg.LoadTaxaNames(AnalysisInfo.MyOriTreeList.OTUNameList);
      ModalResult := SpeciesMapDlg.ShowModal;
      if ModalResult = mrOk then
      begin
        if not Assigned(FMapList) then
          FMapList := TStringList.Create;
        FMapList.Clear;
        for i := 0 to SpeciesMapDlg.MapList.Count - 1 do
        begin
          Key := SpeciesMapDlg.MapList.Names[i];
          Value := SpeciesMapDlg.MapList.ValueFromIndex[i];
          TrimTaxaName2(Value);
          FMapList.Add(Key + '=' + Value);
        end;
//        FMapList.AddStrings(SpeciesMapDlg.MapList);
        if Assigned(TreeExplorer) then { in this case the wizard was launched from TreeViewForm and the species names are needed in the TTreeList which exists there}
          TTreeViewForm(TreeExplorer).SetSpeciesNames(FMapList);
        for i := 0 to FMapList.Count - 1 do
        begin
          AnalysisInfo.MyOriTreeList.SetSpeciesNameForOtu(FMapList.ValueFromIndex[i], FMapList.Names[i]);
          FGeneTreeList.SetSpeciesNameForOtu(FMapList.ValueFromIndex[i], FMapList.Names[i]);
        end;
        if GeneTreeIsRooted then  { the user won't be changing the root of the tree so it is safe to do it here}
          UpdateSpNamesInGeneTree;
        Result := True;
      end;
    except
      on E: Exception do
        ShowMessage('Oh no! An error has occurred: ' + E.Message);
    end;
  finally
    if Assigned(SpeciesMapDlg) then
      SpeciesMapDlg.Release;
  end;
end;

//function TGeneDuplicationsWizard.PrepareRootedTrees: TSimpleTreeNodeArrayArray;
//var
//  ATreeViewer: TTreeViewForm;
//  TempInfo: TAnalysisInfo;
//begin
//
//{$IFDEF VISUAL_BUILD}
//  ATreeViewer := nil;
//
//  try
//    try
//      ATreeViewer := TTreeViewForm.Create(MegaForm);
//      ATreeViewer.JustRootingGeneTree := True;
//      ATreeViewer.Hide;
//      TempInfo := TAnalysisInfo.Create;
//      TempInfo.Assign(AnalysisInfo);  { bad stuff happens if we don't do it this way}
//      ATreeViewer.SetData(TempInfo);
//      ATreeViewer.Tree.LinearizeFunc := nil;
////      Result := ATreeViewer.GetRootedTrees;
//      ATreeViewer.JustRootingGeneTree := False; { so the temp info will get freed}
//    except
//      On E: Exception do
//        ShowMessage(E.Message);
//    end;
//  finally
//    if Assigned(ATreeViewer) then
//      ATreeViewer.Free;
//  end;
//  {$ENDIF}
//end;

procedure TGeneDuplicationsWizard.ResetNodes(IsGeneTree: Boolean; NumOtus: Integer);
var
  i: Integer;
  ATree: TSimpleTreeNodeArray;
  NumNodes: Integer;
begin
  if IsGeneTree then
    ATree := FGeneTree
  else
    ATree := FSpeciesTree;
  NumNodes := Length(ATree);
  Assert((NumNodes > 0), 'TSimpleTreeNode array not initialized');

  for i := 0 to NumOtus - 1 do
  begin
    ATree[i].NodeIndex := i;
    ATree[i].Ancestor := nil;
    ATree[i].Des1 := nil;
    ATree[i].Des2 := nil;
    ATree[i].IsDuplicationEvent := False;
    ATree[i].IsSpeciationEvent := False;
    ATree[i].IsOtu := True;
    ATree[i].IsRoot := False;
    ATree[i].Depth := -1;
    ATree[i].TempIndex := -1;
    ATree[i].ExtantSpecies.Clear;
    ATree[i].AncSpeciesIndex := -1;
    ATree[i].SequenceName := EmptyStr;
    ATree[i].Id := EmptyStr;
    ATree[i].Accession := EmptyStr;
    ATree[i].ScientificName := EmptyStr;
    ATree[i].CommonName := EmptyStr;
    ATree[i].Code := EmptyStr;
    ATree[i].Annotation := EmptyStr;
    ATree[i].Symbol := EmptyStr;
  end;
  for i := NumOtus to NumNodes - 1 do
  begin
    ATree[i].NodeIndex := i;
    ATree[i].Ancestor := nil;
    ATree[i].Des1 := nil;
    ATree[i].Des2 := nil;
    ATree[i].IsDuplicationEvent := False;
    ATree[i].IsSpeciationEvent := False;
    ATree[i].IsOtu := False;
    ATree[i].IsRoot := False;
    ATree[i].Depth := -1;
    ATree[i].TempIndex := -1;
    ATree[i].ExtantSpecies.Clear;
    ATree[i].AncSpeciesIndex := -1;
    ATree[i].SequenceName := EmptyStr;
    ATree[i].Id := EmptyStr;
    ATree[i].Accession := EmptyStr;
    ATree[i].ScientificName := EmptyStr;
    ATree[i].CommonName := EmptyStr;
    ATree[i].Code := EmptyStr;
    ATree[i].Annotation := EmptyStr;
    ATree[i].Symbol := EmptyStr;
  end;
end;

function TGeneDuplicationsWizard.RootATree(IsGeneTree: Boolean): Boolean;
var
  ModalResult: Integer;
  ATreeExplorer: TTreeViewForm;
  TreeToRoot: TSimpleTreeNodeArray;
  TreeListToRoot: TTreeList;
  TreeListCopy: TTreeList;
  MapList: TStringList;
begin
  Result := False;
  ATreeExplorer := nil;
  TreeListCopy := nil;

  try
    try
      if Assigned(AnalysisInfo.MyOriTreeList) then { clean up an existing tree list or create it if it does not exist}
        AnalysisInfo.MyOriTreeList.DeleteAll
      else
        AnalysisInfo.MyOriTreeList := TTreeList.Create;

      if IsGeneTree then { could be either the gene tree or the species tree}
      begin
        TreeToRoot := FGeneTree;
        TreeListToRoot := FGeneTreeList;
      end
      else
      begin
        TreeToRoot := FSpeciesTree;
        TreeListToRoot := FSpeciesTreeList;
      end;

      if Assigned(TreeExplorer) then { if so, the analysis was launched from TTreeViewForm}
        TTreeViewForm(TreeExplorer).Hide; { we don't want the user getting confused by having two TreeViewForms open }
      ATreeExplorer := TTreeViewForm.Create(Owner); { cannot use the existing TreeViewForm because it will be closed after ShowModal }
      ATreeExplorer.JustRootingGeneTree := True; { necessary so that the launchgenedupsaction does not get disabled in SetData}
      ATreeExplorer.CaptionHideActionExecute(nil);
      AnalysisInfo.MyOriTreeList.Assign(TreeListToRoot); { gets a copy of the TreeList - TreeExplorer will take ownership of it}
      TreeListCopy := TTreeList.Create;
      TreeListCopy.Assign(TreeListToRoot); { so we can recover the data if the user cancels and then retries}
      ATreeExplorer.KeepHidden := True;
      ATreeExplorer.SetData(AnalysisInfo);  { it will crash without an instance of AnalysisInfo}
      ATreeExplorer.InitForm;
      ATreeExplorer.PrepareToRootTreeOnly(rtGeneDups); { disables anything we don't want the user doing}
      ATreeExplorer.IsGeneDupsMode := True;
      ATreeExplorer.GeneDupTree := TreeToRoot; { ATreeExplorer will populate it with the correctly rooted tree}
      TreeListToRoot.DeleteAll; { reset it since the root is going to change}
      ATreeExplorer.GeneDupTreeList := TreeListToRoot; { ATreeExplorer will populate this for us as well}
      MapList := TreeListToRoot.GetOtuToSpeciesMapList; { gives key/value pairs of the form 'otuname=spname'}
      ATreeExplorer.SetSpeciesNames(MapList); { safer so that the TreeList stays synched properly}
      ATreeExplorer.Tree.FocusOnNode(Length(TreeToRoot) - 1); { this will set the focus on the current root node - guarantees that RootedIndex will be valid}
      ATreeExplorer.RootedIndex := ATreeExplorer.Tree.FocusedIndex; { also need to set it here for safety}
      ModalResult := ATreeExplorer.ShowModal; { make the user deal with it}
      if ModalResult = mrOk then
      begin
        RootIndex := ATreeExplorer.RootedIndex;
        TreeToRoot[RootIndex - 1].IsRoot := True;
        if IsGeneTree then
        begin
          SetGeneTreeRooted(True);
          UpdateRootIndex(RootIndex);
        end
        else
          SetSpeciesTreeRooted(True);
        Result := True;
      end
      else
      begin
        Result := False; { the user changed his/her mind}
        TreeListToRoot.Assign(TreeListCopy); { recover the data}
      end;
    except
      on E:Exception do
        ShowMessage('Oh no! an error occurred: ' + E.Message);
    end;
  finally
    if Assigned(MapList) then
      MapList.Free;
    if Assigned(TreeListCopy) then
      TreeListCopy.Free;
    if Assigned(ATreeExplorer) then
    begin
      ATreeExplorer.FormIsClosing := True;
      ATreeExplorer.Free;
    end;
  end;
end;

procedure TGeneDuplicationsWizard.SetGeneTreeRooted(const Value: Boolean);
begin
  FGeneTreeIsRooted := Value; { make sure we don't test all possible roots}
  AnalysisInfo.GeneTreeRooted := Value;
  if Assigned(TreeExplorer) then
    TTreeViewForm(TreeExplorer).SetGeneTreeRooted(Value);
end;

procedure TGeneDuplicationsWizard.SetSpeciesNames(SpNames: TStringList);
begin
  FMapList.Assign(SpNames);
end;

procedure TGeneDuplicationsWizard.SetSpeciesTreeRooted(const Value: Boolean);
begin
  FSpeciesTreeIsRooted := Value; { make sure we don't test all possible roots}
  AnalysisInfo.SpeciesTreeRooted := Value;
  if Assigned(TreeExplorer) then
    TTreeViewForm(TreeExplorer).SetSpeciesTreeRooted(Value);
end;

procedure TGeneDuplicationsWizard.SetTreeData(IsGeneTree: Boolean; NumOtus: Integer);
var
  i: integer;
  ATree: TSimpleTreeNodeArray;
  ATreeData: TTreeData;
  NumNodes: Integer;
begin
  ResetNodes(IsGeneTree, NumOtus);
  if IsGeneTree then
  begin
    ATree := FGeneTree;
    ATreeData := FGeneTreeList[0];
  end
  else
  begin
    ATree := FSpeciesTree;
    ATreeData := FSpeciesTreeList[0];
  end;
  NumNodes := Length(ATree);
  Assert((NumNodes > 0), 'TSimpleTreeNode not initialized');
  Assert((ATreeData.NoOfOTUs > 0), 'TTreeData not intialized');

  for i := 0 to NumOtus - 2 do
  begin
    ATree[NumOtus + i].Des1 := ATree[ATreeData[i].Des1];
    ATree[NumOtus + i].des2 := ATree[ATreeData[i].des2];
    ATree[ATreeData[i].des1].Ancestor := ATree[NumOtus + i];
    ATree[ATreeData[i].des2].Ancestor := ATree[NumOtus + i];
  end;

  for i := 0 to NumNodes - 1 do
    if ATree[i].Ancestor = nil then
    begin
      ATree[i].IsRoot := True;
      break;
    end;

  if IsGeneTree then
  begin
    for i := 0 to NumOtus - 1 do
      FGeneTree[i].SequenceName := FGeneTreeList.OTUName[i];
  end
  else
  begin
    for i := 0 to NumOtus - 1 do
      FSpeciesTree[i].SpeciesName := LowerCase(FSpeciesTreeList.SpeciesName[i]);
  end;
end;

procedure TGeneDuplicationsWizard.SetUsingSpeciesTree(const Value: Boolean);
begin
  AnalysisInfo.UsedSpeciesTree := Value;
  if Assigned(TreeExplorer) then
    TTreeViewForm(TreeExplorer).SetUsingSpeciesTree(Value);
end;

function TGeneDuplicationsWizard.UpdateMapList: Boolean;
var
  i: Integer;
  OtuName: AnsiString;
  SpName: AnsiString;
begin
  Result := False;
  try
    if not Assigned(FMapList) then
      FMapList := TStringList.Create;
    FMapList.Clear;
    if AnalysisInfo.MyOriTreeList.SpeciesNamesList.Count > 0 then
    begin
      for i := 0 to AnalysisInfo.MyOriTreeList.SpeciesNamesList.Count - 1 do
      begin
        OtuName := AnalysisInfo.MyOriTreeList.OTUName[i];
        SpName := AnalysisInfo.MyOriTreeList.SpeciesName[i];
        FMapList.Add(OtuName + '=' + SpName);
      end;
      Result := True;
    end;
  except
    on E:Exception do
      ShowMessage('Oh no! An error has occurred: ' + E.Message);
  end;
end;

procedure TGeneDuplicationsWizard.UpdateRootIndex(const Value: Integer);
begin
  AnalysisInfo.GeneDupsRootIndex := Value;
  if Assigned(TreeExplorer) then
    TTreeViewForm(TreeExplorer).RootedIndex := Value;
end;

procedure TGeneDuplicationsWizard.UpdateSpNamesInGeneTree;
var
  i, j: Integer;
begin
  Assert(Assigned(FMapList) and (FMapList.Count > 0));
  for i := 0 to AnalysisInfo.MyOriTreeList.NoOfOTUs - 1 do { using the two loops just to be safe}
    for j := 0 to FMapList.Count - 1 do
    begin
      if SameText(FGeneTree[i].SequenceName, FMapList.Names[i]) then
      begin
        FGeneTree[i].SpeciesName := FMapList.ValueFromIndex[i];
        break
      end;
    end;
end;

function TGeneDuplicationsWizard.ValidateSpeciesTree(var Msg: String): Boolean;
var
  SpInGeneTree: TStringList;
  SpInSpeciesTree: TStringList;
  i: Integer;
  Index: Integer;
  AStr: String;
begin
  SpInGeneTree := nil;
  SpInSpeciesTree := nil;
  Result := False;
  try
    try
      {get the names of the species in the gene tree}
      SpInGeneTree := TStringList.Create;
      SpInGeneTree.Sorted := True;
      SpInGeneTree.Duplicates := dupIgnore;
      if FGeneTreeList.HasSpeciesNames then
        for i := 0 to FGeneTreeList.NoOfOTUs - 1 do
        begin
          AStr := FGeneTreeList.SpeciesName[i];
          TrimTaxaName2(AStr);
          SpInGeneTree.Add(AStr);
        end;

      {get the names of the species in the species tree}
      SpInSpeciesTree := TStringList.Create;
      SpInSpeciesTree.Sorted := True;
      SpInSpeciesTree.Duplicates := dupIgnore;
      for i := 0 to FSpeciesTreeList.NoOfOtus - 1 do
        SpInSpeciesTree.Add(FSpeciesTreeList.OTUName[i]);

      { make sure that every species in the gene tree is also in the species tree}
      if SpInGeneTree.Count > 0 then
      begin
        for i := 0 to SpInGeneTree.Count - 1 do
        begin
          Index := SpInSpeciesTree.IndexOf(SpInGeneTree[i]);
          if Index < 0 then
            raise Exception.Create('A species from the gene tree is not found in the species tree: ' + SpInGeneTree[i]);
        end;
        Result := True;
      end;
    except
      on E: Exception do
        Msg := E.Message;
    end;
  finally
    if Assigned(SpInGeneTree) then
      SpInGeneTree.Free;
    if Assigned(SpInSpeciesTree) then
      SpInSpeciesTree.Free;
  end;
end;

procedure TGeneDuplicationsWizard.InitFromTreeExplorer(AInfo: TAnalysisInfo; TE: TObject);
begin
  TreeExplorer := TE;
  if Assigned(AnalysisInfo) then
    FreeAndNil(AnalysisInfo);
  AnalysisInfo := AInfo;
  if not Assigned(AnalysisInfo.MyGeneDupsInfo) then
    AnalysisInfo.MyGeneDupsInfo := TGeneDupsInfo.Create;
  AnalysisInfo.SetIsGeneDups(True);
  InitGeneTree;
  InitGeneTreeList;
  FGeneTreeList.Assign(AnalysisInfo.MyOriTreeList);
  if FGeneTreeList.isRooted then
  begin
    SetGeneTreeRooted(True);
    SetTreeData(True, AnalysisInfo.MyNoOfSeqs);
  end;
  if AnalysisInfo.MyOriTreeList.HasSpeciesNames then
    if not UpdateMapList then
    begin
      if not MapSpeciesNames then
        ShowMessage('Species names have not been mapped correctly. Gene duplication inferrence cannot be completed until this is resolved');
    end
    else if FGeneTreeList.IsRooted then
      UpdateSpNamesInGeneTree;
end;

{ TGeneDupThreadTerminator }

procedure TGeneDupThreadTerminator.OnGeneDupThreadDone(Thread: TObject);
var
  GeneDupThread: TGeneDuplicationThread;
  AnalysisInfo: TAnalysisInfo;
begin
  GeneDupThread := TGeneDuplicationThread(Thread);
  if not GeneDupThread.IsSuccess then
  begin
    AnalysisInfo := GeneDupThread.ProgressDlg.FMAI;
    AnalysisInfo.ARP.Hide;
    ShowMessage('Oh no! An error has occurred during the gene duplication search: ' + GeneDupThread.ErrorMsg);

    if GeneDupThread.GeneTreeIsRooted then
    begin
      FreeAndNil(AnalysisInfo.ARP);
      FreeAndNil(AnalysisInfo);
    end;
    Exit;
  end;

  try
    if not GeneDupThread.Canceled then
    begin
      {$IFDEF VISUAL_BUILD}
      GeneDupThread.ProgressDlg.UpdateRunStatusInfo('Status', 'Preparing display...');
      GeneDupThread.ProgressDlg.Refresh;
      {$ELSE}
      GeneDupThread.ProgressDlg.UpdateRunStatusInfo('Status', 'Preparing Newick file data...');
      {$ENDIF}
      AnalysisInfo := GeneDupThread.ProgressDlg.FMAI;
      if GeneDupThread.StartedFrom = gdsTreeExplorer then
      begin
        if GeneDupThread.GeneTreeIsRooted then { then the treelist is still owned by the thread, otherwise, TTreeViewForm took control of it}
        begin
          GeneDupThread.DoDrawGeneDupsEvent;
          FreeAndNil(AnalysisInfo.ARP);
          FreeAndNil(AnalysisInfo);
        end
        else
        begin
          if Assigned(AnalysisInfo.MyOriTreeList) then
            AnalysisInfo.MyOriTreeList.DeleteAll
          else
            AnalysisInfo.MyOriTreeList := TTreeList.Create;
          AnalysisInfo.MyOriTreeList.Assign(GeneDupThread.GeneTreeList);
          ShowGeneDupsTree(AnalysisInfo);
        end;
      end
      else
      begin
        if not Assigned(AnalysisInfo.MyOriTreeList) then { if so, TTreeViewForm took ownership}
          AnalysisInfo.MyOriTreeList := TTreeList.Create
        else
          AnalysisInfo.MyOriTreeList.DeleteAll;
        AnalysisInfo.MyOriTreeList.Assign(GeneDupThread.GeneTreeList);
        ShowGeneDupsTree(AnalysisInfo);
      end;
    end;
  finally
    RunningThreadCount := RunningThreadCount - 1;
    Self.Free;
  end;
end;

function GetInputDataFileName: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result  :=  MegaForm.DataFileName;
  {$ELSE}
  Result  :=  D_MegaMain.DataFileName;
  {$ENDIF}
end;

function GetInputDataTitle: String;
begin
  {$IFDEF VISUAL_BUILD}
  Result :=  MegaForm.DataTitle;
  {$ELSE}
  Result :=  D_MegaMain.DataTitle;
  {$ENDIF}
end;

end.
