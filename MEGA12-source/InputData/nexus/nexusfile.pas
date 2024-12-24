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

unit nexusfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nexusblock, nexustokenizer, nexustokens, KeywordConsts;

type

  { TNexusFile }

  TNexusFile = class(TObject)
    private
      FEnforceUppercaseCommands: Boolean;
      FErrorColumn: Integer;
      FErrorLine: Integer;
      FErrorMsg: String;
      FSelectedCharsBlockIndex: Integer;
      FSelectedTreeBlockIndex: Integer;
      FSelectedTreeIndex: Integer;

      function GetItem(Index: Integer): TNexusBlock;
      function GetSelectedCharsBlock: TCharactersBlock;
      function GetSelectedTree: String;
      function GetSelectedTreeBlock: TTreeBlock;
      procedure SetFilename(AValue: String);
      procedure SetSelectedCharsBlockIndex(AValue: Integer);
      procedure SetSelectedTreeIndex(AValue: Integer);
    protected
      FTokenizer: TNexusTokenizer;
      FBlocks: TList; { all blocks are added to this list}
      FTreeBlocks: TList; { any tree blocks are added to this list as well for convenience}
      FCharacterBlocks: TList; { any character blocks are added to this list as well for convenience}
      FTaxaBlocks: TList;
      FUnblockedComments: TList; { the file may have comments that are not contained in any blocks}
      FFilename: String;
      FFactory: TNexusBlockFactory;
      procedure NewBlock(AList: TList);
      function GetBlockType(AList: TList): TNexusBlockType;

      function Validate: Boolean;
    public
      constructor Create(EnforceUppercaseCommands: Boolean = False);
      destructor Destroy; override;

      procedure Clear;
      function Parse: Boolean;
      function GetNewickString(BlockIndex: Integer = 0; TreeIndex: Integer = 0): String;
      function NumTaxaBlocks: Integer;
      function GetTaxaBlock(Index: Integer): TTaxaBlock;
      function NumTreeBlocks: Integer; { there can be multiple blocks for any block type}
      function NumTreesInBlock(TreeBlockIndex: Integer): Integer; { a tree block can have multiple trees, TreeBlockIndex is zero-based}
      function NumTrees: Integer;
      function GetTreeBlock(TreeBlockIndex: Integer): TTreeBlock; overload;
      function GetTreeBlock(ALink: String): TTreeBlock; overload;
      function GetTreeNames: TStringList;
      function HasTreeBlock(ALink: String): Boolean;

      function NumCharacterBlocks: Integer;
      function GetCharacterBlock(CharacterBlockIndex: Integer = 0): TCharactersBlock;
      function GetCharacterBlocksNames: TStringList;
      function NumSequences(CharacterBlockIndex: Integer = 0): Integer;
      function NumSites(CharactersBlockIndex: Integer = 0): Integer;
      function SeqDataType(CharactersBlockIndex: Integer = 0):TSnTokenCode;
      function DebugWriteTokens: TStringList;
      function DebugWriteData: TStringList;
      function ErrorString: String;
      property Filename: String read FFilename write SetFilename;
      property Items[Index: Integer]: TNexusBlock read GetItem; default;
      property ErrorLine: Integer read FErrorLine;
      property ErrorColumn: Integer read FErrorColumn;
      property ErrorMsg: String read FErrorMsg;
      property SelectedCharsBlockIndex: Integer read FSelectedCharsBlockIndex write SetSelectedCharsBlockIndex;
      property SelectedTreeIndex: Integer read FSelectedTreeIndex write SetSelectedTreeIndex;
      property SelectedCharsBlock: TCharactersBlock read GetSelectedCharsBlock;
      property SelectedTreeBlockIndex: Integer read FSelectedTreeBlockIndex;
      property SelectedTreeBlock: TTreeBlock read GetSelectedTreeBlock;
      property SelectedTree: String read GetSelectedTree;
      property EnforceUppercaseCommands: Boolean read FEnforceUppercaseCommands;
  end;

implementation

{ TNexusFile }

function TNexusFile.GetItem(Index: Integer): TNexusBlock;
begin
  Assert((FBlocks.Count > 0) and (Index >= 0) and (Index <FBlocks.Count));
  Result := TNexusBlock(FBlocks[Index]);
end;

function TNexusFile.GetSelectedCharsBlock: TCharactersBlock;
begin
  Result := GetCharacterBlock(FSelectedCharsBlockIndex);
end;

function TNexusFile.GetSelectedTree: String;
var
  i, j: Integer;
  Index: Integer;
begin
  Result := EmptyStr;
  if FSelectedTreeIndex >= 0 then
  begin
    if NumTreeBlocks > 1 then
    begin
      Index := -1;
      for i := 0 to NumTreeBlocks - 1 do
      begin
        if GetTreeBlock(i).NumTrees > 0 then
        begin
          for j := 0 to GetTreeBlock(i).NumTrees - 1 do
          begin
            inc(Index);
            if Index = FSelectedTreeIndex then
            begin
              Result := GetNewickString(i, j);
              Exit;
            end;
          end;
        end;
      end;
    end
    else
    begin
      Assert(FSelectedTreeIndex < GetTreeBlock(0).NumTrees);
      Result := GetNewickString(0, FSelectedTreeIndex);
    end;
  end;
end;

function TNexusFile.GetSelectedTreeBlock: TTreeBlock;
begin
  Result := GetTreeBlock(FSelectedTreeBlockIndex);
end;

procedure TNexusFile.SetFilename(AValue: String);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
end;

procedure TNexusFile.SetSelectedCharsBlockIndex(AValue: Integer);
begin
  if FSelectedCharsBlockIndex=AValue then Exit;
  FSelectedCharsBlockIndex:=AValue;
end;

procedure TNexusFile.SetSelectedTreeIndex(AValue: Integer);
var
  i, j: Integer;
begin
  j := 0;
  if FSelectedTreeIndex=AValue then Exit;
  FSelectedTreeIndex:=AValue;
  if NumTreeBlocks > 0 then
  begin
    for i := 0 to NumTreeBlocks - 1 do
    begin
      j := j + GetTreeBlock(i).NumTrees;
      if j > AValue then
      begin
        FSelectedTreeBlockIndex := i;
        break;
      end;
    end;
  end
  else
    FSelectedTreeBlockIndex:= 0;
end;

procedure TNexusFile.NewBlock(AList: TList);
var
  ABlock: TNexusBlock;
  AType: TNexusBlockType;
begin
  if not AList.Count > 3 then
    raise Exception.Create('Error parsing Nexus file. Thought we found a block but were wrong');
  AType := GetBlockType(AList);
  ABlock := FFactory.CreateBlock(AType);
  ABlock.AddTokens(AList);
  AList.Clear; { the block becomes the owner of the tokens}
  FBlocks.Add(ABlock);
  case AType of
    nbtTaxa:
      begin
        FTaxaBlocks.Add(ABlock);
        if ABlock.Title = EmptyStr then
          ABlock.Title := 'Taxa Block - ' + IntToStr(FTaxaBlocks.Count);
      end;
    nbtCharacters:
      begin
        FCharacterBlocks.Add(ABlock);
        if ABlock.Title = EmptyStr then
          ABlock.Title := 'Characters Block - ' + IntToStr(FCharacterBlocks.Count);
        if FSelectedCharsBlockIndex < 0 then
          FSelectedCharsBlockIndex := 0;
      end;
    nbtData:
      begin
        FCharacterBlocks.Add(ABlock);
        if ABlock.Title = EmptyStr then
          ABlock.Title := 'Data Block - ' + IntToStr(FCharacterBlocks.Count);
        if FSelectedCharsBlockIndex < 0 then
          FSelectedCharsBlockIndex := 0;
      end;
    nbtTree:
      begin
        FTreeBlocks.Add(ABlock);
        if ABlock.Title = EmptyStr then
          ABlock.Title := 'Trees Block - ' + IntToStr(FTreeBlocks.Count);
        if FSelectedTreeIndex < 0 then
          FSelectedTreeIndex := 0;
      end;
    nbtNotes: ;
    nbtUnaligned: ;
    nbtAssumptions: ;
    nbtCodons: ;
    nbtSets: ;
    nbtDistances: ;
    nbtPrivate: ;
  end;
end;

function TNexusFile.GetBlockType(AList: TList): TNexusBlockType;
var
  AToken: TNexusToken;
begin
  AToken := TNexusToken(AList[0]);
  if not (AToken.NexusTokenType = nttBegin) then
    raise Exception.Create('Error parsing Nexus file. Expected BEGIN command but got ' + #39 + AToken.Pattern + #39 + ' at line ' + IntToStr(AToken.Line) + ' column ' + IntToStr(AToken.Column));
  AToken := TNexusToken(AList[1]);
  Result := FFactory.StringToBlockType(Uppercase(AToken.Pattern));
end;

function TNexusFile.GetTreeBlock(TreeBlockIndex: Integer): TTreeBlock;
begin
  Result := nil;
  Assert((FTreeBlocks.Count > 0) and (TreeBlockIndex < FTreeBlocks.Count));
  Result := TTreeBlock(FTreeBlocks[TreeBlockIndex]);
end;

function TNexusFile.GetTreeBlock(ALink: String): TTreeBlock;
var
  i: Integer;
begin
  Assert((FTreeBlocks.Count > 0) and (Trim(ALink) <> EmptyStr));
  Result := nil;
  for i := 0 to FTreeBlocks.Count - 1 do
  begin
    if SameText(TTreeBlock(FTreeBlocks[i]).Link, ALink) then
    begin
      Result := TTreeBlock(FTreeBlocks[i]);
      break;
    end;
  end;
end;

function TNexusFile.GetTreeNames: TStringList;
var
  i, j: Integer;
  TreeNames: TStringList;
begin
  TreeNames := nil;

  Result := TStringList.Create;
  if NumTreeBlocks = 0 then
    Exit;
  try
    if NumTreeBlocks > 1 then
    begin
      for i := 0 to NumTreeBlocks - 1 do
      begin
        if GetTreeBlock(i).NumTrees > 0 then
        begin
          TreeNames := GetTreeBlock(i).GetTreeNames;
          if TreeNames.Count > 0 then
            for j := 0 to TreeNames.Count - 1 do
            begin
              Result.Add(GetTreeBlock(i).Title + ': ' + TreeNames[j]);
            end;
          FreeAndNil(TreeNames);
        end;
      end;
    end
    else
    begin
      TreeNames := GetTreeBlock(0).GetTreeNames;
      Result.AddStrings(TreeNames);
    end;
  finally
    if Assigned(TreeNames) then
      TreeNames.Free;
  end;
end;

function TNexusFile.HasTreeBlock(ALink: String): Boolean;
var
  i: Integer;
begin
  Assert((FTreeBlocks.Count > 0) and (Trim(ALink) <> EmptyStr));
  Result := False;
  for i := 0 to FTreeBlocks.Count - 1 do
  begin
    if SameText(TTreeBlock(FTreeBlocks[i]).Link, ALink) then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TNexusFile.NumCharacterBlocks: Integer;
begin
  Result := FCharacterBlocks.Count;
end;

function TNexusFile.GetCharacterBlock(CharacterBlockIndex: Integer): TCharactersBlock;
begin
  Assert((FCharacterBlocks.Count > 0) and (CharacterBlockIndex < FCharacterBlocks.Count));
  Result := TCharactersBlock(FCharacterBlocks[CharacterBlockIndex]);
end;

function TNexusFile.GetCharacterBlocksNames: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if NumCharacterBlocks > 0 then
    for i := 0 to NumCharacterBlocks - 1 do
      Result.Add(GetCharacterBlock(i).Title);
end;

function TNexusFile.NumSequences(CharacterBlockIndex: Integer = 0): Integer;
var
  aBlock: TCharactersBlock;
begin
  Result := 0;
  if NumCharacterBlocks > 0 then
  begin
    aBlock := GetCharacterBlock(CharacterBlockIndex);
    Result := aBlock.NumSequences;
  end;
end;

function TNexusFile.NumSites(CharactersBlockIndex: Integer): Integer;
var
  aBlock: TCharactersBlock;
begin
  Result := 0;
  if NumCharacterBlocks > 0 then
  begin
    aBlock := GetCharacterBlock(CharactersBlockIndex);
    Result := aBlock.NumSites;
  end;
end;

function TNexusFile.SeqDataType(CharactersBlockIndex: Integer): TSnTokenCode;
var
  aBlock: TCharactersBlock;
begin
  if NumCharacterBlocks > 0 then
  begin
    aBlock := GetCharacterBlock(CharactersBlockIndex);
    Result := aBlock.DataType;
  end
  else
    Result := snNoToken;
end;

function TNexusFile.Validate: Boolean;
var
  aList: TStringList;
  aBlock: TNexusBlock;
  i: Integer;
begin
  aList := nil;
  Result := True;

  try
    try
      aList := TStringList.Create;
      if FBlocks.Count > 0 then
      begin
        for i := (FBlocks.Count - 1) downto 0 do { go backwards so that ErrorLine and ErrorColumn reference the first error in file}
        begin
          aBlock := TNexusBlock(FBlocks[i]);
          if not aBlock.Validate then
          begin
            aList.Add(aBlock.ErrMsg);
            FErrorLine := aBlock.ErrLine;
            FErrorColumn := aBlock.ErrCol;
            Result := False;
          end;
        end;
      end;
      if not Result then
        FErrorMsg := aList.Text;
    except
      on E:Exception do
        raise Exception.Create('An error occurred while validating the Nexus file: ' + E.Message);
    end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

constructor TNexusFile.Create(EnforceUppercaseCommands: Boolean = False);
begin
  FTokenizer := nil;
  FBlocks := TList.Create;
  FTreeBlocks := TList.Create;
  FCharacterBlocks := TList.Create;
  FTaxaBlocks := TList.Create;
  FFactory := TNexusBlockFactory.Create;
  FUnblockedComments := TList.Create;
  FErrorLine := -1;
  FErrorColumn := -1;
  FErrorMsg := EmptyStr;
  FSelectedTreeIndex := -1;
  FSelectedCharsBlockIndex := -1;
  FEnforceUppercaseCommands := EnforceUppercaseCommands;
end;

destructor TNexusFile.Destroy;
begin
  if Assigned(FBlocks) then
  begin
    Clear;
    FBlocks.Free;
  end;
  if Assigned(FTreeBlocks) then
    FTreeBlocks.Free;
  if Assigned(FCharacterBlocks) then
    FCharacterBlocks.Free;
  if Assigned(FTokenizer) then
    FTokenizer.Free;
  if Assigned(FFactory) then
    FFactory.Free;
  if Assigned(FUnblockedComments) then
    FUnblockedComments.Free;
  if Assigned(FTaxaBlocks) then
    FTaxaBlocks.Free;
  inherited Destroy;
end;

procedure TNexusFile.Clear;
var
  i: Integer;
begin
  if not Assigned(FBlocks) then
    Exit;
  if FBlocks.Count > 0 then
    for i := 0 to FBlocks.Count - 1 do
      TNexusBlock(FBlocks[i]).Free;
  FBlocks.Clear;
end;

function TNexusFile.Parse: Boolean;
var
  AToken: TNexusToken;
  AList: TList; { hold tokens temporarily until we know just what we are working with}
  InsideBlock: Boolean;
  InsideComment: Integer;
  HasError: Boolean;
begin
  AToken := nil;
  AList := nil;
  Result := False;
  InsideBlock := False;
  InsideComment := 0;
  HasError := False;

  try
    AList := TList.Create;
    FTokenizer := TNexusTokenizer.Create(FFilename);
    FTokenizer.EnforceUppercaseCommands := FEnforceUppercaseCommands;
    AToken := TNexusToken.Create;
    FTokenizer.NextToken(AToken);
    if not (AToken.Pattern = '#NEXUS') then
    begin
      HasError := True;
      FErrorLine := AToken.Line;
      FErrorColumn := AToken.Column;
      FErrorMsg := 'Nexus files must begin with the #NEXUS command';
    end
    else
    begin
      while FTokenizer.HasNext and (not HasError) do
      begin
        AToken := TNexusToken.Create;
        FTokenizer.NextToken(AToken);
        if AToken.NexusTokenType = nttLBracket then
          inc(InsideComment)
        else if AToken.NexusTokenType = nttRBracket then
        begin
          dec(InsideComment);
        end;
        if (InsideComment <= 0) and (AToken.NexusTokenType = nttBegin) then
          InsideBlock := True;
        if ((InsideComment  > 0) or (AToken.NexusTokenType = nttRBracket)) and (not InsideBlock) then
          FUnblockedComments.Add(AToken)
        else
          AList.Add(AToken);
        if AToken.NexusTokenType = nttEnd then
        begin
          if FTokenizer.HasNext then
          begin
            AToken := TNexusToken.Create;
            FTokenizer.NextToken(AToken);
            if not (AToken.NexusTokenType = nttSemicolon) then
            begin
              HasError := True;
              FErrorLine := AToken.Line;
              FErrorColumn := AToken.Column;
              FErrorMsg := 'missing semicolon after block END command. Line ' + IntToStr(AToken.Line);
            end;
            AList.Add(AToken);
            NewBlock(AList); { creates a new NexusBlock of the appropriate type and adds it to the list}
            AList.Clear;
            InsideBlock := False;
          end
          else
          begin
            HasError := True;
            FErrorLine := AToken.Line;
            FErrorColumn := AToken.Column;
            FErrorMsg := 'missing semicolon after block END command. Line ' + IntToStr(AToken.Line);
          end;
        end;
      end;
      if not HasError then
        Result := Validate
      else
        Result := False;
    end;
  finally
    if Assigned(FTokenizer) then
      FreeAndNil(FTokenizer);
    if Assigned(AList) then
      AList.Free;
  end;
end;

function TNexusFile.GetNewickString(BlockIndex: Integer; TreeIndex: Integer): String;
var
  aBlock: TTreeBlock;
begin
  Result := '';
  aBlock := GetTreeBlock(BlockIndex);
  Assert(aBlock.NumTrees > TreeIndex);
  if not aBlock.Validate then
    raise Exception.Create('Error parsing Nexus file: ' + aBlock.ErrMsg);
  if not aBlock.GetNewickString(TreeIndex, Result) then
    raise Exception.Create('Unknown error when parsing Nexus file. Failed to retrieve newick string');
end;

function TNexusFile.NumTaxaBlocks: Integer;
begin
  Result := FTaxaBlocks.Count;
end;

function TNexusFile.GetTaxaBlock(Index: Integer): TTaxaBlock;
begin
  Result := nil;
  if (Index >= 0) and (Index < (FTaxaBlocks.Count)) then
    Result := TTaxaBlock(FTaxaBlocks[Index]);
end;

function TNexusFile.NumTreeBlocks: Integer;
begin
  Result := FTreeBlocks.Count;
end;

function TNexusFile.NumTreesInBlock(TreeBlockIndex: Integer): Integer;
begin
  Assert((FTreeBlocks.Count > 0) and (TreeBlockIndex < FTreeBlocks.Count));
  Result := TTreeBlock(FTreeBlocks[TreeBlockIndex]).NumTrees;
end;

function TNexusFile.NumTrees: Integer;
var
  i: Integer;
begin
  Result := 0;
  if NumTreeBlocks > 0 then
    for i := 0 to NumTreeBlocks - 1 do
      Result := Result + NumTreesInBlock(i);
end;

function TNexusFile.DebugWriteTokens: TStringList;
var
  i: Integer;
  AList: TStringlist;
begin
  Result := TStringList.Create;
  if FBlocks.Count > 0 then
  begin
    for i := 0 to FBlocks.Count - 1 do
    begin
      Alist := TNexusBlock(FBlocks[i]).DebugWriteTokens;
      Result.AddStrings(Alist);
      AList.Free;
    end;
  end;
end;

function TNexusFile.DebugWriteData: TStringList;
var
  i: Integer;
  AList: TStringlist;
begin
  Result := TStringList.Create;
  if FBlocks.Count > 0 then
  begin
    for i := 0 to FBlocks.Count - 1 do
    begin
      Alist := TNexusBlock(FBlocks[i]).DebugWriteData;
      Result.AddStrings(Alist);
      AList.Free;
    end;
  end;
end;

function TNexusFile.ErrorString: String;
begin
  Result := Format('Line %d, Column %d - %s', [FErrorLine, FErrorColumn, FErrorMsg]);
end;

end.

