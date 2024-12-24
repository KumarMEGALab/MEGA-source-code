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

unit MLoadTreeThread;

interface

uses
  Classes, SysUtils, MTreeList;

type

  TLoadTreeThread = class(TThread)
    private

    protected
      FIsSuccess: Boolean;
      function LoadTree: Boolean; virtual; abstract;
      function GetTreeList: TTreeList;
      procedure SetTreeList(const Value: TTreeList);
    public
      Messages: TStringList;
      FTreeList: TTreeList;
      Filename: String;
      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;

      property TreeList: TTreeList read GetTreeList write SetTreeList; // TreeList should always be owned by some other object so there is no wastful copying
      property IsSuccess: Boolean read FIsSuccess;
  end;

  TLoadNewickTreeThread = class(TLoadTreeThread)

    protected
      function LoadTree: Boolean; override;
    public

  end;



implementation

{ TLoadNewickTreeThread }

function TLoadNewickTreeThread.LoadTree: Boolean;
begin
  Result := False;
  Messages.Clear;
  if not FileExists(Filename) then
  begin
    Messages.Add('The specified newick file does not exist: ' + Filename);
    Exit;
  end;

  try
    if not TreeList.ImportFromNewickFile(Filename, nil, False) then
      Messages.Add(TreeList.ErrorMsg)
    else
      Result := True;
  except
    on E:Exception do
    begin
      Messages.Add('Error when loading the newick tree: ' + E.Message);
    end;
  end;
end;

{ TLoadTreeThread }

constructor TLoadTreeThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FTreeList := nil;
  Messages := TStringList.Create;
end;

destructor TLoadTreeThread.Destroy;
begin
  if Assigned(Messages) then
    Messages.Free;
  // do not free the TreeList, just let some other object take ownership so we don't have to copy it
  inherited;
end;

procedure TLoadTreeThread.Execute;
begin
  FIsSuccess := LoadTree;
end;

function TLoadTreeThread.GetTreeList: TTreeList;
begin
  Result := FTreeList;
end;

procedure TLoadTreeThread.SetTreeList(const Value: TTreeList);
begin
  FTreeList := Value;
end;

end.
