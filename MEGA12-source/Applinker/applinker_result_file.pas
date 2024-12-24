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

unit applinker_result_file;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TAppLinkerResultsFile }

  TAppLinkerResultsFile = class(TObject)
    private
      FDisplayName: String;
      FDisplayOrder: Integer;
      FFilePath: String;
      FTargetNodeName: String;
      FToolTip: String;
      function GetFullDisplayName: String;
    public
      constructor Create(aPath: String; aDisplayName: String; aTooltip: String; aNodeName: String);
      property FilePath: String read FFilePath;
      property DisplayName: String read FDisplayName write FDisplayName;
      property ToolTip: String read FToolTip write FToolTip;
      property FullDisplayName: String read GetFullDisplayName;
      property TargetNodeName: String read FTargetNodeName write FTargetNodeName;
      property DisplayOrder: Integer read FDisplayOrder write FDisplayOrder;
  end;

implementation

{ TAppLinkerResultsFile }

function TAppLinkerResultsFile.GetFullDisplayName: String;
begin
  Result := Format('%s-%s', [DisplayName, FTargetNodeName]);
end;

constructor TAppLinkerResultsFile.Create(aPath: String; aDisplayName: String; aTooltip: String; aNodeName: String);
begin
  FFilePath := aPath;
  FDisplayName := aDisplayName;
  FToolTip := aToolTip;
  FTargetNodeName := aNodeName;
end;

end.

