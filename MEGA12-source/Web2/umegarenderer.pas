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

Unit umegarenderer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
    {$IFDEF DARWIN}uCEFWorkScheduler,{$ENDIF}
    Classes, SysUtils, mbrowserutils, Dialogs, uCEFTypes;

procedure CreateGlobalCEFApp(aApplicationDirectory: ustring = '');
procedure InitProcessMessagesHandler;

implementation

uses
  {$IFNDEF HELPER}MegaUtils, MegaPrivateFiles,{$ENDIF}
  StringUtils,uCEFInterfaces, uCEFApplication, uCEFConstants,uCEFProcessMessage,
  mceflogger2, uCEFDomVisitor;

var
  BrowserMessage : ustring;

function GetExceptionDump(E:Exception): String;
var
  I: Integer;
  Frames: PPointer;
begin
  Result := #9 + 'Stacktrace:' + LineEnding;
  if E <> nil then
  begin
    Result := Result + #9#9 + 'Exception class: ' + E.ClassName + LineEnding;
    Result := Result + #9#9 + 'Message: ' + E.Message + LineEnding;
  end;
  Result := Result + #9#9 + 'Address: ' + BackTraceStrFunc(ExceptAddr) + LineEnding;

  Frames := ExceptFrames;
  if ExceptFrameCount > 0 then
  begin
    Result := Result + #9#9 + 'Frames:' + LineEnding;
    for I := 0 to ExceptFrameCount - 1 do
      Result := Result + LineEnding + #9#9#9 + BackTraceStrFunc(Frames[I]) + LineEnding;
  end
  else
    Result := Result + #9#9 + 'Frames: none';
  Result := TrimRight(Result);
end;

procedure checkFormValidated(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_CHECK_FORM_VALIDATED_COMPLETE);
      node := document.GetElementById(IS_VALIDATED);
      if Assigned(node) then
        message.ArgumentList.SetString(IS_VALIDATED_INDEX, Node.GetElementAttribute('value'))
      else
        message.ArgumentList.SetString(IS_VALIDATED_INDEX, 'false');
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchNewickExportOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_NEWICK_EXPORT_OPTIONS_COMPLETE);
      node := document.GetElementById(NEWICK_BRANCH_LENGTHS);
      message.ArgumentList.SetString(NEWICK_BRANCH_LENGTHS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(NEWICK_BOOTSTRAP_VALUES);
      message.ArgumentList.SetString(NEWICK_BOOTSTRAP_VALUES_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(NEWICK_GENE_DUPLICATIONS);
      message.ArgumentList.SetString(NEWICK_GENE_DUPLICATIONS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(NEWICK_SPECIATIONS);
      message.ArgumentList.SetString(NEWICK_SPECIATIONS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(NEWICK_DIVERGENCE_TIMES);
      message.ArgumentList.SetString(NEWICK_DIVERGENCE_TIMES_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(NEWICK_RELATIVE_TIMES);
      message.ArgumentList.SetString(NEWICK_RELATIVE_TIMES_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(NEWICK_NODE_LABELS);
      message.ArgumentList.SetString(NEWICK_NODE_LABELS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchInputDataOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_INPUT_DATA_OPTIONS_COMPLETE);
      node := document.GetElementById(DNA_MISSING_DATA);
      message.ArgumentList.SetString(DNA_MISSING_DATA_INDEX, node.GetValue);
      node := document.GetElementById(DNA_ALIGNMENT_GAP);
      message.ArgumentList.SetString(DNA_ALIGNMENT_GAP_INDEX, node.GetValue);
      node := document.GetElementById(DNA_IDENTICAL_SYMBOL);
      message.ArgumentList.SetString(DNA_IDENTICAL_SYMBOL_INDEX, node.GetValue);
      node := document.GetElementById(PROTEIN_MISSING_DATA);
      message.ArgumentList.SetString(PROTEIN_MISSING_DATA_INDEX, node.GetValue);
      node := document.GetElementById(PROTEIN_ALIGNMENT_GAP);
      message.ArgumentList.SetString(PROTEIN_ALIGNMENT_GAP_INDEX, node.GetValue);
      node := document.GetElementById(PROTEIN_IDENTICAL_SYMBOL);
      message.ArgumentList.SetString(PROTEIN_IDENTICAL_SYMBOL_INDEX, node.GetValue);
      node := document.GetElementById(PAIRWISE_MISSING_DATA);
      message.ArgumentList.SetString(PAIRWISE_MISSING_DATA_INDEX, node.GetValue);
      node := document.GetElementById(LOWER_LEFT_MATRIX);
      message.ArgumentList.SetString(LOWER_LEFT_MATRIX_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(UPPER_RIGHT_MATRIX);
      message.ArgumentList.SetString(UPPER_RIGHT_MATRIX_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(NUCLEOTIDE_SEQUENCE);
      message.ArgumentList.SetString(NUCLEOTIDE_SEQUENCE_INDEX, node.GetElementAttribute(ISSELECTED));
      node := document.GetElementById(PROTEIN_SEQUENCE);
      message.ArgumentList.SetString(PROTEIN_SEQUENCE_INDEX, node.GetElementAttribute(ISSELECTED));
      node := document.GetElementById(PAIRWISE_DISTANCE);
      message.ArgumentList.SetString(PAIRWISE_DISTANCE_INDEX, node.GetElementAttribute(ISSELECTED));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchTreeLabelOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  markerNode: ICefDomNode;
  name, shape, color: ustring;
  markerInfo: ustring;
  debug: String;
begin
  markerInfo := '';
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_TREE_OPTIONS_LABELS_COMPLETE);
      node := document.GetElementById(TREE_OPTIONS_LABELS_DISPLAY_TAXON_NAMES);
      message.ArgumentList.SetString(TREE_OPTIONS_LABELS_DISPLAY_TAXON_NAMES_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_LABELS_DISPLAY_TAXON_MARKERS);
      message.ArgumentList.SetString(TREE_OPTIONS_LABELS_DISPLAY_TAXON_MARKERS_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TAXA_NAMES_LIST);
      markerNode := node.GetFirstChild;
      shape := markerNode.GetElementAttribute(MARKER_SHAPE);
      color := markerNode.GetElementAttribute(MARKER_COLOR);
      name := markerNode.GetElementAttribute('value');
      markerInfo := markerInfo + ustring(Format('name=%s,shape=%s,color=%s', [name, shape, color]) + LineEnding);
      markerNode := markerNode.GetNextSibling;
      while markerNode <> nil do
      begin
        shape := markerNode.GetElementAttribute(MARKER_SHAPE);
        color := markerNode.GetElementAttribute(MARKER_COLOR);
        name := markerNode.GetElementAttribute('value');
        markerInfo := markerInfo + ustring(Format('name=%s, shape=%s, color=%s', [name, shape, color]) + LineEnding);
        markerNode := markerNode.GetNextSibling;
      end;
      message.ArgumentList.SetString(MARKER_INFO_INDEX, markerInfo);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_LABELS_FONT);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_LABELS_FONT_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(TREE_OPTIONS_BRANCH_LABELS_COLOR);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_LABELS_COLOR_INDEX, Node.GetElementAttribute('value'));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchTreeScaleOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_TREE_OPTIONS_SCALE_COMPLETE);
      node := document.GetElementById(TREE_OPTIONS_SCALE_LINES);
      message.ArgumentList.SetString(TREE_OPTIONS_SCALE_LINES_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_SCALE_DISTANCE_SCALE);
      message.ArgumentList.SetString(TREE_OPTIONS_SCALE_DISTANCE_SCALE_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_SCALE_NAME_CAPTION_DISTANCE_SCALE);
      message.ArgumentList.SetString(TREE_OPTIONS_SCALE_NAME_CAPTION_DISTANCE_SCALE_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_DISTANCE_SCALE_LENGTH);
      message.ArgumentList.SetString(TREE_OPTIONS_DISTANCE_SCALE_LENGTH_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_DISTANCE_TICK_INTERVAL);
      message.ArgumentList.SetString(TREE_OPTIONS_DISTANCE_TICK_INTERVAL_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_SCALE_TIME_SCALE);
      message.ArgumentList.SetString(TREE_OPTIONS_SCALE_TIME_SCALE_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_SCALE_NAME_CAPTION_TIME_SCALE);
      message.ArgumentList.SetString(TREE_OPTIONS_SCALE_NAME_CAPTION_TIME_SCALE_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_SCALE_MAJOR_TICK);
      message.ArgumentList.SetString(TREE_OPTIONS_SCALE_MAJOR_TICK_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_SCALE_MINOR_TICK);
      message.ArgumentList.SetString(TREE_OPTIONS_SCALE_MINOR_TICK_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR);
      message.ArgumentList.SetString(TREE_OPTIONS_SCALE_NODE_HEIGHT_ERR_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_SCALE_FONT);
      message.ArgumentList.SetString(TREE_OPTIONS_SCALE_FONT_INDEX, node.GetElementAttribute('value'));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;



procedure fetchTreeCutoffOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_TREE_OPTIONS_CUTOFF_COMPLETE);
      node := document.GetElementById(TREE_OPTIONS_CUTOFF_CONDENSED);
      message.ArgumentList.SetString(TREE_OPTIONS_CUTOFF_CONDENSED_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_CUTOFF_CONSENSUS);
      message.ArgumentList.SetString(TREE_OPTIONS_CUTOFF_CONSENSUS_INDEX, Node.GetValue);
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchTreeBranchOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_TREE_OPTIONS_BRANCH_COMPLETE);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_LINES);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_LINES_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_DISPLAY_STATS);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_DISPLAY_STATS_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_BRANCH_STAT_PLACEMENT);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_STAT_PLACEMENT_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_HORIZONTAL);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_HORIZONTAL_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_VERTICAL);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_VERTICAL_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_HIDE_LOWER);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_HIDE_LOWER_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_BRANCH_HIDE_VALUES);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_HIDE_VALUES_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_DISPLAY_BRANCH);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_DISPLAY_BRANCH_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_BRANCH_BRANCH_PLACEMENT);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_BRANCH_PLACEMENT_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_PRECISION);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_PRECISION_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_HIDE_SHORTER);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_HIDE_SHORTER_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_HIDE_SHORTER);
      message.ArgumentList.SetString(TREE_OPTIONS_HIDE_SHORTER_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_DISPLAY_DIVERGENCE);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_DISPLAY_DIVERGENCE_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_BRANCH_TIME_PLACEMENT);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_TIME_PLACEMENT_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_DIVERGENCE_PRECISION);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_DIVERGENCE_PRECISION_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_DISTANCE_HORIZONTAL);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_DISTANCE_HORIZONTAL_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_DISTANCE_VERTICAL);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_DISTANCE_VERTICAL_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_BRANCH_STATISTICS_FONT);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_STATISTICS_FONT_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(TREE_OPTIONS_BRANCH_LENGTH_FONT);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_LENGTH_FONT_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(TREE_OPTIONS_BRANCH_DIVERGENCE_FONT);
      message.ArgumentList.SetString(TREE_OPTIONS_BRANCH_DIVERGENCE_FONT_INDEX, Node.GetElementAttribute('value'));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchTreeOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_TREE_OPTIONS_TREE_COMPLETE);
      node := document.GetElementById(TREE_OPTIONS_RECT_TAXON_SEPARATION);
      message.ArgumentList.SetString(TREE_OPTIONS_RECT_TAXON_SEPARATION_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_RECT_WIDTH);
      message.ArgumentList.SetString(TREE_OPTIONS_RECT_WIDTH_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_TREE_TAXON_NAME_CIRCLE);
      message.ArgumentList.SetString(TREE_OPTIONS_TREE_TAXON_NAME_CIRCLE_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_CIRCLE_START_ANGLE);
      message.ArgumentList.SetString(TREE_OPTIONS_CIRCLE_START_ANGLE_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_CIRCLE_RADIUS);
      message.ArgumentList.SetString(TREE_OPTIONS_CIRCLE_RADIUS_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_CIRCLE_CENTER_HOLE);
      message.ArgumentList.SetString(TREE_OPTIONS_CIRCLE_CENTER_HOLE_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_TREE_TAXON_NAME_RAD);
      message.ArgumentList.SetString(TREE_OPTIONS_TREE_TAXON_NAME_RAD_INDEX, Node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(TREE_OPTIONS_RAD_BRANCH_LENGTH);
      message.ArgumentList.SetString(TREE_OPTIONS_RAD_BRANCH_LENGTH_INDEX, Node.GetValue);
      node := document.GetElementById(TREE_OPTIONS_RAD_START_ANGLE);
      message.ArgumentList.SetString(TREE_OPTIONS_RAD_START_ANGLE_INDEX, Node.GetValue);
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchSubtreeDrawingOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_SUBTREE_DRAWING_OPTIONS_COMPLETE);
      node := document.GetElementById(SUBTREE_OPTIONS_DISPLAY_FONT);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_DISPLAY_FONT_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(SUBTREE_OPTIONS_CAPTION_FONT);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_CAPTION_FONT_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(SUBTREE_OPTIONS_NAME_CAPTION);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_NAME_CAPTION_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_OPTIONS_NODE_SHAPE);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_NODE_SHAPE_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_COLOR1);
      message.ArgumentList.SetString(SUBTREE_COLOR1_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(SUBTREE_OPTIONS_APPLY_TO_TAXON_MARKERS);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_APPLY_TO_TAXON_MARKERS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_BRANCH_LINES);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_BRANCH_LINES_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_COLOR2);
      message.ArgumentList.SetString(SUBTREE_COLOR2_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(SUBTREE_OPTIONS_BRANCH_WIDTH);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_BRANCH_WIDTH_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_OPTIONS_BRANCH_STYLE);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_BRANCH_STYLE_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_OPTIONS_DISPLAY_CAPTION);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_DISPLAY_CAPTION_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_ALIGN_VERT);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_ALIGN_VERT_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_DISPLAY_BRACKET);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_DISPLAY_BRACKET_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_BRACKET_STYLE);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_BRACKET_STYLE_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_COLOR3);
      message.ArgumentList.SetString(SUBTREE_COLOR3_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(SUBTREE_DRAWING_OPTIONS_LINE_WIDTH);
      message.ArgumentList.SetString(SUBTREE_DRAWING_OPTIONS_LINE_WIDTH_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_OPTIONS_DISPLAY_TAXON_NAME);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_DISPLAY_TAXON_NAME_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_DISPLAY_NODE);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_DISPLAY_NODE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_DISPLAY_TAXON_MARKER);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_DISPLAY_TAXON_MARKER_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_COMPRESS);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_COMPRESS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_DRAWING_OPTIONS_VERTICAL_UNIT);
      message.ArgumentList.SetString(SUBTREE_DRAWING_OPTIONS_VERTICAL_UNIT_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_OPTIONS_FILL_PATTERN);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_FILL_PATTERN_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_OPTIONS_DISPLAY_IMAGE);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_DISPLAY_IMAGE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_STYLE_OPTIONS);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_STYLE_OPTIONS_INDEX, Node.GetValue);
      node := document.GetElementById(SUBTREE_OPTIONS_OVERWRITE);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_OVERWRITE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_IMAGE_PATH);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_IMAGE_PATH_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(SUBTREE_OPTIONS_CLEAR_IMAGE);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_CLEAR_IMAGE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_OPTIONS_EXPORT_IMAGE);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_EXPORT_IMAGE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SUBTREE_COLOR4);
      message.ArgumentList.SetString(SUBTREE_COLOR4_INDEX, Node.GetElementAttribute('value'));
      node := document.GetElementById(SUBTREE_OPTIONS_GROUP_NAME);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_GROUP_NAME_INDEX, node.GetElementAttribute('value'));
      node := document.GetElementById(SUBTREE_OPTIONS_NODE_ID);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_NODE_ID_INDEX, node.GetElementAttribute('value'));
      node := document.GetElementById(SUBTREE_OPTIONS_DEFAULTS);
      message.ArgumentList.SetString(SUBTREE_OPTIONS_DEFAULTS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchClustalCodingOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_CLUSTALCODINGOPTIONS_COMPLETE);
      node := document.GetElementById(CLUSTALW_CODONS_PAIRWISE_GAP_OPENING_PENALTY);
      message.ArgumentList.SetString(CLUSTALW_CODONS_PAIRWISE_GAP_OPENING_PENALTY_INDEX, Node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_PAIRWISE_GAP_EXTENSION_PENALTY);
      message.ArgumentList.SetString(CLUSTALW_CODONS_PAIRWISE_GAP_EXTENSION_PENALTY_INDEX, Node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_MULTIPLE_GAP_OPENING_PENALTY);
      message.ArgumentList.SetString(CLUSTALW_CODONS_MULTIPLE_GAP_OPENING_PENALTY_INDEX, Node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_MULTIPLE_GAP_EXTENSION_PENALTY);
      message.ArgumentList.SetString(CLUSTALW_CODONS_MULTIPLE_GAP_EXTENSION_PENALTY_INDEX, Node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_PROTEIN_WEIGHT_MATRIX);
      message.ArgumentList.SetString(CLUSTALW_CODONS_PROTEIN_WEIGHT_MATRIX_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_RESIDUE_SPECIFIC_PENALTIES);
      message.ArgumentList.SetString(CLUSTALW_CODONS_RESIDUE_SPECIFIC_PENALTIES_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_HYDROPHILIC_PENALTIES);
      message.ArgumentList.SetString(CLUSTALW_CODONS_HYDROPHILIC_PENALTIES_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_GAP_SEPARATION);
      message.ArgumentList.SetString(CLUSTALW_CODONS_GAP_SEPARATION_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_END_GAP_SEPARATION);
      message.ArgumentList.SetString(CLUSTALW_CODONS_END_GAP_SEPARATION_INDEX, Node.GetValue);
      node := document.GetElementById(SELECT_GENETIC_CODE);
      message.ArgumentList.SetString(SELECT_GENETIC_CODE_INDEX, node.GetElementAttribute('codeName'));
      node := document.GetElementById(CLUSTALW_CODONS_USE_NEGATIVE_MATRIX);
      message.ArgumentList.SetString(CLUSTALW_CODONS_USE_NEGATIVE_MATRIX_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_DIVERGENT_CUTOFF);
      message.ArgumentList.SetString(CLUSTALW_CODONS_DIVERGENT_CUTOFF_INDEX, Node.GetValue);
      node := document.GetElementById(CLUSTALW_CODONS_PREDEFINED_GAP);
      message.ArgumentList.SetString(CLUSTALW_CODONS_PREDEFINED_GAP_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(CLUSTALW_CODONS_UPLOAD_TREE);
      message.ArgumentList.SetString(CLUSTALW_CODONS_UPLOAD_TREE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(CLUSTALW_GENETIC_CODE_EXPORT_TABLE);
      message.ArgumentList.SetString(CLUSTALW_GENETIC_CODE_EXPORT_TABLE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(CLUSTALW_GENETIC_CODE_EXPORT_STATISTICS);
      message.ArgumentList.SetString(CLUSTALW_GENETIC_CODE_EXPORT_STATISTICS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(GENETIC_CODE_VIEW_EXPORT_TYPE);
      message.ArgumentList.SetString(GENETIC_CODE_VIEW_EXPORT_TYPE_INDEX, node.GetValue);
      node := document.GetElementById(GENETIC_CODE_STATS_EXPORT_TYPE);
      message.ArgumentList.SetString(GENETIC_CODE_STATS_EXPORT_TYPE_INDEX, node.GetValue);

    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchClustalProteinOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_CLUSTALPROTEINOPTIONS_COMPLETE);
      node := document.GetElementById(CLUSTALW_PROTEIN_PAIRWISE_GAP_OPENING_PENALTY);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_PAIRWISE_GAP_OPENING_PENALTY_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_PAIRWISE_GAP_EXTENSION_PENALTY);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_PAIRWISE_GAP_EXTENSION_PENALTY_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_MULTIPLE_GAP_OPENING_PENALTY);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_MULTIPLE_GAP_OPENING_PENALTY_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_MULTIPLE_GAP_EXTENSION_PENALTY);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_MULTIPLE_GAP_EXTENSION_PENALTY_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_WEIGHT_MATRIX);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_WEIGHT_MATRIX_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_GAP_SEPARATION);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_GAP_SEPARATION_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_USE_NEGATIVE_MATRIX);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_USE_NEGATIVE_MATRIX_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_DIVERGENT_CUTOFF);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_DIVERGENT_CUTOFF_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_PREDEFINED_GAP);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_PREDEFINED_GAP_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(CLUSTALW_PROTEIN_UPLOAD_TREE);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_UPLOAD_TREE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(CLUSTALW_PROTEIN_RESIDUE_SPECIFIC_PENALTIES);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_RESIDUE_SPECIFIC_PENALTIES_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_HYDROPHILIC_PENALTIES);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_HYDROPHILIC_PENALTIES_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_PROTEIN_END_GAP_SEPARATION);
      message.ArgumentList.SetString(CLUSTALW_PROTEIN_END_GAP_SEPARATION_INDEX, node.GetValue);
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchClustalDnaOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_CLUSTALDNAOPTIONS_COMPLETE);
      node := document.GetElementById(CLUSTALW_DNA_PAIRWISE_GAP_OPENING_PENALTY);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + CLUSTALW_DNA_PAIRWISE_GAP_OPENING_PENALTY + 'value');
      message.ArgumentList.SetString(CLUSTALW_DNA_PAIRWISE_GAP_OPENING_PENALTY_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_DNA_PAIRWISE_GAP_EXTENSION_PENALTY);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + CLUSTALW_DNA_PAIRWISE_GAP_EXTENSION_PENALTY + 'value');
      message.ArgumentList.SetString(CLUSTALW_DNA_PAIRWISE_GAP_EXTENSION_PENALTY_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_DNA_MULTIPLE_GAP_OPENING_PENALTY);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + CLUSTALW_DNA_MULTIPLE_GAP_OPENING_PENALTY + 'value');
      message.ArgumentList.SetString(CLUSTALW_DNA_MULTIPLE_GAP_OPENING_PENALTY_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_DNA_MULTIPLE_GAP_EXTENSION_PENALTY);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + CLUSTALW_DNA_MULTIPLE_GAP_EXTENSION_PENALTY + 'value');
      message.ArgumentList.SetString(CLUSTALW_DNA_MULTIPLE_GAP_EXTENSION_PENALTY_INDEX, node.GetValue);
      node := document.GetElementById(SELECT_DNA_WEIGHT_MATRIX);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + SELECT_DNA_WEIGHT_MATRIX + 'value');
      message.ArgumentList.SetString(SELECT_DNA_WEIGHT_MATRIX_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_DNA_TRANSITION_WEIGHT);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + CLUSTALW_DNA_TRANSITION_WEIGHT + 'value');
      message.ArgumentList.SetString(CLUSTALW_DNA_TRANSITION_WEIGHT_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_DNA_USE_NEGATIVE_MATRIX);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + CLUSTALW_DNA_USE_NEGATIVE_MATRIX + 'value');
      message.ArgumentList.SetString(CLUSTALW_DNA_USE_NEGATIVE_MATRIX_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_DNA_DIVERGENT_CUTOFF);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + CLUSTALW_DNA_DIVERGENT_CUTOFF + 'value');
      message.ArgumentList.SetString(CLUSTALW_DNA_DIVERGENT_CUTOFF_INDEX, node.GetValue);
      node := document.GetElementById(CLUSTALW_DNA_PREDEFINED_GAP);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + CLUSTALW_DNA_PREDEFINED_GAP + 'value');
      message.ArgumentList.SetString(CLUSTALW_DNA_PREDEFINED_GAP_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(CLUSTALW_DNA_UPLOAD_TREE);
      if not Assigned(node) then
        Raise Exception.Create('missing ' + CLUSTALW_DNA_UPLOAD_TREE + 'value');
      message.ArgumentList.SetString(CLUSTALW_DNA_UPLOAD_TREE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchSeqDataExportOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_SEQDATAEXPORTOPTIONS_COMPLETE);
      node := document.GetElementById(SDEO_FILE_FORMAT_ID);
      message.ArgumentList.SetString(SDEO_FILE_FORMAT_INDEX, node.GetValue);
      node := document.GetElementById(SDEO_SITES_PER_LINE_ID);
      message.ArgumentList.SetString(SDEO_SITES_PER_LINE_INDEX, node.GetValue);
      node := document.GetElementById(SDEO_IS_INTERLEAVED_ID);
      message.ArgumentList.SetString(SDEO_IS_INTERLEAVED_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SDEO_WRITING_SITE_NUMBERS_ID);
      message.ArgumentList.SetString(SDEO_WRITING_SITE_NUMBERS_INDEX, node.GetValue);
      node := document.GetElementById(SDEO_INCLUDE_FIRST_POSITION_ID);
      message.ArgumentList.SetString(SDEO_INCLUDE_FIRST_POSITION_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SDEO_INCLUDE_SECOND_POSITION_ID);
      message.ArgumentList.SetString(SDEO_INCLUDE_SECOND_POSITION_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SDEO_INCLUDE_THIRD_POSITION_ID);
      message.ArgumentList.SetString(SDEO_INCLUDE_THIRD_POSITION_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SDEO_INCLUDE_NONCODING_POSITIONS_ID);
      message.ArgumentList.SetString(SDEO_INCLUDE_NONCODING_POSITIONS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SDEO_GAP_TREATMENT_ID);
      message.ArgumentList.SetString(SDEO_GAP_TREATMENT_INDEX, node.GetValue());
      node := document.GetElementById(SDEO_INCLUDED_SITES_ID);
      message.ArgumentList.SetString(SDEO_INCLUDED_SITES_INDEX, node.GetValue);
      node := document.GetElementById(SDEO_SEQDATA_TITLE_ID);
      message.ArgumentList.SetString(SDEO_SEQDATA_TITLE_INDEX, node.GetValue);
      node := document.GetElementById(SDEO_SEQDATA_DESCRIPTION_ID);
      message.ArgumentList.SetString(SDEO_SEQDATA_DESCRIPTION_INDEX, node.GetValue);
      node := document.GetElementById(SDEO_FULL_CODONS_ID);
      message.ArgumentList.SetString(SDEO_FULL_CODONS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchDistMatrixExportOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_DISTEXPORTOPTIONS_COMPLETE);
      node := document.GetElementById(DMEO_FILE_FORMAT_ID);
      message.ArgumentList.SetString(DMEO_FILE_FORMAT_INDEX, node.GetValue);
      node := document.GetElementById(DMEO_EXPORT_TYPE_ID);
      message.ArgumentList.SetString(DMEO_EXPORT_TYPE_INDEX, node.GetValue);
      node := document.GetElementById(DMEO_MATRIX_LAYOUT1_ID);
      message.ArgumentList.SetString(DMEO_MATRIX_LAYOUT1_INDEX, node.GetValue);
      node := document.GetElementById(DMEO_MATRIX_LAYOUT2_ID);
      message.ArgumentList.SetString(DMEO_MATRIX_LAYOUT2_INDEX, node.GetValue);
      node := document.GetElementById(DMEO_SHOW_DISTANCES_ID);
      message.ArgumentList.SetString(DMEO_SHOW_DISTANCES_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(DMEO_SHOW_DISTANCES_AND_STD_ERRORS_ID);
      message.ArgumentList.SetString(DMEO_SHOW_DISTANCES_AND_STD_ERRORS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(DMEO_PRECISION_ID);
      message.ArgumentList.SetInt(DMEO_PRECISION_INDEX, StrToInt(String(node.GetValue)));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchSeqsFromHtml(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
Var
  message: ICefProcessMessage;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_FETCHSEQSFROMHTML_EXPORT);
      With message.ArgumentList do
      begin
        SetString(0, document.Body.ElementInnerText);
      end;
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchSeqNameOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_SEQNAME_OPTIONS_COMPLETE);
      node := document.GetElementById(SEQNAME_FIRST);
      message.ArgumentList.SetString(SEQNAME_FIRST_INDEX, node.GetValue);
      node := document.GetElementById(SEQNAME_SECOND);
      message.ArgumentList.SetString(SEQNAME_SECOND_INDEX, node.GetValue);
      node := document.GetElementById(SEQNAME_THIRD);
      message.ArgumentList.SetString(SEQNAME_THIRD_INDEX, node.GetValue);
      node := document.GetElementById(SEQNAME_FOURTH);
      message.ArgumentList.SetString(SEQNAME_FOURTH_INDEX, node.GetValue);
      node := document.GetElementById(SEQNAME_USE_INITIAL);
      message.ArgumentList.SetString(SEQNAME_USE_INITIAL_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(SEQNAME_FULL_INFO);
      message.ArgumentList.SetString(SEQNAME_FULL_INFO_INDEX, node.GetValue);
      node := document.GetElementById(SEQNAME_SEQ_LABEL);
      message.ArgumentList.SetString(SEQNAME_SEQ_LABEL_INDEX, node.GetValue);
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchEditAlignmnentWelcomeOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_ALIGN_EDIT_WELCOME_OPTIONS_COMPLETE);
      node := document.GetElementById(CREATE_NEW_ALIGNMENT);
      message.ArgumentList.SetString(CREATE_NEW_ALIGNMENT_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(OPEN_SAVED_ALIGNMENT);
      message.ArgumentList.SetString(OPEN_SAVED_ALIGNMENT_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
      node := document.GetElementById(RETRIEVE_SEQUENCE_FROM_FILE);
      message.ArgumentList.SetString(RETRIEVE_SEQUENCE_FROM_FILE_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchGeneticCodeTableOptions(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_GENETIC_CODE_TABLE_OPTIONS_COMPLETE);
      node := document.GetElementById(SEQDATA_GENETIC_CODE_NAME);
      message.ArgumentList.SetString(SEQDATA_GENETIC_CODE_NAME_INDEX, node.GetElementAttribute('codeName'));
      node := document.GetElementById(SEQDATA_GENETIC_CODE_TABLE);
      message.ArgumentList.SetString(SEQDATA_GENETIC_CODE_TABLE_INDEX, node.GetElementAttribute('codeTable'));
      node := document.GetElementById(SEQDATA_GENETIC_CODE_VIEW_EXPORT_TYPE);
      message.ArgumentList.SetString(SEQDATA_GENETIC_CODE_VIEW_EXPORT_TYPE_INDEX, node.GetValue);
      node := document.GetElementById(SEQDATA_GENETIC_CODE_STATS_EXPORT_TYPE);
      message.ArgumentList.SetString(SEQDATA_GENETIC_CODE_STATS_EXPORT_TYPE_INDEX, node.GetValue);
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure fetchJSMessageDialog(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_JS_MESSAGE_DIALOG_COMPLETE);
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        node := document.GetElementById(JS_MESSAGE);
        message.ArgumentList.SetString(JS_MESSAGE_INDEX, node.GetElementInnerText);
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure placeholderLoaded(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_PLACEHOLDERLOADED_COMPLETE);
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        node := document.GetElementById(JS_MESSAGE);
        message.ArgumentList.SetString(JS_MESSAGE_INDEX, node.GetElementInnerText);
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure copyAllToClipboard(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
Var
  message: ICefProcessMessage;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_COPYALLTOCLIPBOARD_COMPLETE);
      With message.ArgumentList do
      begin
        SetString(0, document.Body.ElementInnerText);
      end;
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure macSaveCaptionToText(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_MACSAVECAPTIONTOTEXT_COMPLETE);
      if not message.ArgumentList.SetString(0, BrowserMessage) then
        raise Exception.Create('failed to add message to argument list');
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure saveCaptionToText(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
Var
  message: ICefProcessMessage;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_SAVECAPTIONTOTEXT_COMPLETE);
      With message.ArgumentList do
      begin
        SetString(0, document.Body.ElementInnerText);
      end;
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure printCaption(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
Var
  message: ICefProcessMessage;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_PRINTCAPTION_COMPLETE);
      With message.ArgumentList do
      begin
        SetString(0, document.Body.ElementInnerText);
      end;
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure getAppDepotSelections(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
var
  message: ICefProcessMessage;
  node: ICefDomNode;
  inputNode: ICefDomNode;
  isChecked: Boolean = False;
  cssClass: ustring = '';
  processTypes: ustring = '';
  app_name: ustring = '';
  icon_file: ustring = '';
  index: Integer = 0;
begin
  try
    message := TCefProcessMessageRef.New(VISITDOMPROC_APPDEPOT_GET_SELECTIONS_COMPLETE);
    with message.ArgumentList do
    begin
      node := document.GetElementById(APPS_CONTAINER);
      node := node.GetFirstChild; { this is the header div}
      node := node.NextSibling;
      while node <> nil do
      begin
        cssClass := node.GetElementAttribute('class');
        isChecked := ContainsText(String(cssClass), 'active');
        if isChecked then
        begin
          inputNode := node.GetFirstChild;
          processTypes := 'process_types=' + inputNode.GetElementAttribute('process_types');
          app_name := 'app_name=' + inputNode.GetElementAttribute('app_name');
          icon_file := 'icon_file=' + inputNode.GetElementAttribute('icon_file');
          message.ArgumentList.SetString(index, processTypes + ',' + icon_file + ',' + app_name);
          inc(index);
        end;
        node := node.NextSibling;
      end;
      //SetString(0, processTypes);
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure getCDSsToImport(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
Var
  message: ICefProcessMessage;
  node: ICefDomNode;
  checkBoxNode: ICefDomNode;
  divNode: ICefDomNode;
  temp: ustring;
  index: Integer = 0;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_SELECT_CDS_TO_IMPORT_COMPLETE);
      With message.ArgumentList do
      begin
        node := document.GetElementById(CDS_IMPORT_ONLY_CDS);
        SetString(CDS_IMPORT_ONLY_CDS_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
        node := document.GetElementById(CDS_IMPORT_ALL_DATA);
        SetString(CDS_IMPORT_ALL_DATA_INDEX, node.GetElementAttribute(DMEO_IS_CHECKED));
        node := document.GetElementById(CDS_FIELDSET);
        index := CDS_IMPORT_ALL_DATA_INDEX + 1;
        divNode := document.GetElementById('gp0');
        if divNode = nil then
          raise Exception.Create('invalid CDS import html file');
        while divNode <> nil do
        begin
          temp := '';
          checkBoxNode := divNode.FirstChild; { header tag}
          if checkBoxNode = nil then
          begin
            SetString(index, 'the next tag not found: ' + ustring(BoolToStr(divNode.IsElement, True)));
            break;
          end;
          checkBoxNode := checkBoxNode.NextSibling; { checkbox tag}
          if checkBoxNode.GetElementAttribute(DMEO_IS_CHECKED) = 'true' then
            temp := temp + 'T'
          else
            temp := temp + 'F';
          checkBoxNode := checkBoxNode.NextSibling; { label tag}
          checkBoxNode := checkBoxNode.NextSibling; { br tag}
          checkBoxNode := checkBoxNode.NextSibling; { br tag}
          while checkBoxNode <> nil do
          begin
            checkBoxNode := checkBoxNode.NextSibling; { checkbox tag}
            if Assigned(checkboxNode) then
            begin
              if checkBoxNode.GetElementAttribute(DMEO_IS_CHECKED) = 'true' then
                temp := temp + 'T'
              else
                temp := temp + 'F';
              checkBoxNode := checkBoxNode.NextSibling; { label tag}
              checkBoxNode := checkBoxNode.NextSibling; { br tag}
              checkBoxNode := checkBoxNode.NextSibling; { br tag}
            end;
          end;
          SetString(index, temp);
          divNode := divNode.NextSibling;
          if not Assigned(divNode) then
            break;
          inc(index);
        end;
      end;
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;
procedure domVisitorCallback(const browser: ICefBrowser ;const frame: ICefFrame; const document: ICefDomDocument);
Var
  message: ICefProcessMessage;
  debug: String;
begin
  try
    try
      message := TCefProcessMessageRef.New(VISITDOMPROC_DOMVISITORCALLBACK_COMPLETE);
      With message.ArgumentList do
      begin
        SetString(0, document.Body.ElementInnerText);
      end;
    except
      on E:Exception do
      begin
        message.ArgumentList.SetString(0, RENDER_PROCESS_ERROR + ': ' + ustring(E.Message));
        {$IFDEF DEBUG}
        debug := GetExceptionDump(E);
        WriteToLog(E.Message);
        WriteToLog(debug);
        {$ENDIF}
      end;
    end;
  finally
    frame.SendProcessMessage(PID_BROWSER, message);
  end;
end;

procedure GlobalCEFApp_OnScheduleMessagePumpWork(const aDelayMS : int64);
begin
  {$IFDEF DARWIN}
  if (GlobalCEFWorkScheduler <> nil) then GlobalCEFWorkScheduler.ScheduleMessagePumpWork(aDelayMS);
  {$ENDIF}
end;

procedure GlobalCEFApp_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                const frame         : ICefFrame;
                                                      sourceProcess : TCefProcessId;
                                                const message       : ICefProcessMessage;
                                                var   aHandled      : boolean);
var
  debug : String;
  TempVisitor : TCefFastDomVisitor2;
begin
  aHandled := False;
  {$IFDEF DEBUG}
  WriteToLog('OnProcessMessageReceived: ' + String(message.Name));
  {$ENDIF}

  try
    if message.Name = VISITDOMPROC_CHECK_FORM_VALIDATED then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, checkFormValidated);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_FETCHSEQSFROMHTML then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchSeqsFromHtml);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_COPYALLTOCLIPBOARD then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, copyAllToClipboard);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_SAVECAPTIONTOTEXT then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, saveCaptionToText);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_MACSAVECAPTIONTOTEXT then
    begin
      BrowserMessage := message.ArgumentList.GetString(0);
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, macSaveCaptionToText);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_PRINTCAPTION then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, printCaption);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_DOMVISITORCALLBACK then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, domVisitorCallback);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_DISTEXPORTOPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchDistMatrixExportOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_SEQDATAEXPORTOPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchSeqDataExportOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_CLUSTALDNAOPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchClustalDnaOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_CLUSTALPROTEINOPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchClustalProteinOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_CLUSTALCODINGOPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchClustalCodingOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_SUBTREE_DRAWING_OPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchSubtreeDrawingOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_TREE_OPTIONS_TREE then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchTreeOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_TREE_OPTIONS_BRANCH then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchTreeBranchOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_TREE_OPTIONS_SCALE then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchTreeScaleOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_TREE_OPTIONS_LABELS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchTreeLabelOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_TREE_OPTIONS_CUTOFF then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchTreeCutoffOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_INPUT_DATA_OPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchInputDataOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_NEWICK_EXPORT_OPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchNewickExportOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_ALIGN_EDIT_WELCOME_OPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchEditAlignmnentWelcomeOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_SEQNAME_OPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchSeqNameOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_GENETIC_CODE_TABLE_OPTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchGeneticCodeTableOptions);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_JS_MESSAGE_DIALOG then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, fetchJSMessageDialog);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_PLACEHOLDERLOADED then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, placeholderLoaded);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_SELECT_CDS_TO_IMPORT then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, getCDSsToImport);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else if message.Name = VISITDOMPROC_APPDEPOT_GET_SELECTIONS then
    begin
      TempVisitor := TCefFastDomVisitor2.Create(browser, frame, getAppDepotSelections);
      frame.VisitDom(TempVisitor);
      aHandled := True;
    end
    else
    begin
      {$IFDEF DEBUG}
      raise Exception.Create('missing dom procedure: ' + String(message.Name));
      {$ENDIF}
      aHandled := False;
      //aHandled := inherited OnProcessMessageReceived(browser, sourceProcess, message);
    end;
  except
    on E:Exception do
    begin
      aHandled := False;
      {$IFDEF DEBUG}
      debug := GetExceptionDump(E);
      WriteToLog('ERROR in OnProcessMessageReceived');
      WriteToLog(debug);
      WriteToLog(E.Message);
      {$ENDIF}
    end;
  end;
end;

procedure CreateGlobalCEFApp(aApplicationDirectory: ustring = '');
var
  aPath: ustring;
begin
  {$IFDEF DARWIN}
  GlobalCEFWorkScheduler := TCEFWorkScheduler.Create(nil);
  {$ENDIF}

  GlobalCEFApp := TCefApplication.Create;
  if ((aApplicationDirectory <> '') and DirectoryExists(aApplicationDirectory)) then
  begin
    aPath := aApplicationDirectory + 'locales';
    GlobalCEFApp.LocalesDirPath := aPath;
  end;

  InitProcessMessagesHandler;
  {$IFNDEF HELPER}
  GlobalCEFApp.LogFile := ustring(GetPrivateOutputFile(mfBrowserLogFile));
  GlobalCEFApp.Cache := ustring(GetPrivateOutputFile(mfBrowserCacheFile));
  {$ENDIF}

  GlobalCEFApp.WindowlessRenderingEnabled := False;
  GlobalCEFApp.EnableGPU := True;
  {$IFNDEF LINUX}
  GlobalCEFApp.EnableHighDPISupport := True;
  {$ENDIF}
  {$IFDEF DARWIN}
  GlobalCEFApp.ExternalMessagePump       := True;
  GlobalCEFApp.MultiThreadedMessageLoop  := False;
  GlobalCEFApp.OnScheduleMessagePumpWork := GlobalCEFApp_OnScheduleMessagePumpWork;
  GlobalCEFApp.UseMockKeyChain := True;
  {$ENDIF}

  {$IFDEF DEBUG}
  GlobalCEFApp.LogSeverity := LOGSEVERITY_INFO;
  {$ELSE}
  GlobalCEFApp.LogSeverity := LOGSEVERITY_ERROR;
  {$ENDIF}
end;

procedure InitProcessMessagesHandler;
begin
  GlobalCEFApp.OnProcessMessageReceived := GlobalCEFApp_OnProcessMessageReceived;
end;
end.
