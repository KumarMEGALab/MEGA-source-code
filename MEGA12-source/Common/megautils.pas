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

unit MegaUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, FileUtil, Dos, StdCtrls, types,
  Forms, SysUtils, MegaConsts, MSingleList, Classes,
  MegaVerConsts, controls, Graphics, variants,
  Registry, KeywordConsts, FPCanvas,
  Dialogs, {$IFDEF MSWINDOWS}jwapsapi,{$ENDIF}
  MLTree, MD_Sequences, MProcessPack, MDistPack, fpjson;


type TWinVersion = (wvUnknown, wvWin95, wvWin98, wvWin98SE, wvWinNT, wvWinME, wvWin2000, wvWinXP, wvWinVista, wvWin7);

function ComputeBootstrapStdDev(trueBcl: Double; replicateBclVals: TArrayOfExt): Double;
function ExpectedBclPrecision(aBcl: Double; numReplicates: Integer): Double;
function CheckSupportValuesMakeSense(const trueBcl: Double; const replicateBclVals: TArrayOfExt; var aMedian: Double; var aMean: Double): Boolean;
function ValuesAreClose(v1: Extended; v2: Extended; relativeTol: Extended = 1E-9; absoluteTol: Extended = 1E-8): Boolean;
function DebugIntArrayString(aIntArray: PArrayOfLongint; n: Integer): String;
function MLInitTreeTypeToString(aType: Integer): String;
function PointInRect(p: TPoint; r: TRect; inclusive: Boolean = True): Boolean;
function ScalingFactor: Double;
function ValidPenStyle(aPenStyle: TPenStyle): TPenStyle;
function ValidBrushStyle(aBrushStyle: TBrushStyle): TBrushStyle;
function ValidFontStyles(aFontStyle: TFontStyles): TFontStyles;
function FindAvailableFontFromCssRule(css: String): String;
function UpdateFontFromCssString(var aFont: TFont; const css: String): Boolean;
function CssStringForFont(aFont: TFont): String;
function BoolToStrLC(aBool: Boolean; asString: Boolean = True): String;
function HtmlStringToTNodeMarkerShape(str: String): TNodeMarkerShape;
function NodeMarkerShapeToHtmlString(shape: TNodeMarkerShape): String;
function MarkerToAnchorString(aShape: TNodeMarkerShape; aColor: TColor): String;
function NodeMarkerShapeToUnicodeString(shape: TNodeMarkerShape): String;
function SColorToHtmlColor(Color: TColor): string; //convert TColor -> HTML color string #rrggbb
function SHtmlColorToColor(var s: string; out Len: integer; Default: TColor): TColor;  //convert string which starts with HTML color token #rgb, #rrggbb -> TColor, get len of color-string
function ColorToRgbCssString(aColor: TColor): String;

function IsCharWord(ch: char): boolean;
function IsCharHex(ch: char): boolean;
function StringToBranchOption(str: String): TBranchOption;
function StringToBracketStyle(str: String): TBracketStyle;
function BracketStyleToString(style: TBracketStyle): String;
function BranchOptionToStr(option: TBranchOption): String;
function StringToBrushStyle(str: String): TBrushStyle;
function BrushStyleToString(style: TBrushStyle): String;
function GraphicAlignToString(aAlign: TGraphicAlign): String;
function StringToGraphicAlign(str: String): TGraphicAlign;
function HtmlLineTypeStrToPenStyle(str: String): TPenStyle;
function PenStyleToHtmlLineTypeStr(style: TPenStyle): String;

function SessionIsSameTargetPlatform(const sessionTarget: LongInt; var msg: String): Boolean;
function VerifiedTargetPlatform: Integer;
function TargetPlatformString(const sessionTarget: LongInt): String;

function IsInRangeInclusive(testValue: Extended; rangeMin, rangeMax: Extended): Boolean;
function CustomStdDev(const AData : ArrayOfExtended) : Extended;
function CustomVariance(const AData: ArrayOfExtended; const n: Integer): Extended;
function CustomTotalVariance(const AData : ArrayOfExtended;Const n : Integer) : Extended;
procedure CustomSumsAndSquares(const AData : ArrayOfExtended; Const n : Integer; var Sum, SumOfSquares : Extended);
function IsRelTimeAnalysis(Option: TDistTreeDlgOption): Boolean; overload;
function IsRelTimeAnalysis(ProcessPack: TProcessPack): Boolean; overload;
function IsReltimeMLAnalysis(Option: TDistTreeDlgOption): Boolean;
function IsReltimeNonMLAnalysis(Option: TDistTreeDlgOption): Boolean;
function IsReltimeBlensAnalysis(Option: TDistTreeDlgOption): Boolean;
function IsCorrTestAnalysis(Option: TDistTreeDlgOption): Boolean;
function FloatTimeToDateTime(floatTime: Extended): TDateTime;
function FloatTimeToDateString(floatTime: Extended; formatStr: String): String;
function RtdtDaysElapsed(latestRtdtTime: Extended; rtdtDivTime: Extended): Int64;
procedure MegaRandSeed(seed: Longint);
function  PrevRandSeed: LongInt;
function  URan(m: Longint): Longint;  // uniform rn 0..m-1
function SortAndDumpBootTableToFile(f: PArrayOfInt; numSites: Int64; filename: String): Boolean;
function arrayToFile(source: array of extended; destination: String):Boolean;
// Mathmatic functions
function RandomFromLogNormalDist(x: Extended; aMean: Extended; aStdDev: Extended): Extended;
function RandomFromExponentialDist(x: Extended; decay: Extended): Extended;
function RandomFromExponentialDistInterval(aMin, aMax, decay: Extended): Extended;
function GammaLn(xx: Double): Double;
function GammaSer(a,x: Double; var GLn: Double): Double;
function GammaCfn(a, x: Double; var Gln: Double): Double;
function GammaQ(a, x: Double): Double;
function Factrl(n: Longint): Double;
function Factln(n: Longint): Double;
function ChiSqObsExp(const Bins, EBins: array of Double; ChSqValue: PDouble): Double;  // returns probability
function ChiSqTwoBins(const Bins1, Bins2: array of Double; ChSqValue: PDouble): Double;  // returns probability
function ChiSqObsExpForSingleList(Bins, EBins: TSingleList; MaxLen: Integer; ChSqValue: PDouble): Double;  // returns probability
function ChiSqTwoBinsForSingleList(Bins1, Bins2: TSingleList; MaxLen: Integer; ChSqValue: PDouble): Double;  // returns probability
function Beta(z, w: Double): Double;
function BetaCfn(a,b,x: Double): Double;
function Betai(a,b,x: Double): Double;
function DistanceFormula(point1: TPoint; point2: TPoint): Double;

function tTest(x: Double): Double;  // mean/stderr; one tailed result: calls zTest
function zTest(x: Double): Double;  // mean/stderr; one tailed result

function BinomialP(n, s: Double; p: Double): Double; // if input is not integer than fix it
function CoefficientOfVariation(Values: ArrayOfExtended): Extended;
function PercentDifference(Value1, Value2: Double): Double; // a percentage in the range of 0.0 to 100.0
function FisherExactTestOneTailed(Sd, S, Nd, N: Integer): Double;      // if input is not integer than fix it
function FisherExactTestTwoTailed(Sd, S, Nd, N: Integer): Double;      // if input is not integer than fix it
                    // n= trials, s: success, p=prob_success
function PoiDev(xm: Double): Double;

procedure Resample(var FFreqTable: ArrayOfInteger; TotalSites: Longint);
function RandomSequenceOfUniqueIntegers(numIntegers: LongInt): ArrayOfInteger;

// sort fns
procedure SortIntArray(p : PArrayOfInt; L, R : integer);
procedure SortExtArray(p: TArrayOfExt; L, R: Integer);

// file filters
function NewickFilesFilter: String;
function NewickAndTreeSessionFilesFilter: String;
function TreeSessionFilesFilter: String;
function MegaFilesFilter: String;
function MegaSessionFilesFilter: String;
function FastaFilesFilter: String;
function TextFilesFilter: String;
function AllFilesFilter: String;

function GetPrivateDir: String; overload;
function GetPrivateDir(AValue:String; CopyIfNeeded: Boolean = True): String; overload;
function GetAPPDATA: String;
function GetHomeDirectory: String;
function GetMuscleInputFile: String;
function ExcelExportToFileType(aType: TExportType): TOutputFileType;
function FileExtensionForExportType(aType: TExportType): String;
function FileExtensionForOutputFileType(aType: TOutputFileType): String;
function ExportIsWorkbookDisplay(aType: TExportType): Boolean;
function ExportIsWorkbookSave(aType: TExportType): Boolean;
function ExportIsExcel8(aType: TExportType): Boolean;
function ExportIsExcelXml(aType: TExportType): Boolean;
function ExportIsOds(aType: TExportType): Boolean;
function ExportIsCsv(aType: TExportType): Boolean;
function ExportIsText(aType: TExportType): Boolean;
function ExportIsTextEditorDisplay(aType: TExportType): Boolean;
function IsWorkbookFormat(aType: TOutputFileType): Boolean;
function StringToOutputFileType(aString: String): TOutputFileType;

{$IFDEF FPC}
function GetCurrentProcessId: DWord;

{$ENDIF}
function Base64ToStream(const ABase64: String; var AStream: TMemoryStream): Boolean;
function GetGuidString: String;
function GetMuscleOutputFile: String;
function GetMuscleRunLog: String;
function GetMuscleExe: String;
function GetNnlsExe: String;
function GetTEMP: String;
function GetMegaGlobalFile(FileName: String; CopyIfNeeded: Boolean = True): String;
function GetPrivateFile(AValue: String; CopyIfNeeded: Boolean = True): String;
function GetPrivateExecutableFile(AValue: String; CopyIfNeeded: Boolean = True): String; { for Linux, will chmod +x if needed}
function GetPrivateOutputFile(AValue: String): String;
function IsFileExecutable(const FileName: string): Boolean;
function NextAvailableFilename(DefaultName: String): String; // Returns a valid filename (one not already in use) based on the preferred (defaultName) name.
function NextAvailableFileNumber(DefaultName: String): String; overload;
function NextAvailableFileNumber(const targetDir: String; defaultsNamesList: TStringList): Integer; overload;
// code tables are available too
function GetNoOfDefaultCodeTables: Integer;
function GetDefaultCodeTableName(Index: Integer): String;
function GetDefaultCodeTable(Index: Integer): String;
function GetStandardGeneticCode: String;
function GetVertebrateMtGeneticCode : String;
function GetInvertebrateMtGeneticCode : String;
function GetYeastMtGeneticCode : String;
function GetMoldMtGeneticCode : String;
function GetCoelentrateMtGeneticCode : String;
function GetMycoplasmaGeneticCode : String;
function GetSpiroplasmaGeneticCode : String;
function GetCiliateNcGeneticCode : String;
function GetPasycladaceanNcGeneticCode : String;
function GetHexamitaNcGeneticCode : String;
function GetEchinodermGeneticCode : String;
function GetEuplotidNcGeneticCode : String;
function GetBacterialPlastidGeneticCode : String;
function GetPlantPlastidGeneticCode : String;
function GetAlternativeYeastMtGeneticCode : String;
function GetAscidianMtGeneticCode : String;
function GetFlatwormMtGeneticCode : String;
function GetBlepharismaGeneticCode : String;
function GetChlorophyceanMtGeneticCode : String;

// Function to get index of a string in a string list
function StrListIndexOf(AList: TStringList; AStr: AnsiString): LongInt;
function StrListCountDistinctStr(AList: TStringList): LongInt;
procedure GenerateStrListMapIds(MyGpNames: TStringList; MyGpIds: PArrayOfInt);

function ChangeInitialDirectorySaveDialogForMac(InitialDirectory : String): String;

//
procedure MegaSafeTrim(var S: AnsiString);  // it removes any ! or # from name
//Functions to cleanup names
procedure TrimTaxaName(var S: AnsiString);
procedure TrimTaxaName2(var S: String);
procedure TrimName(var S: AnsiString; MaxLen: Longint);
procedure TrimNameWithAllowAlphaNum(var S: AnsiString; MaxLen: Longint);
function  CaptionToText(S: AnsiString): AnsiString;
function CalculateIncrement(aValue: Extended): Double;
function CalculateCollapseClusterSizeIncrement(numOtus: Integer): Integer;
function CalculateCollapseClusterSizeInitialValue(numOtus: Integer): Integer;
function CountBits(x: Longint): Longint;

// General character handling routines
function IsUnambiguousAminoAcid(x: AnsiChar): Boolean;
function IsUnambiguousNucleotide(x: AnsiChar): Boolean;
function IsUnambiguousMappedNucleotide(x: AnsiChar): Boolean;
function GetThreeLetterCode(Ch: AnsiChar): AnsiString;
function ConvertStringToRecodeScheme(AList: AnsiString; var RecodeScheme: TArrayAtoZofChar; IgnoreBase: AnsiChar): AnsiString;  // always returns a 26 character long array of char
function RecodeNucOrAmino(Ch: AnsiChar; const RecodeScheme: TArrayAtoZofChar): AnsiChar;
function DistMapToNuc(x: AnsiChar): AnsiChar;
function DistMapToAmino(x: AnsiChar): AnsiChar;
function NucToDistMap(Ch: AnsiChar): AnsiChar;
function NucToParsimMap(x: AnsiChar): AnsiChar;
//function NucToTvParsimMap(x: Char): Char;
function AminoToDistMap(Ch: AnsiChar): AnsiChar;
function AminoToParsimMap(x: AnsiChar): Longint;
function ParsimMapToNuc(x: AnsiChar): AnsiChar;
function ParsimMapToNucStr(x: Longint): AnsiString;
function ParsimMapToAmino(x: Longint): AnsiChar;
function ParsimMapToAminoStr(x: Longint): AnsiString;

function IsNucColParsimInfo(CharArray: PAnsiChar; NTaxa: LongInt; var MinTLen: LongInt): Boolean;
function IsAminoColParsimInfo(CharArray: PAnsiChar; NTaxa: LongInt; var MinTLen: LongInt): Boolean;

function HasAmbiguousInNucCol(CharArray: PAnsiChar; NTaxa: LongInt): Boolean;
function HasAmbiguousInAminoCol(CharArray: PAnsiChar; NTaxa: LongInt): Boolean;
function HasAmbiguousInCodonMapCol(CharArray: PAnsiChar; NTaxa: LongInt): Boolean;

function CountAmbiguousInNucCol(CharArray: PAnsiChar; NTaxa: LongInt): Integer;
function CountAmbiguousInAminoCol(CharArray: PAnsiChar; NTaxa: LongInt): Integer;
function CountAmbiguousInCodonMapCol(CharArray: PAnsiChar; NTaxa: LongInt): Integer;

function HasStopAminoInAminoCol(CharArray: PAnsiChar; NTaxa: LongInt; lastCol: Boolean): Boolean;
function ConvertStopToMissingData(var CharArray: PAnsiChar; NTaxa: LongInt; lastCol: Boolean): Boolean;
function ConvertCodonMapStopToMissingData(var CharArray: PAnsiChar; NTaxa: LongInt; const ACodeTable: array of AnsiChar): Boolean;
function HasStopCodonInCodonMapCol(CharArray: PAnsiChar; NTaxa: LongInt; const ACodeTable: array of AnsiChar): Boolean;
function IsValidSiteLabel(x: AnsiChar): Boolean; overload;

//--- Distance stuff
procedure SwapRectDistEntries(D: PDistanceMatrix; n, x, y: Longint); // rect matrix
procedure SwapDistEntries(D: PDistanceMatrix; n, x, y: Longint); // Assumes that D in lowerleft
procedure FreeDistMatrix(Value: PDistanceMatrix; n: Longint);
function  NewDistMatrix(n: Longint; IsFullRect: Boolean): PDistanceMatrix;
function  NewDistArray(n: Longint): PArrayOfDouble;

function  CreateDistanceMatrix(NoOfOTUs: integer):PDistanceMatrix;
procedure DestroyDistanceMatrix(var D: PDistanceMatrix; NoOfOTUs: integer);
procedure CopyDistanceMatrix(Source, Copy: PDistanceMatrix; NoOfOTUs: integer);

procedure ConvertVarToStdErrMat(MyV: PDistanceMatrix; n: Longint; IsFullRect: Boolean);
procedure ConvertVarToStdErrArray(MyV: ArrayOfDouble; n: Longint);
procedure CopyDistMatrix(n: Longint; Dest, Source: PDistanceMatrix; IsFullRect: Boolean);
function UserVarMatrixToFile(m: PDistanceMatrix; filename: String; numSeqs: Integer): Boolean;



//====
function ConvertToMegaWinCaption(Value: AnsiString): AnsiString;
//--- Formatting strings etc
function BlankString(Value: Longint): AnsiString;
function StrToStrWidth(Value: AnsiString; Width: Longint):AnsiString;
function PadStrToWidth(Value: AnsiString; Width: Longint):AnsiString;
function OtuNameToMegaStr(Value: AnsiString): AnsiString;
procedure splitStr( strIn, sep: AnsiString; stl: TStrings; IgnoreEmpty: boolean = false);
function endsWith(FullString: String; EndString:String):Boolean;

//--- Plain with no cutoff or regard to where the -ve sign goes
function StrToIntWithInvalid(AStr: AnsiString; InvalidValue: Integer=-1): Integer;
function StrToFloatWithInvalid(AStr: AnsiString; InvalidValue: double=-1): Double;

function IntToStrWidth(Value, Width: Longint):AnsiString;
function FloatToStrWidth(Value: Double; Width, digits: Longint):AnsiString;
function FloatToPrecision(Value: Double; digits: Longint):Double;
function StrToStrPrecision(Value: AnsiString; digits: Longint): AnsiString;  // Truncates a string of a floating point number to a specific precision.
function FloatToStrPrecision(Value : Double; digits: Longint = 10): AnsiString;
//--- Formatting strings where a negative value could be obtained
function NegIntToStrWidth(Value, Width: Longint):AnsiString;
function NegFloatToStrWidth(Value: Double; Width, digits: Longint):AnsiString;
//--- Formatting strings with writing '-' or 'n/c'
function WriteIntToStrWidth(Value, Width, Cutoff: Longint):AnsiString;
function WriteFloatToStrWidth(Value: Double; Width, digits: Longint; Cutoff: Double):AnsiString;

//--- System Path
{$IFNDEF FPC}
function GetSystemPath(Folder: Integer): String;  // You pass it CSIDL_APPDATA and it returns the %appdata% path
function UnixToDateTime(Value: Longword; InUTC: Boolean): TDateTime;
function FindVolumeSerial(const Drive : PAnsiChar) : AnsiString;
function GetSystemDrive: AnsiString;
function IsFileInUse(fName: String): Boolean;
function KillTask(ExeFileName: String): integer;
{$ENDIF}

function BuildNumberFormat(Precision: Integer): AnsiString;
function GetIEVersion: Integer;
function GetCellColor(x: AnsiChar): TColor;

procedure CopyFileFromRES(FromFileName, ToFileName : String; CopyIfNeeded: Boolean);
{$IFDEF FPC}
procedure CopyFileFromResFPC(Source, Target: String; CopyIfNeeded: Boolean);
function ExtractFileBaseName(Filename: String): String;
function ResourceType(FromFileName: String): String; { for fpc, we need to specify the resource type}
function FpcResourceType(AType: String): TResourceType;
{$ENDIF}
// --- System info
function GetNoOfProcessors: integer; overload;
function GetDefaultNoOfProcessors(curOperation: TDistTreeDlgOption): Integer; overload;
function GetOperatingSystem: String;
function GetTechnicalInfoJson: TJSONObject;

//--- Validating a tree file
function IsPerfectTreeFile(AFileName: String): Boolean; //Validates file format
function IsPerfectTreeFileWithInternalLabels(FileName: String): Boolean;
function RegexReplaceAStringList(const theList: TStringList; const RegExpr: AnsiString; const ReplaceStr: AnsiString): TStringList;

function FloatToStrNP( val : extended) : AnsiString;  //Float to string 5 decimal precision avoids exponential notation that FloatToStr may use and removes trailing 0s
function StripGaps(Str: AnsiString): AnsiString;
function StripSpaces(Str: AnsiString): AnsiString;
function NewickCompatibleName(Name: AnsiString): AnsiString;
procedure AddMaoFileHeader(var AList: TStringList; const FLStr: String);

procedure CalcCRC32 (p: pointer; ByteCount: DWORD; var CRCValue: DWORD);
{$IFDEF VISUAL_BUILD}
function MessageDlgCheck(aParent: TCustomForm; const Msg, ACaption, ACheckCaption: AnsiString; var AChecked : boolean; DlgType: TMsgDlgType = mtInformation; Buttons: TMsgDlgButtons = [mbOK]; HelpCtx: Longint = 0): Integer;
{$ENDIF}

function SubSetOptionsToStringList(options: TDataSubsetOptions): TStringList;
function SubSetOptionsToString(options: TDataSubsetOptions): String;
function validEmail(email: AnsiString): boolean;

function CheckStopCodons(SeqList: TSequenceList; var Row: Integer; var Col: Integer): Boolean;
function MapIntegerToBaseString(Num: Integer; IsProteinData: Boolean): AnsiString;
function IntMapToNuc(Num: Integer): AnsiString;
function IntMapToAmino(Num: Integer): AnsiString;

procedure LogCurrentProcessMemory(myMessage: AnsiString = '');
procedure WriteProcessMemoryLog(FileName: String);
procedure WriteDebugStringToFile (MyString: String; MyFileName: String);
procedure SequenceListToFile(SequenceList: TSequenceList; MyFileName: String);
function AddTaxaNameToNewickString(TaxaName, NewickString: AnsiString): AnsiString;
(*procedure ParseAnalysisPreferencesFile(pp: TProcessPack; FileName: AnsiString); *)
function ConcatenateDivTimeArrays(const minTimes: TDivTimesArray; const maxTimes: TDivTimesArray): TDivTimesArray;
{$IFDEF DEBUG}
function DivTimeArraysToDebugFile(minTimes, maxTimes: TDivTimesArray; filename: String): Boolean;
{$ENDIF}
procedure FindMinAndMaxTimes(const allTimes: TDivTImesArray; var minTime: Extended; var MaxTime: Extended);
function ArrayOfIntToList(data: ArrayOfInteger): TStringList;
function ArrayOfIntToFile(data: ArrayOfInteger; filename: String): Boolean;
function ArrayOfDoubleToFile(data: ArrayOfDouble; filename: String): Boolean;
function PArrayOfIntToList(data: PArrayOfInt; n: LongInt): TStringList;
function PArrayOfIntToFile(data: PArrayOfInt; filename: String; n: LongInt): Boolean;
function TwoDArrayOfExtendedToFile(Data: T2dArrayOfExtended; Filename: String; Caption: String): Boolean;
function TwoDArrayOfExtendedToList(Data: T2DArrayOfExtended; Caption: String): TStringList;
function OneDArrayOfExtendedToFile(Data: ArrayOfExtended; Filename: String; Caption: String): Boolean;
function TwoDArrayOfIntToFile(Data: T2DArrayOfInteger; Filename: String; Caption: String): Boolean;
function TwoDArrayOfIntToList(Data: T2DArrayOfInteger; Caption: String): TStringList;
procedure DumpStateToFile(MyStringList: TStringList; FileName: String; Overwrite: Boolean; Comment: AnsiString = '');
function MakePrettyString(MyString: AnsiString; DesiredLength: Integer): AnsiString;
function MakePrettyTaxonNameString(OriginalString: String; DesiredLength: Integer): String;
function MapDataTypeStringToTokenCode(DataTypeString: String):TSnTokenCode;
function MapTokenCodeToDataTypeString(TokenCode: TSnTokenCode): String;
function MapTSnTokenCodeStringToTokenCode(TokenCodeString: String):TSnTokenCode;
function FileToString(FileName: string; ProcessMessages: boolean = false): string;

function downloadURL(urlToDownload: string; saveFilename: String): string;
{procedure directoriesToList(directories: String; var fileList: TStringList; recursive: Boolean);}

procedure FindAllFilesNotFolders(Path: String; Attr: Integer; List: TStringList; recursive: Boolean);
function fileExtValidForAnalysis(ext: String): boolean;
function fileExtValidForTree(ext: String): boolean;
function HasFastaFileExtension(filename: String): Boolean;
function IsBootstrapTreeSearchClass(ClassName: String): Boolean;
function DumpExceptionCallStack(E: Exception): TStringList;
function ExceptionCallStack(E: Exception): String;
function CommandLineStr: String;

function RectWidth(aRect: TRect): Integer;
function RectHeight(aRect: TRect): Integer;

function TransposeMatrixOfExtended(input: T2dArrayOfExtended): T2dArrayOfExtended;
function HasIndels(const aSeq: String; const indelCharacter: Char): Boolean;
procedure UpdateCoverages(const siteConfiguration: String; const indelChar: Char; var coverages: TIntArray);
function NumSiteConfigurations(seqStrings: TStringList; const indelCharacter: Char; var coverages: TIntArray): Integer;
function NumSiteConfigurationsFewSeqs(seqStrings: TStringList; const indelCharacter: Char; var coverages: TIntArray): Integer;
function GetAlignmentOfUniqueSiteConfigurations(sourceData: TStringList): TStringList;
function GetAlignmentOfUniqueSiteConfigurationsFewSeqs(sourceData: TStringList): TStringList;
function GetAlignmentOfUniqueSiteConfigurationsUsingStrHashMap(sourceData: TStringList): TStringList;
function GetAlignmentOfUniqueSiteConfigurationsSlow(sourceData: TStringList): TStringList;
function DebugUniqueStringsInverted(sourceData: TStringList): TStringList;
function DebugUniqueConfigsSorted(sourceData: TStringList): TStringList;
function GetSitePatternFrequencies(seqData: TStringList): TArrayOfInt64;
function ExpandBootAlignmentToFullSeqData(aTable: PArrayOfInt; bootTableLength: Integer; seqLength: Integer; seqs: TStringList): TStringList;
function ExpandBootAlignmentToFasta(aTable: PArrayOfInt; bootTableLength: Integer; seqLength: Integer; seqs: TStringList; otuNames: TStringList; filename: String): Boolean;
function CalculateTargetSitePatternCountFactor(const aNumSitePatterns: Integer; const isAmino: Boolean; var msg: String): Double;
{$IFDEF DEBUG}
function AlignmentToFasta(seqs: TStringList; otuNames: TStringList; filename: String): Boolean;

{$ENDIF}
{$IFDEF VISUAL_BUILD}
  {$IFDEF MSWINDOWS}
function GetExcelVersionStr(var Version: AnsiString): Boolean;
  {$ENDIF}
function IsExcel2010OrGreater: Boolean;
{$ENDIF}
{$IFDEF DEBUG}
function WriteToDevLogFile(aMsg: String): Boolean;
procedure WriteToDevLog(aMsg: String);
{$ENDIF}
function DensityDistributToCalibrationCategory(aDensityDist: TCalibrationDensityDistribution): TCalibrationCategory;
function DebugNucSiteCompositionToFile(seqs: TStringList; filename: String): Boolean;
function DebugRemoveLowCoverageSitesAndWriteToFasta(seqs: TStringList; names: TStringList; coverage: TIntArray; cutoff: Integer; filename: String): Boolean;
function DebugArrayOfExtToFile(data: TArrayOfExt; filename: String): Boolean;
function order(r:double):integer;

var
  ProcessMemoryLog: TStringList;
  SuppressStopCodonsMessage: Boolean;

implementation

uses
  {$IFDEF DARWIN}
  macos_files, SysCtl,
  {$ENDIF}
  {$IFNDEF VISUAL_BUILD}
  MegaUtils_NV,
  {$ELSE}
  mcustom_msg_dlg, mega_main,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix, Unix, process,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  windirs, Windows,
  {$ENDIF}
  LResources, fphttpclient, Math, mbrowserutils, MegaPrivateFiles, spe, mstringbuilder,
  MegaAnalysisPrefStrings, ErrorMessages_HC, MegaErrUtils, RegExpr, StrUtils, ghashmap,
  MGlobalSettings, TypInfo, StringUtils, MD_InputSeqData, MLongintList, StrHashMap,
  contnrs, dateutils, base64, LazFileUtils{$IFDEF DEBUG}, syncobjs{$ENDIF},
  mmemutils, mextendedlist;

function DebugArrayOfExtToFile(data: TArrayOfExt; filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
begin
  try
    AssignFile(aFile, filename);
    Rewrite(aFile);
    if Length(data) > 0 then
    begin
      for i := Low(data) to High(data) do
        Write(aFile, Format('%12d', [i + 1]));
      WriteLn(aFile);
      for i := Low(data) to High(data) do
        Write(aFile, Format('%12.8f', [data[i]]));
    end;
    WriteLn(aFile);
    Result := FileExists(filename);
  finally
    CloseFile(aFile);
  end;
end;

function order(r: double): integer;
begin
  Result := Floor(log10(r));
  if r < 1.0 then
    Dec(Result)
  else
    Inc(Result);
end;

function HasFastaFileExtension(filename: String): Boolean;
var
  ext: String;
begin
  ext := ExtractFileExt(filename);
  Result := (Pos(ext, FastaExts) > 0);
end;

function IsBootstrapTreeSearchClass(ClassName: String): Boolean;
begin
  Result := (SameText(ClassName, 'TBootstrapMLThread') or
             SameText(ClassName, 'TBootstrapUPGMAThread') or
             SameText(ClassName, 'TBootstrapNJThread') or
             SameText(ClassName, 'TBootstrapMEThread') or
             SameText(ClassName, 'TBootstrapBranchBoundSearchThread') or
             SameText(ClassName, 'TBootstrapMiniMini_CNISearchThread') or
             SameText(ClassName, 'TBootstrapRBA_CNISearchThread') or
             SameText(ClassName, 'TBootstrapMPTreeSearchThread'));
end;

function DumpExceptionCallStack(E: Exception): TStringList;
var
  I: Integer;
  Frames: PPointer;
  Report: string = '';
begin
  Result := TStringList.Create;
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Result.Text := Report;
  Result.Add(Format('command line: %s', [CommandLineStr]));
end;

function ExceptionCallStack(E: Exception): String;
var
  aList: TStringList = nil;
begin
  try
    aList := DumpExceptionCallStack(E);
    Result := aList.Text;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function CommandLineStr: String;
var
  i: Integer;
begin
  Result := EmptyStr;
 for i := 0 to ParamCount do
   Result := Result + ParamStr(i) + ' ';
 Result := Trim(Result);
end;

function AddTaxaNameToNewickString(TaxaName, NewickString: AnsiString): AnsiString;
var
  MyString: AnsiString;
  SemiColonPosition: Integer;
begin
  MyString := Trim(NewickString);
  SemiColonPosition := Pos(';', MyString);
  if SemiColonPosition < 1 then
  {$IFDEF VISUAL_BUILD}
    MessageDlg('Invalid Newick tree; no semi-colon found.', mtWarning, [mbOK], 0);
  {$ELSE}
    error_nv('Invalid Newick tree; no semi-colon found.');
  {$ENDIF}
  MyString := Copy(NewickString, 1, SemiColonPosition - 1);
  MyString := '(' + MyString + ',' + TaxaName + ');';
  Result := MyString;
end;

procedure SequenceListToFile(SequenceList: TSequenceList; MyFileName: String);
var
  MySequence: AnsiString;
  MyName: AnsiString;
  NumSequences: Integer;
  i: Integer;
  FileToWrite: TextFile;
begin
  try
    AssignFile(FileToWrite, MyFileName);
    if FileExists(MyFileName) then
      Append(FileToWrite)
    else
      Rewrite(FileToWrite);
    NumSequences := SequenceList.Count;
    for i := 0 to NumSequences - 1 do
    begin
      MyName := '#' + PAnsiChar(SequenceList[i].FSeqName);
      MySequence := MyName + ' ' + PAnsiChar(SequenceList[i].FSeqData);
      WriteLn(FileToWrite, MySequence);
    end;
    CloseFile(FileToWrite);
  except
    on E: Exception do
      ShowMessage(E.ClassName + ' raised error: ' + E.Message);
  end;
end;

function MapIntegerToBaseString(Num: Integer; IsProteinData: Boolean): AnsiString;
begin
  if IsProteinData then
    Result := IntMapToAmino(Num)
  else
    Result := IntMapToNuc(Num);
end;

function IntMapToNuc(Num: Integer): AnsiString;
begin
  case Num of
    0: result := 'A';
    1: result := 'T';
    2: result := 'C';
    3: result := 'G';
    else result := 'Error in IntMapToNuc';
  end;
end;

function TwoDArrayOfIntToFile(Data: T2DArrayOfInteger; Filename: String; Caption: String): Boolean;
var
  AList: TStringList = nil;
begin
  Result := False;
  if Length(Data) = 0 then
    Exit;

  try
    try
      AList := TwoDArrayOfIntToList(Data, Caption);
      AList.SaveToFile(Filename);
      Result := FileExists(Filename);
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Failed to export 2D array of double: ' + E.Message);
      {$ELSE}
      warn_nv('Failed to export 2D array of double: ' + E.Message);
      {$ENDIF}
    end;
  finally
    if Assigned(AList) then
      AList.Free;
  end;
end;

function TwoDArrayOfIntToList(Data: T2DArrayOfInteger; Caption: String): TStringList;
var
  aRow, aCol: Integer;
  AStr: String;
begin
  Result := TStringList.Create;
  if Length(Data) = 0 then
    Exit;

  try
    Result.Add(Caption);
    for aRow := 0 to Length(Data) - 1 do
    begin
      AStr := Format('%4d', [aRow]) + #9;
      for aCol := 0 to Length(Data[aRow]) - 1 do
      begin
        AStr := AStr + Format('%4d', [Data[aRow][aCol]]) + #9;
      end;
      Result.Add(Trim(AStr));
    end;
  except
    on E:Exception do
    {$IFDEF VISUAL_BUILD}
    ShowMessage('Failed to export 2D array of double: ' + E.Message);
    {$ELSE}
    warn_nv('Failed to export 2D array of double: ' + E.Message);
    {$ENDIF}
  end;
end;

procedure DumpStateToFile(MyStringList: TStringList; FileName: String; Overwrite: Boolean; Comment: AnsiString = '');
var
  MyFile: TextFile;
  i: Integer;
  PrettyString: AnsiString;
  MaxKeyStringLength: Integer;
begin
  Assign(MyFile, FileName);

  if not FileExists(FileName) then
    Rewrite(MyFile)
  else
  begin
    if Overwrite = True then
      Rewrite(MyFile)
    else
      Append(MyFile);
  end;

  MaxKeyStringLength := 0;
  for i:= 0 to MyStringList.Count - 1 do
  begin
    if Length(MyStringList.Names[i]) > MaxKeyStringLength then
      MaxKeyStringLength := Length(MyStringList.Names[i]);
  end;
  inc(MaxKeyStringLength, 3);

  if Trim(Comment) <> EmptyStr then
    WriteLn(MyFile, Comment);
  for i := 0 to MyStringList.Count - 1 do
    begin
   PrettyString := MakePrettyString(MyStringList.Names[i], MaxKeyStringLength);
   PrettyString := PrettyString + MyStringList.Values[MyStringList.Names[i]];
   WriteLn(MyFile, PrettyString);
  end;
  close(MyFile);
end;

/// <summary>Transform OriginalString into a string of DesiredLength. If OriginalString
/// has length > DesiredLength, then the center part of OriginalString will be
/// removed and replaced by '...'. If OriginalString has length < DesiredLength,
/// the result will be OriginalString padded with spaces. If the OriginalString
/// hase length equal to DesiredLength, it will be returned as is.</summary>
/// <param name="OriginalString">The target string which may be modified</param>
/// <param name="DesiredLength">The number of characters that will comprise the
/// returned result</param>
/// <returns>A string whose length equals DesiredLength, constructed from OriginalString.</returns>
/// <author>Glen</author>
function MakePrettyTaxonNameString(OriginalString: String; DesiredLength: Integer): String;
var
  PrettyString: String;
  CopyTo: Integer;
  CopyFrom: Integer;
  NumCharsToCopy: Integer;
begin
  if Length(OriginalString) < DesiredLength then
  begin
    PrettyString := OriginalString;
    while Length(PrettyString) < DesiredLength do
    begin
      PrettyString := PrettyString + ' ';
    end;
  end
  else if Length(OriginalString) > DesiredLength then
  begin
    CopyTo := (DesiredLength div 2) - 2;
    PrettyString := Copy(OriginalString, 1, CopyTo);
    PrettyString := PrettyString + '[...]';
    NumCharsToCopy := DesiredLength - Length(PrettyString);
    CopyFrom := Length(OriginalString) - NumCharsToCopy + 1;
    PrettyString := PrettyString + Copy(OriginalString, CopyFrom, NumCharsToCopy);
  end
  else
    PrettyString := OriginalString;
  Result := PrettyString;
end;

/// <summary>Transform MyString into a string whose length is equal to DesiredLength
/// if its length is already shorter. Otherwise, just return MyString (this function
/// will never truncate MyString</summary>
/// <param name="MyString">The source string</param>
/// <param name="DesiredLength">The minimum length of the returned result</param>
/// <returns>A string whose length is >= DesiredLength, derived from MyString and
/// padded with spaces if needed.
function MakePrettyString(MyString: AnsiString; DesiredLength: Integer): AnsiString;
var
  PrettyString: AnsiString;
const
  MaxLength = 40;
begin
  PrettyString := MyString;
  while (Length(PrettyString) < DesiredLength) and (length(PrettyString) < MaxLength) do
    PrettyString := PrettyString + ' ';

  Result := PrettyString;
end;

procedure WriteDebugStringToFile (MyString: String; MyFileName: String);
var
  FileToWrite: TextFile;
begin
  try
    AssignFile(FileToWrite, MyFileName);
    if FileExists(MyFileName) then
      Append(FileToWrite)
    else
      Rewrite(FileToWrite);
    WriteLn(FileToWrite, MyString);
    CloseFile(FileToWrite);
  except
    on E: Exception do
    begin
     {$IFDEF VISUAL_BUILD}
     ShowMessage(E.className + ' raised error: ' + E.Message);
     {$ELSE}
     warn_NV('failed to save debug string to file: ' + E.Message);
     {$ENDIF}
    end;
  end;
end;

/// <summary>Estimate the current memory usage of MEGA and record the value
/// to the ProcessMemoryLog object along with the given message if any.</summary>
procedure LogCurrentProcessMemory(myMessage: AnsiString = '');
var
  CurrentProcessMemory: Double;
begin
  if ProcessMemoryLog = nil then
    ProcessMemoryLog := TStringList.Create;
  CurrentProcessMemory := GetCurrentProcessMemoryMB;
  ProcessMemoryLog.Add(myMessage + ' [' + FloatToStrF(CurrentProcessMemory, ffFixed, 2, 2) + ' ]');
end;

/// <summary>Write the process memory log data to the indicated file.</summary>
procedure WriteProcessMemoryLog(FileName: String);
begin
    ProcessMemoryLog.SaveToFile(FileName);
end;

function IntMapToAmino(Num: Integer): AnsiString;
begin
  case Num of
    0: Result := 'A';
    1: Result := 'R';
    2: Result := 'N';
    3: Result := 'D';
    4: Result := 'C';
    5: Result := 'Q';
    6: Result := 'E';
    7: Result := 'G';
    8: Result := 'H';
    9: Result := 'I';
    10: Result := 'L';
    11: Result := 'K';
    12: Result := 'M';
    13: Result := 'F';
    14: Result := 'P';
    15: Result := 'S';
    16: Result := 'T';
    17: Result := 'W';
    18: Result := 'Y';
    19: Result := 'V';
    else
      raise Exception.Create('invalid base');
  end;
end;

procedure Resample(var FFreqTable: ArrayOfInteger; TotalSites: Longint);
var
  i, rn: Longint;
begin
  for i:=0 to TotalSites-1 do
    FFreqTable[i] := 0;
  for i:=0 to TotalSites-1 do
  begin
    rn := Random(TotalSites);
    //rn := uran(TotalSites);
    //if rn >= TotalSites then
    //  RaiseErrorMessage(HC_Unexpected_Error, 'Pseudo-random number generator (URan) returned incorrect values.');
    Inc(FFreqTable[rn]);
  end;
end;

function RandomSequenceOfUniqueIntegers(numIntegers: LongInt): ArrayOfInteger;
var
  i: Integer;
  j: Integer;
  tmp: Integer;
begin
  SetLength(Result, numIntegers);
  for i := 0 to numIntegers - 1 do
    Result[i] := i;
  for i := 0 to numIntegers - 2 do
  begin
    j := RandomRange(i, numIntegers);
    tmp := Result[i];
    Result[i] := Result[j];
    Result[j] := tmp;
  end;
end;

// convert
function ConvertToMegaWinCaption(Value: AnsiString): AnsiString;
begin
  Result := VER_MEGA_WIN_CAPTION_PREFIX +': '+Trim(Value);
end;

function NewickFilesFilter: String;
begin
  Result := 'Newick files|*.nwk;*.newick;*.tre;*.dnd;*.ph;*.pub;*.phy|All Files|*.*';
end;

function NewickAndTreeSessionFilesFilter: String;
begin
  Result := TreeSessionFilesFilter + '|' + NewickFilesFilter;
end;

function TreeSessionFilesFilter: String;
begin
  Result := 'MEGA Tree Sessions|*.mtsx;*.mts';
end;

// file filters
function MegaFilesFilter: String;
begin
  Result := 'MEGA file|*.meg; *.mdsx';
end;

function MegaSessionFilesFilter: String;
begin
  Result := 'MEGA Session file|*.mdsx; *.meg';
end;

function FastaFilesFilter: String;
begin
  Result := 'Fasta file|*.fasta; *.fas';
end;

function TextFilesFilter: String;
begin
  Result := 'Text file|*.txt';
end;

function AllFilesFilter: String;
begin
  Result := 'All files|*.*';
end;

function GetPrivateDir: String;
begin
  Result := ExtractFilePath(Application.ExeName)+'private' + PathDelim;
end;

function GetPrivateDir(AValue: String; CopyIfNeeded: Boolean): String;
var
  OpenDir : TStringList;
  i : integer;
  InIde: Boolean;
begin
  Result := EmptyStr;
  {$IFNDEF FPC}
  InIde := (DebugHook = 0); // Debug hook is not 0 when we are running in an ide.
  {$ELSE}
  InIde := False;
  {$ENDIF}

  InIde := False;
  if not InIde then
  begin
    if AValue[length(AValue)] <> PathDelim then
      AValue := AValue + PathDelim;
    OpenDir := TStringList.Create;
    OpenDir.LoadFromFile(GetPrivateFile(AValue+'ResMakerFileList'));
    for i := 0 to OpenDir.Count-1 do
    begin
      if OpenDir.Strings[i][length(OpenDir.Strings[i])] = PathDelim then
        GetPrivateDir(OpenDir.Strings[i], CopyIfNeeded)
      else
        GetPrivateFile(OpenDir.Strings[i]);
    end;
  end
  else
    Result := ExtractFilePath(Application.ExeName) + 'private' + PathDelim + AValue;
end;

function GetAPPDATA: String;
begin
  {$IFDEF DARWIN}
  Result := GetPrivateFile('Private' + PathDelim + 'temp', False) + PathDelim;
  {$ELSE}
  Result := GetAppConfigDir(False);
  {$ENDIF}
end;

function GetNnlsExe: String;
begin
 {$IFDEF VISUAL_BUILD}
   {$IFDEF DEBUG}
   Result := ExtractFilePath(Application.ExeName) + 'MEGA7_Install' + PathDelim + 'bin' + PathDelim + CF_REGRESSION_EXE;
   {$ELSE}
     {$IFDEF DARWIN}
     Result := GetPrivateExecutableFile('Private' + PathDelim + 'CloneFinder' + PathDelim +  CF_REGRESSION_EXE, False);
     {$ELSE}
     Result := ExtractFilePath(Application.ExeName) + CF_REGRESSION_EXE;
     {$ENDIF}
   {$ENDIF}
 {$ELSE}
   {$IFDEF DEBUG}
   Result := ParamStr(0) + PathDelim + 'MEGA7_Install' + PathDelim + 'bin' + PathDelim + CF_REGRESSION_EXE;
   {$ELSE}
   Result := ParamStr(0) + PathDelim + CF_REGRESSION_EXE;
   {$ENDIF}
 {$ENDIF}
end;

function GetTEMP: String;
begin
  Result := GetTempDir;
end;

function GetMegaGlobalFile(FileName: String; CopyIfNeeded: Boolean = True): String;
const
  MEGA_GLOBAL_DIR = {$IFNDEF DARWIN}PathDelim +{$ENDIF} 'MEGA_Global' + PathDelim;
var
  GlobalDir : String;
begin

  Result := EmptyStr;
  try
  {$IFDEF UNIX}
    {$IFDEF DARWIN} // macOS
    GlobalDir := GetAppSupportDir + MEGA_GLOBAL_DIR + FileName;
    {$ELSE} // Linux
    GlobalDir := ChompPathDelim(GetAppConfigDir(False));
    GlobalDir := ChompPathDelim(ExtractFilePath(GlobalDir));
    GlobalDir := GlobalDir + MEGA_GLOBAL_DIR + FileName;
    {$ENDIF}
  {$ELSE} // Windows
  GlobalDir := SysUtils.GetEnvironmentVariable('LOCALAPPDATA') + MEGA_GLOBAL_DIR + FileName;
  {$ENDIF}


    if AnsiCompareText(ExtractFileName(FileName), '') = 0 then // If no file is specified the just return the path where the file would be stored.
    begin
      result := GlobalDir;
      if not DirectoryExists(GlobalDir) then
        if not ForceDirectories(GlobalDir) then
        begin
          GlobalDir := ExtractFileDir(ExtractFilePath(Application.ExeName) + 'temp' + PathDelim);
          ForceDirectories(GlobalDir);
          Result := GlobalDir;
        end;
      Exit;
    end;

    if not DirectoryExists(GlobalDir) then  //If the folder in AppData is not there
    begin
      // Need to handle inability to create dir
      if not ForceDirectories(GlobalDir) then  // For some reason we couldn't create the Folder structure in APPDATA, so go to a backup plan.
      begin
        GlobalDir := ExtractFileDir(ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + FileName); // On failure of using the %APPDATA%/MEGA_BUILD to store, use ExeLocation/temp to store the files, NOTE: This is NOT user indepenent, meaning all users share the same settings if this happens
        if ForceDirectories(GlobalDir)  then // Create needed directories for ExeLocation/MEGA/Temp
        begin
          if not FileExists(ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + FileName)  then // If the file doesn't exist copy the file from (in most cases 'private/Ini') to /temp as a template
            CopyFileFromRES(FileName, ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + FileName, CopyIfNeeded);
          Result := ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + FileName;
          exit;
        end;
      end;
    end;

    if not FileExists(ExtractFileDir(GetAPPDATA + MEGA_GLOBAL_DIR + FileName)) then
      CopyFileFromRES(FileName, GetAPPDATA + MEGA_GLOBAL_DIR + FileName, CopyIfNeeded);
    Result := GlobalDir + ExtractFileName(FileName);
  Except
    on E:Exception do
    begin
      Result := EmptyStr;
    end;
  end;
end;

function GetPrivateOutputFile(AValue: String): String;
begin
  {$IFDEF DARWIN}
  Result := GetFileFromAppSupport(AValue);
  {$ELSE}
  Result := GetPrivateFile(AValue, False);
  {$ENDIF}
end;

function GetPrivateFile(AValue: String; CopyIfNeeded: Boolean): String;
  function IsIni(Path : String) : Boolean;
  begin
    Result := (Pos('Ini', Path) > 0);
  end;

  function IsWebOptionsDialog(aPath: String): Boolean;
  var
    aDir: String;
  begin
    Result := False;
    Exit;
    aDir := ExtractFileDir(aPath);
    if (aDir = wofWebOptionsDialogsDir) or (aDir = ExtractFileDir(wofWebOptionsDialogsArchive)) then
      Result := True;
  end;

var
  UserDir : String;
  FullPath: String;
  InIde: Boolean = False;
begin
  {$IFDEF DARWIN}
  Result := GetPrivateFileMacOS(AValue, CopyIfNeeded);
  Exit;
  {$ENDIF}

   {$IFDEF DEBUG}
   InIde := True;
     //if (AValue <> mfWebHelpArchive) and
     //   (AValue <> mfWebHelpIndex) and
     //   (AValue <> mfWebHelpDir) then
     //   (not IsWebOptionsDialog(AValue)) then
     //  InIde := True;
     //{$IFDEF VISUAL_BUILD}
     //  {$IFDEF UNIX}
     //  InIde := False;
     //  {$ELSE}
     //  InIde := False; { set InIde to True whenever you are working on captions so that MEGA will pull them from the local Private directory instead of from compiled resources - which you would have to continuously compile}
     //  {$ENDIF}
     //{$ELSE}
     //InIde := False;
     //{$ENDIF}
   {$ENDIF}
  if InIde then
  begin
    {$IFDEF VISUAL_BUILD}
    Result := ExtractFilePath(ExtractFileDir(Application.ExeName)) + AValue;
    {$ELSE}
    Result := ExtractFilePath(Application.ExeName) + AValue;
    {$ENDIF}
    Exit; // When running in the IDE we don't copy anything, it is completely seperate from the user version.
  end;
  UserDir := ExtractFileDir(GetAPPDATA + MEGA_USER_DIR + AValue) + PathDelim;
  FullPath := UserDir + ExtractFileName(AValue);

  if AnsiCompareText(ExtractFileName(AValue), '') = 0 then // If no file is specified the just return the path where the file would be stored.
  begin
    result := userDir;
    Exit;
  end;

  if not DirectoryExists(userDir) then  //If the folder in AppData is not there
  begin
    // Need to handle inability to create dir
    if not ForceDirectories(userDir) then  // For some reason we couldn't create the Folder structure in APPDATA, so go to a backup plan.
    begin
      userDir := ExtractFileDir(ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + AValue); // On failure of using the %APPDATA%/MEGA_BUILD to store, use ExeLocation/temp to store the files, NOTE: This is NOT user indepenent, meaning all users share the same settings if this happens
      if ForceDirectories(userDir) then // Create needed directories for ExeLocation/MEGA/Temp
      begin
        if not FileExists(ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + AValue)  then // If the file doesn't exist copy the file from (in most cases 'private/Ini') to /temp as a template
          CopyFileFromRES(String(AValue), ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + AValue, CopyIfNeeded);
        Result := ExtractFilePath(Application.ExeName) + 'temp' + PathDelim + AValue;
        exit;
      end
      else
        Raise Exception.Create('Unable to extract necessary file ' + AValue + ' from RES file, creation of %appdata% folders, and temp folders failed');
    end;
  end;

  // need to handle a non-zero returned
  if not FileExists(FullPath) then
    CopyFileFromRES(AValue, FullPath, CopyIfNeeded);
  Result := FullPath;
end;

procedure CopyFileFromRES(FromFileName, ToFileName: String; CopyIfNeeded: Boolean);
begin
  if not CopyIfNeeded then
    Exit;
  {$IFDEF DARWIN}
  CopyFile(GetResourcesPath + PathDelim + FromFileName, ToFileName, false);
  {$ELSE}
  CopyFileFromResFpc(FromFileName, ToFileName, CopyIfNeeded);
  {$ENDIF}
end;


function ResourceType(FromFileName: String): String;
begin
  Result := UpperCase(ExtractFileExt(FromFilename));
  while Pos('.', Result) > 0 do
    Result := Copy(Result, Pos('.', Result) + 1, Length(Result));
end;

function FpcResourceType(AType: String): TResourceType;
begin
  if SameText(AType, 'exe') or (AType = EmptyStr) then
    Result := {$IFDEF MSWINDOWS}Windows.{$ENDIF}RT_RCDATA
  else if SameText(AType, 'ico') then
    Result := RT_ICON
  else
    Result := RT_STRING;
  //RT_CURSOR = TResourceType(1);
  //RT_BITMAP = TResourceType(2);
  //RT_ICON = TResourceType(3);
  //RT_MENU = TResourceType(4);
  //RT_DIALOG = TResourceType(5);
  //RT_STRING = TResourceType(6);
  //RT_FONTDIR = TResourceType(7);
  //RT_FONT = TResourceType(8);
  //RT_ACCELERATOR = TResourceType(9);
  //RT_RCDATA = TResourceType(10);
  //RT_MESSAGETABLE = TResourceType(11);
  //RT_GROUP_CURSOR = TResourceType(12);
  //RT_GROUP_ICON = TResourceType(14);
  //RT_VERSION = TResourceType(16);
end;

procedure CopyFileFromResFPC(Source, Target: String; CopyIfNeeded: Boolean);
var
  RStream: TLazarusResourceStream;
  RName: String;
  RType: String;
begin
  if not CopyIfNeeded then
    Exit;
  RName := ExtractFileBaseName(Source);
  RType := ResourceType(Source);

  try
   RStream := TLazarusResourceStream.Create(RName, PChar(RType));
   if not Assigned(RStream) then
     raise Exception.Create('A needed application resource is missing (' + Source + ')');
    RStream.SaveToFile(Target);
  finally
    if Assigned(RStream) then
      RStream.Free;
  end;
end;

function ExtractFileBaseName(Filename: String): String;
begin
  Result := ExtractFileName(FileName);
  while (Pos('.', Result) > 0) do
    Result := Copy(Result, 1, Pos('.', Result) - 1);
end;

 procedure splitStr( strIn, sep: AnsiString; stl: TStrings; IgnoreEmpty: boolean = false);  //Split by the ',' to get each element this procedure correctly handles spaces unlike the stringlist's delimited text function
  var
    intPos: integer;
    strTmp: AnsiString;
  begin
    stl.clear;
    intPos := pos(sep,strIn);
    while intPos <> 0 do begin
      strTmp := copy(strIn, 1, intPos-1);
      System.Delete(strIn,1,intPos+Length(sep)-1);
      if (trim(strTmp) <> '') or (not ignoreempty) then stl.Add(strTmp);
      intPos := pos(sep, strIn);
    end;
    if (trim(strIn) <> '') or (not Ignoreempty) then stl.Add(strIn);
  end;

// Function to get index of a string in a string list
function StrListIndexOf(AList: TStringList; AStr: AnsiString): LongInt;
var
  i: Integer;
begin
  Result := -1;
  if AList = nil then Exit;
  for i:=0 to AList.Count-1 do
  begin
    if CompareStr(AList[i], AStr) = 0 then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function StrListCountDistinctStr(AList: TStringList): LongInt;
var
  i,j: Integer;
  IsStrFound: Boolean;
begin
  Result := 0;
  if AList = nil then Exit;
  for i:=0 to AList.Count-1 do
  begin
    if Length(AList[i]) = 0 then
      Continue; // empty str is not worth it
    IsStrFound := False;
    for j:=i+1 to AList.Count-1 do
      if CompareStr(AList[i], AList[j]) = 0 then // same string found then break
      begin
        IsStrFound := True;
        break;
      end;
    if not IsStrFound then
      Inc(Result);
  end;
end;

procedure GenerateStrListMapIds(MyGpNames: TStringList; MyGpIds: PArrayOfInt);
var
  NextId, i,j: Integer;
begin
  if MyGpIds = nil then
    Exit;

  for i:=0 to MyGpNames.Count-1 do
    MyGpIds[i] := -1;
  //
  NextId := 0;
  for i:=0 to MyGpNames.Count-1 do
  begin
    if Length(MyGpNames[i]) = 0 then
      Exit;
    if MyGpIds[i] >= 0 then  // we already know the id
      continue;

    MyGpIds[i] := NextId;
    // now mark all the same one's below with nextId
    for j:=i+1 to MyGpNames.Count-1 do
      if CompareStr(MyGpNames[i], MyGpNames[j]) = 0 then // same string found then break
        MyGpIds[j] := NextId;
    Inc(NextId);
  end;
end;

procedure MegaSafeTrim(var S: AnsiString);  // it removes any ! or # from name
var
  I, K, L: Longint;
begin
  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  if S[1] = ':' then S[1] := ' ';  // don't allow the first char to be colon
  for I:= 1 to Length(S) do
    case S[I] of
      '#', '!':  S[I] := ' ';
    end;

  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  // now L <> 0 then
  I := 1;
  while (I < L) do
  begin
    if ((S[I] <= ' ') and (S[I+1] <= ' ')) then
    begin
      for K := I+1 to L-1 do  // copies rest as it is
        S[K] := S[K+1];
      S[L] := #0;  // this helps us to trim right later
      Dec(L);
    end
    else
      Inc(I);
  end;
  S := TrimRight(S);
end;


function ChangeInitialDirectorySaveDialogForMac(InitialDirectory : String): String;
begin  // Correctly selects the initial directory on MACs.
  Result := InitialDirectory;
end;

//------------------------------
procedure TrimTaxaName(var S: AnsiString);
var
  I, K, L: Longint;
begin
  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  for I:= 1 to Length(S) do
    if not (S[I] in ValidOtuNameStartSet - ['_']) then
      S[I] := ' '
    else
      break;
  S := Trim(S);

  for I:= 2 to Length(S) do
    if not (S[I] in ValidOtuNameContinueSet -['_']) then
      S[I] := ' ';
    // change everthing in a blank

  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  // now L <> 0 then
  I := 1;
  while (I < L) do
  begin
    if ((S[I] <= ' ') and (S[I+1] <= ' ')) then
    begin
      for K := I+1 to L-1 do  // copies rest as it is
        S[K] := S[K+1];
      S[L] := #0;  // this helps us to trim right later
      Dec(L);
    end
    else
      Inc(I);
  end;

  if Length(S) > MMaxTaxaNameLen then
    S:= Copy(S,1,MMaxTaxaNameLen);
  S := TrimRight(S);
end;

procedure TrimTaxaName2(var S: String);
var
  I, K, L: Longint;
begin
  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  for I:= 1 to Length(S) do
    if not (S[I] in ValidOtuNameStartSet - ['_']) then
      S[I] := ' '
    else
      break;
  S := Trim(S);

  for I:= 2 to Length(S) do
    if not (S[I] in ValidOtuNameContinueSet -['_']) then
      S[I] := ' ';
    // change everthing in a blank

  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  // now L <> 0 then
  I := 1;
  while (I < L) do
  begin
    if ((S[I] <= ' ') and (S[I+1] <= ' ')) then
    begin
      for K := I+1 to L-1 do  // copies rest as it is
        S[K] := S[K+1];
      S[L] := #0;  // this helps us to trim right later
      Dec(L);
    end
    else
      Inc(I);
  end;

  if Length(S) > MMaxTaxaNameLen then
    S:= Copy(S,1,MMaxTaxaNameLen);
  S := TrimRight(S);
end;

//------------------------------
procedure TrimName(var S: AnsiString; MaxLen: Longint);
var
  I, K, L: Longint;
begin
  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  for I:= 1 to Length(S) do
    if S[I] = '_' then S[I] := ' ';

  // now L <> 0 then
  I := 1;
  while (I < L) do
  begin
    if ((S[I] <= ' ') and (S[I+1] <= ' ')) then
    begin
      for K := I+1 to L-1 do  // copies rest as it is
        S[K] := S[K+1];
      S[L] := #0;  // this helps us to trim right later
      Dec(L);
    end
    else
      Inc(I);
  end;
  if (MaxLen > 0) and (Length(S) > MaxLen) then
    S[MaxLen+1] := #0;  // to ensure trimming
  S := TrimRight(S);
end;

//------------------------------
procedure TrimNameWithAllowAlphaNum(var S: AnsiString; MaxLen: Longint);
var
  I, K, L: Longint;
begin
  for I:= 1 to Length(S) do
  begin
    case Upcase(S[i]) of
      'A'..'Z', '0'..'9': ; // does nothing
    else   S[I] := ' ';
    end;
  end;

  // check it again
  S := Trim(S);  // trims all leading and trailing junk
  L := Length(S);
  if L = 0 then
    Exit;

  // now L <> 0 then
  I := 1;
  while (I < L) do
  begin
    if ((S[I] <= ' ') and (S[I+1] <= ' ')) then
    begin
      for K := I+1 to L-1 do  // copies rest as it is
        S[K] := S[K+1];
      S[L] := #0;  // this helps us to trim right later
      Dec(L);
    end
    else
      Inc(I);
  end;
  if (MaxLen > 0) and (Length(S) > MaxLen) then
    S[MaxLen+1] := #0;  // to ensure trimming
  S := TrimRight(S);
end;

function CaptionToText(S: AnsiString): AnsiString;
var
  APos:  Integer;
begin
  Result := EmptyStr;
  APos := Pos('&',S);

  if APos > 0 then // as initial & has no difference
    Result := Copy(S, 1, APos-1) + Copy(S, APos+1, length(S))
  else
    Result := S;
  Result := Trim(Result);
end;

function CalculateIncrement(aValue: Extended): Double;
var
  temp: String;
  index: Integer;
  numZeros: Integer = 0;
begin
  if CompareValue(aValue, 1000, FP_CUTOFF) >= 0 then
    Result := 100
  else if CompareValue(aValue, 100, FP_CUTOFF) >= 0 then
    Result := 10
  else if CompareValue(aValue, 50, FP_CUTOFF) >= 0 then
    Result := 5
  else if CompareValue(aValue, 10, FP_CUTOFF) >= 0 then
    Result := 2
  else if CompareValue(aValue, 1, FP_CUTOFF) >= 0 then
    Result := 1
  else if CompareValue(aValue, 0, FP_CUTOFF) > 0 then
  begin
    temp := Format('%.20f', [aValue]);
    index := Pos(FormatSettings.DecimalSeparator, temp);
    if (index >= 1) and (index < Length(temp)) then
    begin
      inc(index);
      while temp[index] = '0' do
      begin
        inc(NumZeros);
        inc(index);
      end;
      Result := Power(10, -1*numZeros);
    end
    else
      Result := 0.01;
  end
  else
    Result := 0.1;
end;

function IsUnambiguousAminoAcid(x: AnsiChar): Boolean;
begin
  Result := False;
  case UpCase(x) of
    'B', 'J', 'O', 'U', 'X', 'Z': Result := False;
    'A','C'..'I','K'..'N','P'..'T','V'..'W','Y': Result := True;
  end;
end;

function IsUnambiguousNucleotide(x: AnsiChar): Boolean;
begin
  Result := False;
  case UpCase(x) of
    'A', 'T', 'C', 'G', 'U': Result := True;
  end;
end;

function IsUnambiguousMappedNucleotide(x: AnsiChar): Boolean;
begin
  Result := False;
  if (x = pmBaseA) or (x = pmBaseG) or (x = pmBaseC) or (x = pmBaseT) then
    Result := True;
end;

function IsValidSiteLabel(x: AnsiChar): Boolean;
begin
   case x of
     'A'..'Z',
     'a'..'z',
     '0'..'9',
     '*','+','-','_': Result := True;
   else
     Result := False;
   end;
end;

{function NucToTvParsimMap(x: Char): Char;
begin
  case UpCase(x) of
    'A', 'G',     'R'    : Result := pmBaseA;
    'T','U', 'C', 'Y'    : Result := pmBaseT;
  else
    Result := Char(Longint(pmBaseA) + Longint(pmBaseT));
  end;
end; }

function DistMapToNuc(x: AnsiChar): AnsiChar;
begin
 Result := '?';
 if x = gdBaseA then Result := 'A'
 else if x = gdBaseT then Result := 'T'
 else if x = gdBaseU then Result := 'U'
 else if x = gdBaseC then Result := 'C'
 else if x = gdBaseG then Result := 'G'
 else if x = gdBaseN then Result := 'N';
end;

function DistMapToAmino(x: AnsiChar): AnsiChar;
begin
 if x = gdResiA then Result := 'A'
 else if x = gdResiC then Result := 'C'
 else if x = gdResiD then Result := 'D'
 else if x = gdResiE then Result := 'E'
 else if x = gdResiF then Result := 'F'
 else if x = gdResiG then Result := 'G'
 else if x = gdResiH then Result := 'H'
 else if x = gdResiI then Result := 'I'
 else if x = gdResiK then Result := 'K'
 else if x = gdResiL then Result := 'L'
 else if x = gdResiM then Result := 'M'
 else if x = gdResiN then Result := 'N'
 else if x = gdResiP then Result := 'P'
 else if x = gdResiQ then Result := 'Q'
 else if x = gdResiR then Result := 'R'
 else if x = gdResiS then Result := 'S'
 else if x = gdResiT then Result := 'T'
 else if x = gdResiV then Result := 'V'
 else if x = gdResiW then Result := 'W'
 else if x = gdResiY then Result := 'Y'
 else if x = gdResiX then Result := 'X'
 else
   Result := '?';
end;

function NucToDistMap(Ch: AnsiChar): AnsiChar;
begin
  Result := gdBaseN;
  case Upcase(Ch) of
    'T','U': Result := gdBaseT;
    'A':     Result := gdBaseA;
    'C':     Result := gdBaseC;
    'G':     Result := gdBaseG;
  end;
end;

function AminoToDistMap(Ch: AnsiChar): AnsiChar;
begin
  Result := gdResiX;
  // it must be 0..20
  case UpCase(Ch) of
    'A': Result := gdResiA;
    'C': Result := gdResiC;
    'D': Result := gdResiD;
    'E': Result := gdResiE;
    'F': Result := gdResiF;
    'G': Result := gdResiG;
    'H': Result := gdResiH;
    'I': Result := gdResiI;
    'K': Result := gdResiK;
    'L': Result := gdResiL;
    'M': Result := gdResiM;
    'N': Result := gdResiN;
    'P': Result := gdResiP;
    'Q': Result := gdResiQ;
    'R': Result := gdResiR;
    'S': Result := gdResiS;
    'T': Result := gdResiT;
    'V': Result := gdResiV;
    'W': Result := gdResiW;
    'Y': Result := gdResiY;
  end;
end;

function NucToParsimMap(x: AnsiChar): AnsiChar;
begin
  Result := pmBaseN;
  case UpCase(x) of
    'A'    : Result := pmBaseA;
    'T','U': Result := pmBaseT;
    'C'    : Result := pmBaseC;
    'G'    : Result := pmBaseG;
    'R'    : Result := pmBaseR;
    'Y'    : Result := pmBaseY;

    'M'    : Result := pmBaseM;
    'K'    : Result := pmBaseK;
    'S'    : Result := pmBaseS;
    'W'    : Result := pmBaseW;
    'H'    : Result := pmBaseH;
    'B'    : Result := pmBaseB;
    'V'    : Result := pmBaseV;
    'D'    : Result := pmBaseD;
    'N'    : Result := pmBaseN;
    'X'    : Result := pmBaseN;
  end;
end;

function AminoToParsimMap(x: AnsiChar): Longint;
begin
  Result := pmResiX;
  case UpCase(x) of
    'A' : Result := pmResiA;
    'C' : Result := pmResiC;
    'D' : Result := pmResiD;
    'E' : Result := pmResiE;
    'F' : Result := pmResiF;
    'G' : Result := pmResiG;
    'H' : Result := pmResiH;
    'I' : Result := pmResiI;
    'K' : Result := pmResiK;
    'L' : Result := pmResiL;
    'M' : Result := pmResiM;
    'N' : Result := pmResiN;
    'P' : Result := pmResiP;
    'Q' : Result := pmResiQ;
    'R' : Result := pmResiR;
    'S' : Result := pmResiS;
    'T' : Result := pmResiT;
    'V' : Result := pmResiV;
    'W' : Result := pmResiW;
    'Y' : Result := pmResiY;
    'B' : Result := pmResiB;
    'Z' : Result := pmResiZ;
    'X' : Result := pmResiX;
  end;
end;

//Recoding functions; assumes that a vertical bar is separater of states;
//It allows for bases to coded as an allele; all bases missing from the coding are coded ambiguous
// E.g., for 20 amino acids:       ACDEFGHIKLMNPQRSTVWY
// E.g., N-content binary code is       WQNLRH    [ for >0 nitrogen in the side chain]
// E.g., N-content multiallele code is  WQN|KH|R  [for 1, 2, and 3 nitrogens)
function ConvertStringToRecodeScheme(AList: AnsiString; var RecodeScheme: TArrayAtoZofChar; IgnoreBase: AnsiChar): AnsiString;  // always returns a 26 character long array of char
var
  j: Integer;
  c, CurBase, CurCode: AnsiChar;
  FoundOpenParantheses, NeedNextSymbol: Boolean;
begin
  Result := EmptyStr;
  CurCode := #0; // Initalizing the CurCode
  for c:='A' to 'Z' do
    RecodeScheme[c] := IgnoreBase;

  FoundOpenParantheses := False;
  NeedNextSymbol := True;
  for j:= 1 to Length(AList) do // for each character
  begin
    if AList[j] = '(' then
    begin
      FoundOpenParantheses := True;
      continue;
    end
    else if AList[j] = ')' then
      break;
    if not FoundOpenParantheses then
      continue;
    // otherwise code
    CurBase := UpCase(AList[j]);
    if CurBase = '|' then
    begin
      NeedNextSymbol := True;
      Continue;
    end;

    if CurBase in ['A'..'Z'] then
    begin
      if NeedNextSymbol then
      begin
        CurCode := CurBase;
        NeedNextSymbol := False;
      end;

      if RecodeScheme[CurBase] = IgnoreBase  then // i.e., not yet modified
        RecodeScheme[CurBase] :=  CurCode;     // first recoding is used when conflict
     end;
  end;
end;

// It is a 0..26 character array
function RecodeNucOrAmino(Ch: AnsiChar; const RecodeScheme: TArrayAtoZofChar): AnsiChar;
begin
  Result := Ch;
  if UpCase(ch) in ['A'..'Z'] then
    Result := RecodeScheme[Upcase(Ch)];
end;

//
function ParsimMapToNuc(x: AnsiChar): AnsiChar;
begin
  Result := '?';
  case x of
   #1   : Result := 'A';
   #2   : Result := 'T';
   #4   : Result := 'C';
   #8   : Result := 'G';
   #15  : Result := 'N';
   #9   : Result := 'R';
   #6   : Result := 'Y';
   #5   : Result := 'M';
   #10  : Result := 'K';
   #12  : Result := 'S';
   #3   : Result := 'W';
   #7   : Result := 'H';
   #14  : Result := 'B';
   #13  : Result := 'V';
   #11  : Result := 'D';
   #255 : Result := 'X';
  end;
end;

function ParsimMapToNucStr(x: Longint): AnsiString;
begin
  Result := EmptyStr;
  if x >= Longint(pmBaseN) then  // maximum; N
  begin
    Result := '?';
    exit;
  end;

  if (x and Longint(pmBaseA)) <> 0 then Result := Result+'A';   //pmBaseA
  if (x and Longint(pmBaseT)) <> 0 then Result := Result+'T';   //pmBaseT
  if (x and Longint(pmBaseC)) <> 0 then Result := Result+'C';   //pmBaseC
  if (x and Longint(pmBaseG)) <> 0 then Result := Result+'G';   //pmBaseG
end;

function ParsimMapToAmino(x: Longint): AnsiChar;  // must be single base
begin
  Result := '?';
  case x of
    $00001: Result := 'A';
    $00002: Result := 'C';
    $00004: Result := 'D';
    $00008: Result := 'E';
    $00010: Result := 'F';
    $00020: Result := 'G';
    $00040: Result := 'H';
    $00080: Result := 'I';
    $00100: Result := 'K';
    $00200: Result := 'L';
    $00400: Result := 'M';
    $00800: Result := 'N';    
    $01000: Result := 'P';
    $02000: Result := 'Q';    
    $04000: Result := 'R';    
    $08000: Result := 'S';
    $10000: Result := 'T';
    $20000: Result := 'V';    
    $40000: Result := 'W';    
    $80000: Result := 'Y';
    $08004: Result := 'B';    
    $02008: Result := 'Z';
    $FFFFF: Result := 'X';
  end;
end;

function ParsimMapToAminoStr(x: Longint): AnsiString;
begin
  Result := EmptyStr;
  if x >= pmResiX then // missing/gap data
  begin
    Result := '?';
    exit;
  end;

  if (x and pmResiA) <> 0 then Result := Result+'A';
  if (x and pmResiC) <> 0 then Result := Result+'C';
  if (x and pmResiD) <> 0 then Result := Result+'D';
  if (x and pmResiE) <> 0 then Result := Result+'E';
  if (x and pmResiF) <> 0 then Result := Result+'F';
  if (x and pmResiG) <> 0 then Result := Result+'G';
  if (x and pmResiH) <> 0 then Result := Result+'H';
  if (x and pmResiI) <> 0 then Result := Result+'I';
  if (x and pmResiK) <> 0 then Result := Result+'K';
  if (x and pmResiL) <> 0 then Result := Result+'L';
  if (x and pmResiM) <> 0 then Result := Result+'M';
  if (x and pmResiN) <> 0 then Result := Result+'N';
  if (x and pmResiP) <> 0 then Result := Result+'P';
  if (x and pmResiQ) <> 0 then Result := Result+'Q';
  if (x and pmResiR) <> 0 then Result := Result+'R';
  if (x and pmResiS) <> 0 then Result := Result+'S';
  if (x and pmResiT) <> 0 then Result := Result+'T';
  if (x and pmResiV) <> 0 then Result := Result+'V';
  if (x and pmResiW) <> 0 then Result := Result+'W';
  if (x and pmResiY) <> 0 then Result := Result+'Y';
end;

// parsim informativeness
function IsNucColParsimInfo(CharArray: PAnsiChar; NTaxa: LongInt; var MinTLen: LongInt): Boolean;
var
  Freq: array['A'..'Z'] of LongInt;
  MaxFreq, Kinds,j: LongInt;
  x: AnsiChar;
begin
  MinTLen := 0;
  for x:='A' to 'Z' do
    Freq[x] := 0;

  for j:=0 to NTaxa-1 do
  begin
    x:= Upcase(CharArray[j]);
    if IsUnambiguousNucleotide(x) then
      Inc(Freq[x]);
  end;

  Kinds := 0;
  MaxFreq := 0;
  for x:='A' to 'Z' do
  begin
    if Freq[x] > 1 then Inc(Kinds);
    MinTLen := MinTLen + Freq[x];
    if Freq[x] >= MaxFreq then
      MaxFreq := Freq[x];
  end;
  MinTLen := MinTLen - MaxFreq;
  Result := Kinds > 1;
  if Result then
    MinTLen := 0; // as this is an informative site
end;

function IsAminoColParsimInfo(CharArray: PAnsiChar; NTaxa: LongInt; var MinTLen: LongInt): Boolean;
var
  Freq: array['A'..'Z'] of LongInt;
  MaxFreq, Kinds, j: LongInt;
  x: AnsiChar;
begin
  MinTLen := 0;
  for x:='A' to 'Z' do
    Freq[x] := 0;

  for j:=0 to NTaxa-1 do
  begin
    x:= Upcase(CharArray[j]);
    if IsUnambiguousAminoAcid(x) then
      Inc(Freq[x])
  end;

  Kinds := 0;
  MaxFreq := 0;
  for x:='A' to 'Z' do
  begin
    if Freq[x] > 1 then Inc(Kinds);
    MinTLen := MinTLen + Freq[x];
    if Freq[x] >= MaxFreq then
      MaxFreq := Freq[x];
  end;
  MinTLen := MinTLen - MaxFreq;
  Result := Kinds > 1;
  if Result then
    MinTLen := 0; // as this is an informative site
end;

function HasAmbiguousInNucCol(CharArray: PAnsiChar; NTaxa: LongInt): Boolean;
var
  j: Longint;
  x: AnsiChar;
begin
  Result := True;
  for j:=0 to NTaxa-1 do
  begin
    x:= Upcase(CharArray[j]);
    if not IsUnambiguousNucleotide(x) then
      Exit;
  end;
  Result := False;
end;

function HasAmbiguousInAminoCol(CharArray: PAnsiChar; NTaxa: LongInt): Boolean;
var
  j: Longint;
  x: AnsiChar;
begin
  Result := True;
  for j:=0 to NTaxa-1 do
  begin
    x:= Upcase(CharArray[j]);
    if not IsUnambiguousAminoAcid(x) then
      Exit;
  end;
  Result := False;
end;


function HasAmbiguousInCodonMapCol(CharArray: PAnsiChar; NTaxa: LongInt): Boolean;
var
  j: Longint;
begin
  Result := True;
  for j:=0 to NTaxa-1 do
  begin
    if CharArray[j] > #63 then
      Exit;
  end;
  Result := False;
end;


//---- These 3 functions used in preparing data with partial deletion ---

function CountAmbiguousInNucCol(CharArray: PAnsiChar; NTaxa: LongInt): Integer;
var
  j: Longint;
  x: AnsiChar;
begin
  Result := 0;
  for j:=0 to NTaxa-1 do
  begin
    x:= Upcase(CharArray[j]);
    if not IsUnambiguousNucleotide(x) then
      Inc(Result) ;
  end;
end;

function CountAmbiguousInAminoCol(CharArray: PAnsiChar; NTaxa: LongInt): Integer;
var
  j: Longint;
  x: AnsiChar;
begin
  Result := 0;
  for j:=0 to NTaxa-1 do
  begin
    x:= Upcase(CharArray[j]);
    if not IsUnambiguousAminoAcid(x) then
      Inc(Result);
  end;
end;


function CountAmbiguousInCodonMapCol(CharArray: PAnsiChar; NTaxa: LongInt): Integer;
var
  j: Longint;
begin
  Result := 0;
  for j:=0 to NTaxa-1 do
  begin
    if CharArray[j] > #63 then
      Inc(Result);
  end;
end;

//--- END InCol functions ---


function HasStopAminoInAminoCol(CharArray: PAnsiChar; NTaxa: LongInt; lastCol: Boolean): Boolean;
var
  j: Longint;
  x: AnsiChar;
begin
  Result := True;
  for j:=0 to NTaxa-1 do
  begin
    x:= Upcase(CharArray[j]);
    if  x = '*' then
    begin
      {$IFNDEF VISUAL_BUILD}
      if not SuppressStopCodonsMessage then
      begin
        if lastCol then
          Warn_NV('Stop codon(s) were found at the end of at least 1 sequence in your alignment.  They were removed and the analysis continued.')
        else
          Warn_NV('Stop codon(s) were found in your alignment.  They were removed and the analysis continued.');
        SuppressStopCodonsMessage := True;
      end;
      {$ENDIF}
      Exit;
    end;
  end;
  Result := False;
end;

function ConvertStopToMissingData(var CharArray: PAnsiChar; NTaxa: LongInt; lastCol: Boolean): Boolean;
var
  i: Longint;
  x: AnsiChar;
begin
  Result := False;
  for i := 0 to NTaxa - 1 do
  begin
    x:= CharArray[i];
    if  x = '*' then
    begin
      {$IFNDEF VISUAL_BUILD}
      if not SuppressStopCodonsMessage then
      begin
        if lastCol then
          Warn_NV('Stop codon(s) were found at the end of at least 1 sequence in your alignment.  They were treated as missing data and the analysis continued.')
        else
          Warn_NV('Stop codon(s) were found in your alignment.  They were treated as missing data and the analysis continued.');
        SuppressStopCodonsMessage := True;
      end;
      {$ENDIF}
      CharArray[i] := '?';
      Result := True;
    end;
  end;
end;

function ConvertCodonMapStopToMissingData(var CharArray: PAnsiChar; NTaxa: LongInt; const ACodeTable: array of AnsiChar): Boolean;
var
  i: Longint;
  x: AnsiChar;
begin
  Result := False;
  for i:=0 to NTaxa-1 do
  begin
    x := CharArray[i];
    x := ACodeTable[Integer(x)];
    if x = '*' then
    begin
      {$IFNDEF VISUAL_BUILD}
      if not SuppressStopCodonsMessage then
      begin
        Warn_NV('Stop codon(s) were found in your alignment.  They were treated as missing data and the analysis continued.');
        SuppressStopCodonsMessage := True;
      end;
      {$ENDIF}
      CharArray[i] := '?';
      Result := True;
    end;
  end;
end;

function HasStopCodonInCodonMapCol(CharArray: PAnsiChar; NTaxa: LongInt; const ACodeTable: array of AnsiChar): Boolean;
var
  j: Longint;
  x: AnsiChar;
begin
  Result := True;
  for j:=0 to NTaxa-1 do
  begin
    x := CharArray[j];
    if x >= #64 then
      continue; // do not handle ambiguous
    x := ACodeTable[Integer(x)];
    if x = '*' then
    begin
      {$IFNDEF VISUAL_BUILD}
      if not SuppressStopCodonsMessage then
      begin
        Warn_NV('Stop codon(s) were found in your alignment.  They were removed and the analysis continued.');
        SuppressStopCodonsMessage := True;
      end;
      {$ENDIF}
      Exit;
    end;
  end;
  Result := False;
end;

//
function GetThreeLetterCode(Ch: AnsiChar): AnsiString;
begin
  case Ch of
    '*': Result := '***';     'A': Result := 'Ala';
    'R': Result := 'Arg';     'N': Result := 'Asn';
    'D': Result := 'Asp';     'C': Result := 'Cys';
    'Q': Result := 'Gln';     'E': Result := 'Glu';
    'G': Result := 'Gly';     'H': Result := 'His';
    'I': Result := 'Ile';     'L': Result := 'Leu';
    'K': Result := 'Lys';     'M': Result := 'Met';
    'F': Result := 'Phe';     'P': Result := 'Pro';
    'S': Result := 'Ser';     'T': Result := 'Thr';
    'W': Result := 'Trp';     'Y': Result := 'Tyr';
    'V': Result := 'Val';
  else
    Result := '***';
  end;
end;

// Add to Mega Utilities for future use;
procedure ConvertVarToStdErrMat(MyV: PDistanceMatrix; n: Longint; IsFullRect: Boolean);
var
  i, j: Longint;
begin
  for i:= 0 to n-1 do
    for j:= 0 to n-1 do
    begin
      if i = j then MyV[i][i] := -1
      else if (j > i) and (not IsFullRect) then
        Break
      else if MyV[i][j] > 0 then
        MyV[i][j] := SQRT(MyV[i][j]);
    end;
end;

procedure ConvertVarToStdErrArray(MyV: ArrayOfDouble; n: Longint);
var
  i: Longint;
begin
  for i:= 0 to n-1 do
    if MyV[i] > 0 then
      MyV[i] := SQRT(MyV[i]);
end;

procedure SwapRectDistEntries(D: PDistanceMatrix; n, x, y: Longint);
var
  dist: Double;
  i: Longint;
begin
  // n= no of otus
  if x = y then  Exit;
  if x < y then
  begin
    i := x; x := y; y := i;
  end;

  //==== so x > y always
  for i:=0 to y-1 do  // x > i and y > i
  begin
    // swap lower left
    dist := D[x][i];
    D[x][i] := D[y][i];
    D[y][i] := dist;
    // swap upper right
    dist := D[i][x];
    D[i][x] := D[i][y];
    D[i][y] := dist;
  end;

  // i = y is not necessary since it needs a diagonal elt
  for i:= y+1 to x-1 do  // x > i and y < i
  begin
    // swap lower left
    dist := D[x][i];
    D[x][i] := D[i][y];
    D[i][y] := dist;
    // swap upper right
    dist := D[i][x];
    D[i][x] := D[y][i];
    D[y][i] := dist;
  end;

  // i = x is not necessary since a diagonal entry will be involved
  for i:= x+1 to n-1 do // x < i and y < i
  begin
    // swap lower left
    dist := D[i][x];
    D[i][x] := D[i][y];
    D[i][y] := dist;
    // swap upper right
    dist := D[x][i];
    D[x][i] := D[y][i];
    D[y][i] := dist;
  end;
end;

// Assumes that D is in lower left; and that the diagonal is available
procedure SwapDistEntries(D: PDistanceMatrix; n, x, y: Longint);
var
  dist: Double;
  i: Longint;
begin
  // n= no of otus
  if x = y then  Exit;
  if x < y then
  begin
    i := x; x := y; y := i;
  end;

  //==== so x > y always
  for i:=0 to y-1 do  // x > i and y > i
  begin
    dist := D[x][i];
    D[x][i] := D[y][i];
    D[y][i] := dist;
  end;

  // i = y is not necessary since it needs a diagonal elt
  for i:= y+1 to x-1 do  // x > i and y < i
  begin
    dist := D[x][i];
    D[x][i] := D[i][y];
    D[i][y] := dist;
  end;

  // i = x is not necessary since a diagonal entry will be involved
  for i:= x+1 to n-1 do // x < i and y < i
  begin
    dist := D[i][x];
    D[i][x] := D[i][y];
    D[i][y] := dist;
  end;
end;

procedure FreeDistMatrix(Value: PDistanceMatrix; n: Longint);
var
  i: Longint;
begin
  if not Assigned(Value) then
    Exit;
  for i := 0 to n-1 do
  if Assigned(Value[i]) then
    FreeMemAndNil(Value[i]);
  FreeMemAndNil(Value);
end;

function NewDistMatrix(n: Longint; IsFullRect: Boolean): PDistanceMatrix;
var
  i: Longint;
begin
  Result := nil;
  try
    GetMem(Result, n*sizeOf(PArrayOfDouble));
    for i:=0 to n-1 do
      Result[i] := nil;

    for i:=0 to n-1 do
      if IsFullRect then
      begin
        Getmem(Result[i], n*sizeOf(Double));
      end
      else
      begin
        Getmem(Result[i], (i+1)*sizeOf(Double));
      end;

  except
    On E: Exception do
    begin
      if Result <> nil then
      begin
        for i:=0 to n-1 do if Result[i] <> nil then FreeMemAndNil(Result[i]);
        FreeMemAndNil(Result);
        Result := nil;
      end;
      RaiseErrorMessage(HC_Unexpected_Error, E.message);
    end;
  end;
end;

procedure CopyDistMatrix(n: Longint; Dest, Source: PDistanceMatrix; IsFullRect: Boolean);
var
  i, j: Integer;
begin
  for i:=0 to n-1 do
    for j:= 0 to n-1 do
    begin
      if (j>=i) and (not IsFullRect) then
        break;
      Dest[i][j] := Source[i][j];
    end;
end;

function UserVarMatrixToFile(m: PDistanceMatrix; filename: String; numSeqs: Integer): Boolean;
var
  i, j: Integer;
  outFile: Text;
  tempVal: Extended;
begin
  AssignFile(outFile, filename);
  Rewrite(outFile);
  try
  for i := 1 to numSeqs - 1 do
    for j := 0 to i - 1 do
    begin
      if m[i][j] > 0 then
        tempVal := sqrt(m[i][j])
      else
        tempVal := 0;
      WriteLn(outFile, Format('%.6f', [tempVal]));
    end;
  finally
    CloseFile(outFile);
  end;
  Result := FileExists(filename);
end;


function CreateDistanceMatrix(NoOfOTUs: integer):PDistanceMatrix;
var i : integer;
begin
  GetMem(Result, SizeOf(Pointer)*NoOfOTUs);
  for i := 0 to NoOfOTUs-1 do
    GetMem(Result[i], SizeOf(double)*NoOfOTUs);
  for i := 0 to NoOfOTUs-1 do
    FillChar(Result[i]^, SizeOf(double)*NoOfOTUs, 0);
end;

procedure CopyDistanceMatrix(Source, Copy: PDistanceMatrix; NoOfOTUs: integer);
var i,j: integer;
begin
  for i := 0 to NoOfOTUs-1 do
    for j := 0 to NoOfOTUs-1 do
      Copy[i][j] := Source[i][j];
end;

procedure DestroyDistanceMatrix(var D: PDistanceMatrix; NoOfOTUs: integer);
var i : integer;
begin
    if D = nil then Exit;
    for i := 0 to NoOfOTUs-1 do
      FreeMemAndNil(D[i]);
    FreeMemAndNil(D);
    D := nil;
end;

function NewDistArray(n: Longint): PArrayOfDouble;
begin
  GetMem(Result, n*sizeOf(Double));
end;

function BlankString(Value: Longint):AnsiString;
begin
  result := DupeString(' ',Value); // Previous implementaiton was limited to 100 chars, when we specified over 100 chars we ended up with a char buffer overflow which ended up overwriting stack memory and crashing the program.
end;

function StrToStrWidth(Value: AnsiString; Width: Longint):AnsiString;
var
 Len,i : Longint;
 Buf: array[0..1024] of AnsiChar;
begin
  Len := Length(Value);
  if (Len < Width) and (Width < 1024) then
  begin
    for i:=0 to Len-1 do
      Buf[i] := Value[i+1]; // +1 because String index begins with 1
    for i:= Len to Width-1 do
      Buf[i] := ' ';
    Buf[Width] := #0;
    Result := AnsiString(Buf);
  end
  else
    Result := Value;
end;

function PadStrToWidth(Value: AnsiString; Width: Longint):AnsiString;
var
 Len,i : Longint;
 Buf: array[0..1024] of AnsiChar;
begin
  Len := Length(Value);
  if (Len < Width) and (Width < 1024) then
  begin
    for i:=0 to Len-1 do
      Buf[i] := Value[i+1]; // +1 because String index begins with 1
    for i:= Len to Width-1 do
      Buf[i] := ' ';
    Buf[Width] := #0;
    Result := AnsiString(Buf);
  end
  else
    Result := Value;
end;

function StrToIntWithInvalid(AStr: AnsiString; InvalidValue: Integer=-1): Integer;  // change prevents developers from seeing an exception popup (cleaner)
var
  ConvertedInt: Integer;
begin
  if TryStrToInt(AStr, ConvertedInt) then
    Result := ConvertedInt
  else
    Result := InvalidValue;
end;

function StrToFloatWithInvalid(AStr: AnsiString; InvalidValue: double=-1): Double;  // change prevents developers from seeing an exception popup (cleaner)
var
  ConvertedFloat: double;
begin
  if TryStrToFloat(AStr, ConvertedFloat) then
    Result := ConvertedFloat
  else
    Result := InvalidValue;
end;

const BufSize          = 4096;
// For int to string
function IntToStrWidth(Value, Width: Longint):AnsiString;
var
 Len,i : Longint;
 Buf: array[0..BufSize+1] of AnsiChar;
begin
  if Width >= BufSize then
    RaiseErrorMessage(HC_Unexpected_Error, 'IntToStrWidth asked to write too long a string.');
  Result := IntToStr(Value);
  Len := Length(Result);
  if (Len < Width) and (Width < BufSize) then
  begin
    for i:= 0 to (Width-Len)-1 do
      Buf[i] := ' ';
    for i:=0 to Len-1 do
      Buf[Width-Len+i] := Result[i+1]; // +1 because String index begins with 1
    Buf[width] := #0;
    Result := AnsiString(Buf);
  end;
end;

function NegIntToStrWidth(Value, Width: Longint):AnsiString;
begin
  if Width >= BufSize then
    RaiseErrorMessage(HC_Unexpected_Error, 'NegIntToStrWidth asked to write too long a string.');
  if Value >= 0 then Result := ' '+IntToStrWidth(Value, Width-1)
  else               Result := '-'+IntToStrWidth(Value, Width-1);
end;

//--- Formatting strings with writing '-' or 'n/c'
function WriteIntToStrWidth(Value, Width, Cutoff: Longint):AnsiString;
begin
  if Width >= BufSize then
    RaiseErrorMessage(HC_Unexpected_Error, 'WriteIntToStrWidth asked to write too long a string.');
  if Value >= Cutoff then Result := NegIntToStrWidth(Value, Width)
                     else Result := StrToStrWidth('n/c', Width);
end;

// For float to str width
function FloatToStrWidth(Value: Double; Width, digits: Longint):AnsiString;
var
 Len,i : Longint;
 Buf: array[0..BufSize+1] of AnsiChar;
begin
  if Width >= BufSize then
    RaiseErrorMessage(HC_Unexpected_Error, 'FloatToStrWidth asked to write too long a string.');
  Result := FloatToStrF(Value, ffFixed, width, digits);
  Len := Length(Result);
  if (Len < Width) and (Width < BufSize) then
  begin
    for i:= 0 to (Width-Len)-1 do
      Buf[i] := ' ';
    for i:=0 to Len-1 do
      Buf[Width-Len+i] := Result[i+1]; // +1 because String index begins with 1
    Buf[width] := #0;
    Result := AnsiString(Buf);
  end;
end;

function FloatToPrecision(Value: Double; digits: Longint):Double;
var
 StrFloat: AnsiString;
 PreciseFloat: Double;
begin
  StrFloat := FloatToStrF(Value, ffFixed, 100, digits);
  if TryStrToFloat(StrFloat, PreciseFloat) then
    result := PreciseFloat
  else
    result := Value; // If for some reason we can't convert back to float then just return the origional value.  We will just see more precision than we should but MEGA still continues.  
end;

function StrToStrPrecision(Value: AnsiString; digits: Longint): AnsiString;
var
   PreciseFloat: Double;
begin
  if TryStrToFloat(Value, PreciseFloat) then
  begin
    PreciseFloat := FloatToPrecision(PreciseFloat, digits);
    Result := FloatToStr(PreciseFloat);
  end
  else
    result := value;
end;

function FloatToStrPrecision(Value : Double; digits: Longint = 10): AnsiString;
begin
  result := FloatToStr(Value);
  While(Length(result) - Pos('.', result) < digits) do
  begin
    result := result + '0';
  end;

end;

function NegFloatToStrWidth(Value: Double; Width, digits: Longint):AnsiString;
begin
  if Width >= BufSize then
    RaiseErrorMessage(HC_Unexpected_Error, 'NegFloatToStrWidth asked to write too long a string.');
  if Value >= 0.0 then Result := ' '+FloatToStrWidth(Value, Width-1, digits)
                  else Result := '-'+FloatToStrWidth(-Value, Width-1, digits);
end;

function WriteFloatToStrWidth(Value: Double; Width, digits: Longint; Cutoff: Double):AnsiString;
begin
  if Width >= BufSize then
    RaiseErrorMessage(HC_Unexpected_Error, 'WriteFloatToStrWidth asked to write too long a string.');
  if Value >= Cutoff then Result := NegFloatToStrWidth(Value, Width, digits)
                     else Result := StrToStrWidth('n/c', Width);
end;

function OtuNameToMegaStr(Value: AnsiString): AnsiString;
var
  i, Len: Longint;
begin
  Result := Trim(Value);
  Len := Length(Result);
  for i:=1 to Len do
    if Result[i] = ' ' then
      Result[i] := '_';
end;

//---- All math functions are here
var
  Idum : Longint = -1;
  Ran1_Ix1, Ran1_Ix2, Ran1_Ix3: Longint;
  Ran1_R: array[0..98] of Double;
  Factrl_ntop: Longint = 0;
  Factrl_a: array[0..32]  of Double;
  Factln_a: array[0..99] of Double;
  PoiDev_sq: Double;
  PoiDev_alxm: Double;
  PoiDev_g: Double;
  PoiDev_oldm: Double = -1.0;

const
  GammaLnCof: array[0..5] of Double=(76.18009172947146,-86.50532032941677,
 		                     24.01409824083091,-1.231739572450155,
                                     0.1208650973866179e-2,-0.5395239384953e-5);
  Cfn_ITMAX = 1000;  // ITerations MAXimum = 1000; safer
  Cfn_EPS   =3.0e-7;
  Cfn_FPMIN =1.0e-30;

   m1 = 259200;
  ia1 = 7141;
  ic1 = 54773;
  rm1 = 1/m1;
   m2 = 134456;
  ia2 = 8121;
  ic2 = 28411;
  rm2 = 1/m2;
   m3 = 243000;
  ia3 = 4561;
  ic3 = 51349;

function IsReltimeMLAnalysis(Option: TDistTreeDlgOption): Boolean;
begin
  case Option of
    dtdoRelTimeML, dtdoCorrTestML, dtdoRtdtML, dtdoLbsTiming: Result := True;
    else
      Result := False;
  end;
end;

function IsReltimeNonMLAnalysis(Option: TDistTreeDlgOption): Boolean;
begin
  case Option of
    dtdoRelTimeBLens, dtdoRelTimeLS, dtdoCorrTestBlens, dtdoRtdtLS, dtdoRtdtBlens: Result := True;
    else
      Result := False;
  end;
end;

function IsReltimeBlensAnalysis(Option: TDistTreeDlgOption): Boolean;
begin
  case Option of
    dtdoRelTimeBLens, dtdoCorrTestBlens, dtdoRtdtBlens: Result := True;
    else
      Result := False;
  end;
end;

function IsCorrTestAnalysis(Option: TDistTreeDlgOption): Boolean;
begin
  case Option of
    dtdoCorrTestBlens, dtdoCorrTestML: Result := True;
    else
      Result := False;
  end;
end;

function FloatTimeToDateTime(floatTime: Extended): TDateTime;
var
  floatPart: Extended;
  aYear: Word;
  aDayOfYear: Integer;
begin
  if CompareValue(floatTime, 0, FP_CUTOFF) <= 0 then
  begin
    Result := MinDateTime;
  end
  else
  begin
    aYear := trunc(floatTime);
    floatPart := frac(floatTime);
    aDayOfYear := min(DaysInAYear(aYear), Round(floatPart*DaysInAYear(aYear)));
    Result := EncodeDateDay(aYear, max(1, aDayOfYear));
  end;
end;

function FloatTimeToDateString(floatTime: Extended; formatStr: String): String;
var
  aDate: TDateTime;
begin
  if CompareValue(floatTime, 1.0, FP_CUTOFF) > 0 then
  begin
    aDate := FloatTimeToDateTime(floatTime);
    Result := FormatDateTime(formatStr, aDate);
  end
  else
    Result := IntToStr(Trunc(floatTime));
end;

function RtdtDaysElapsed(latestRtdtTime: Extended; rtdtDivTime: Extended): Int64;
var
  aNow, aThen: TDateTime;
  years: Int64 = -1;
  days: Int64 = -1;
begin
  try
    if (CompareValue(latestRtdtTime, 0.0, FP_CUTOFF) >= 0) and (CompareValue(rtdtDivTime, 0.0, FP_CUTOFF) >= 0) then
    begin
     aNow := FloatTimeToDateTime(latestRtdtTime);
     aThen := FloatTimeToDateTime(rtdtDivTime);
     Result := DaysBetween(aNow, aThen);
    end
    else if (CompareValue(latestRtdtTime, 0.0, FP_CUTOFF) < 0) and (CompareValue(rtdtDivTime, 0.0, FP_CUTOFF) < 0) then
    begin
     years := abs(trunc(latestRtdtTime) - trunc(rtdtDivTime));
     days := abs(Round(frac(latestRtdtTime)*365.25 - frac(rtdtDivTime)*365.25));
     Result := Round(years*365.25 + days);
    end
    else if CompareValue(latestRtdtTime, 0.0, FP_CUTOFF) >= 0 then
    begin
     years := trunc(latestRtdtTime) + abs(trunc(rtdtDivTime));
     days := abs(Round(frac(latestRtdtTime)*365.25 + frac(rtdtDivTime)*365.25));
     Result := Round(years*365.25 + days);
    end
    else
    begin
     years := trunc(rtdtDivTime) + abs(trunc(latestRtdtTime));
     days := abs(Round(frac(latestRtdtTime)*365.25 + frac(rtdtDivTime)*365.25));
     Result := Round(years*365.25 + days);
    end;
  except
    on E:Exception do
      Result := -1;
  end;
end;

procedure MegaRandSeed(seed: Longint);
begin
  Idum := -1*abs(seed);
end;

function PrevRandSeed: LongInt;
begin
  Result := abs(Idum);
end;

function Ran1: Double;  // used internally; not visible to the user
var
  j: Longint;
begin
    if Idum<0 then // initializes the random number generator
    begin
      Ran1_Ix1 := (ic1 - idum)            mod m1;
      Ran1_Ix1 := ((ia1 * Ran1_Ix1) + ic1) mod m1;
      Ran1_Ix2 := Ran1_Ix1 mod m2 ;
      Ran1_Ix1 := ((ia1 * Ran1_Ix1) + ic1) mod m1;
      Ran1_Ix3 := Ran1_Ix1 mod m3 ;
      for j := 1 to 97 do
      begin
	Ran1_Ix1  := ((ia1*Ran1_Ix1) + ic1) mod M1 ;
	Ran1_Ix2  := ((ia2*Ran1_Ix2) + ic2) mod M2 ;
	Ran1_R[j] := (Ran1_Ix1 + Ran1_Ix2*rm2)*rm1 ;
      end;
      Idum := 1 ;
    end;
    Ran1_Ix1 := ((ia1*Ran1_Ix1) + ic1) mod M1;
    Ran1_Ix2 := ((ia2*Ran1_Ix2) + ic2) mod M2;
    Ran1_Ix3 := ((ia3*Ran1_Ix3) + ic3) mod M3;
    j := 1 + ((97*Ran1_Ix3) div M3);
    if ((j>97) or (j<1)) then
      RaiseErrorMessage(HC_Unexpected_Error, 'Ran1 error');
    Result := Ran1_R[j];
    Ran1_R[j] := (Ran1_Ix1 + (Ran1_Ix2*rm2))*rm1; //replaces used entry
end;

// Uniform random number in the range 0 to m-1
function URan(m: Longint): Longint;
begin
  Result := Floor(Ran1*m);
  while Result = m do
    Result := Floor(Ran1*m);
end;

function SortAndDumpBootTableToFile(f: PArrayOfInt; numSites: Int64; filename: String): Boolean;
var
  aList: TLongintList = nil;
  i: Integer;
  aFile: TextFile;
begin
  try
    aList := TLongintList.Create;
    for i := 0 to numSites do
      aList.Add(f[i]);
    aList.SortAscending;
    AssignFile(aFile, filename);
    Rewrite(aFile);
    for i := 0 to aList.Count - 1 do
    WriteLn(aFile, aList[i]);
  finally
    CloseFile(aFile);
    aList.Free;
  end;
  Result := FileExists(filename);
end;

function arrayToFile(source: array of extended; destination: String):Boolean;
var
  aList: TStringList = nil;
  i: Integer;
begin
  try
    aList := TStringList.Create;
    if Length(source) > 0 then
    begin
      for i := 0 to Length(source) - 1 do
      begin
        aList.Add(Format('%8.8d %.4f', [i, source[i]]));
      end;
    end;
    aList.SaveToFile(destination);
    Result := FileExists(destination);
  finally
    if Assigned(aList) then
         aList.Free;
  end;
end;

function RandomFromLogNormalDist(x: Extended; aMean: Extended; aStdDev: Extended): Extended;
const
  RESOLUTION = 1000;
var
  unif: Extended;
begin
  repeat
    unif := random(RESOLUTION)/RESOLUTION;
  until  CompareValue(unif, 0.0, FP_CUTOFF) <> 0;
  { m and s are scaled mean and stddev}
  //m := ln(aMean*aMean/sqrt(aMean*aMean + aStdDev*aStdDev));
  //s := sqrt(ln((aMean*aMean + aStdDev*aStdDev)/aMean*aMean));
  Result := x + Max(0, exp(invnormaldist(unif)*aStdDev + aMean));
end;

function RandomFromExponentialDist(x: Extended; decay: Extended): Extended;
const
  RESOLUTION = 1000;
var
  unif: Extended;
begin
  if decay = 0 then
    Result := NaN
  else
  begin
    repeat
      unif := random(RESOLUTION)/RESOLUTION;
    until CompareValue(unif, 0.0, FP_CUTOFF) <> 0;
    Result := x - 1/decay*ln(unif);
  end;
end;

function RandomFromExponentialDistInterval(aMin, aMax, decay: Extended): Extended;
begin
  Assert((aMin >= 0) and (aMax > aMin), 'invalid bounds for exponential distribution');
  Result := RandomFromExponentialDist(aMin, decay);
  while (Result < aMin) or (Result > aMax) do
    Result := RandomFromExponentialDist(aMin, decay);
end;

// Ln Gamma function
function GammaLn(xx: Double): Double;
var
  x,y,tmp,ser: Double;
  j: Longint;
begin
  y   := xx;
  x   := xx;
  tmp := x + 5.5;
  tmp := tmp - (x+0.5)*ln(tmp);
  ser := 1.000000000190015;
  for j:=0 to 5 do
  begin
    y := y+1;
    Ser := Ser + GammaLnCof[j]/y;
  end;
  Result := -tmp + ln(2.5066282746310005*ser/x);
end;

function GammaSer(a,x: Double; var GLn: Double): Double;
var
  n: Longint;
  sum,del,ap: double;
begin
  GLn    := GammaLn(a);
  Result := 0.0;
  if x <= 0.0 then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in GammaSer function');
  ap := a;
  del := 1.0/a;
  sum := 1.0/a;
  for n := 1 to Cfn_ITMAX do  // 100 iterations
  begin
    ap  := ap+1;
    del := del*x/ap;
    sum := sum + del;
    if abs(del) < abs(sum)*Cfn_EPS then
    begin
      Result := sum*Exp(-x+a*ln(x)-GLn);
      Exit;
    end;
  end;
  // fall here if problem
  RaiseErrorMessage(HC_Unexpected_Error, 'Error in GammaSer function');
end;

function GammaCfn(a, x: Double; var Gln: Double): Double;
var
  i: Longint;
  an,b,c,d,del,h: Double;
begin
  GLn := GammaLn(a);
  b   := x+1.0-a;
  c   := 1.0/Cfn_FPMIN;
  d   := 1.0/b;
  h   := d;
  for i:= 1 to Cfn_ITMAX do  // iterations
  begin
    an := -i*(i-a);
    b  := b + 2.0;
    d  := an*d+b;
    if abs(d) < Cfn_FPMIN then
      d := Cfn_FPMIN;
    c :=b+an/c;
    if abs(c) < Cfn_FPMIN then
      c:= Cfn_FPMIN;
    d   := 1.0/d;
    del := d*c;
    h   := h*del;
    if abs(del-1.0) < Cfn_EPS then
      break;
  end;
  if i > Cfn_ITMAX then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in GammaCfn function');
  Result := Exp(-x+a*ln(x)-Gln)*h;
end;

function GammaQ(a, x: Double): Double;
var
  Gln: Double;
begin
  Gln := 0.0;
  if (x < 0.0) or (a <= 0.0) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in GammaQ function');

  if x < 0.000001 then
    result := 1.0
  else if x < (a+1.0) then
    Result := 1.0- GammaSer(a,x, GLn)
  else
    Result := GammaCfn(a,x, GLn);
end;

// Factorial of an Longint
function Factrl(n: Longint): Double;
var
  j: Longint;
begin
  if (n < 0) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in Factrl function');

  if (n > 32) then
  begin
    Result := Exp(GammaLn(n+1.0));
    Exit;
  end;

  if Factrl_ntop = 0 then  // this is for initializing
    Factrl_a[0] := 1;

  while Factrl_ntop < n do
  begin
    j:= Factrl_ntop;
    Inc(Factrl_ntop);
    Factrl_a[Factrl_ntop] := Factrl_a[j]*Factrl_ntop;
  end;
  Result := Factrl_a[n];
end;

function Factln(n: Longint): Double;
var
  i: Longint;
begin
  if (n < 0) then 
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in Factln');
  if n <= 1 then
  begin
    Result := 0;
    Exit;
  end;

  if Factln_a[0] < 0 then  // initialize
   for i:=0 to 99 do
     Factln_a[i] := -1;

  if n < 100 then
  begin
    if Factln_a[n] < 0.0 then
      Factln_a[n] := GammaLn(n+1.0);
    Result := Factln_a[n];
  end
  else
    Result := GammaLn(n+1.0);
end;

function ChiSqObsExp(const Bins, EBins: array of Double; ChSqValue: PDouble): Double;  // returns probability
var
  j, df: Longint;
  ChSq, temp: Double;
begin
  if High(Bins) <> High(EBins) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in ChiSqObsExp function');

  df   := High(Bins); // High returns actual no of elts -1
  ChSq := 0.0;
  for j:=0 to High(Bins) do
  begin
    if Ebins[j] <= 0.0 then
      RaiseErrorMessage(HC_Unexpected_Error, 'Expected value 0 in ChiSqObsExp function');
    Temp := Bins[j] - Ebins[j];
    ChSq := ChSq + (Temp/Ebins[j])*Temp;
  end;
  if ChSqValue <> nil then
    ChSqValue^ := chsq;
  Result := GammaQ(0.5*df, 0.5*ChSq);
  if Result > 0.999999999 then
    Result := 0.999999999;
end;

function ChiSqTwoBins(const Bins1, Bins2: array of Double; ChSqValue: PDouble): Double;  // returns probability
var
  j, df: Longint;
  temp, ChSq: Double;
begin
  if High(Bins1) <> High(Bins2) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in ChiSqTwoBins function');

  df   := High(Bins1);
  ChSq := 0;

  // if the row sums are different than we need to normalize
{  GetMem(SumBin, sizeof(Double)*df);
  for j:= 0 to High(Bins1) do
    SumBin[j] := 0;

  for j:= 0 to High(Bins1) do
    if (Bins1[j] <= 0.0) and (Bins2[j] <= 0.0) then
      SumBin[j] := Bins1[j]+Bins2[j];

 }
  for j:= 0 to High(Bins1) do
    if (Bins1[j] <= 0.0) and (Bins2[j] <= 0.0) then
      Dec(df)
    else
    begin
      Temp := Bins1[j] - Bins2[j];
      ChSq := ChSq + Temp/(Bins1[j]+Bins2[j])*temp;
    end;

  if df < 0 then // then categories are 0
  begin
    Result := -1;
    if ChSqValue <> nil then
      ChSqValue^ := -1;
    Exit;
  end;

  if df = 0 then
    df := 1;
  if ChSqValue <> nil then
    ChSqValue^ := chsq;
  Result := GammaQ(0.5*df, 0.5*ChSq);
  if Result > 0.999999999 then
    Result := 0.999999999;
end;

function ChiSqObsExpForSingleList(Bins, EBins: TSingleList; MaxLen: Integer; ChSqValue: PDouble): Double;  // returns probability
var
  j, df: Longint;
  ChSq, temp: Double;
begin
  if (Bins.Count < MaxLen) or (EBins.Count < MaxLen) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in ChiSqObsExpForSingleList function');

  df   := MaxLen-1; // High returns actual no of elts -1
  ChSq := 0.0;
  for j:=0 to maxLen-1 do
  begin
    if Ebins[j] <= 0.0 then
      RaiseErrorMessage(HC_Unexpected_Error, 'Expected value is 0 in ChiSqObsExpForSingleList function');
    Temp := Bins[j] - Ebins[j];
    ChSq := ChSq + (Temp/Ebins[j])*Temp;
  end;
  if ChSqValue <> nil then
    ChSqValue^ := chsq;
  Result := GammaQ(0.5*df, 0.5*ChSq);
  if Result > 0.999999999 then
    Result := 0.999999999;
end;

function ChiSqTwoBinsForSingleList(Bins1, Bins2: TSingleList; MaxLen: Integer; ChSqValue: PDouble): Double;  // returns probability
var
  j, df: Longint;
  temp, ChSq: Double;
begin
  if (Bins1.Count < MaxLen) or (Bins2.Count < MaxLen) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in ChiSqObsExpForSingleList function');

  df   := MaxLen;
  ChSq := 0;

  // if the row sums are different than we need to normalize
{  GetMem(SumBin, sizeof(Double)*df);
  for j:= 0 to High(Bins1) do
    SumBin[j] := 0;

  for j:= 0 to High(Bins1) do
    if (Bins1[j] <= 0.0) and (Bins2[j] <= 0.0) then
      SumBin[j] := Bins1[j]+Bins2[j];

 }
  for j:= 0 to MaxLen-1 do
    if (Bins1[j] <= 0.0) and (Bins2[j] <= 0.0) then
      Dec(df)
    else
    begin
      Temp := Bins1[j] - Bins2[j];
      ChSq := ChSq + Temp/(Bins1[j]+Bins2[j])*temp;
    end;

  if df < 0 then // then categories are 0
  begin
    Result := -1;
    if ChSqValue <> nil then
      ChSqValue^ := -1;
    Exit;
  end;

  if df = 0 then
    df := 1;
  if ChSqValue <> nil then
    ChSqValue^ := chsq;
  Result := GammaQ(0.5*df, 0.5*ChSq);
  if Result > 0.999999999 then
    Result := 0.999999999;
end;

function Beta(z, w: Double): Double;
begin
  Result := Exp(GammaLn(z)+gammaln(w)-GammaLn(z+w));
end;

function BetaCfn(a,b,x: Double): Double;
var
  m,m2: Longint;
  aa,c,d,del,h,qab,qam,qap: Double;
begin
  qab := a + b;
  qap := a + 1.0;
  qam := a - 1.0;
  c   := 1.0;
  d   := 1.0 - qab*x/qap;
  if abs(d) < Cfn_FPMIN then
    d := Cfn_FPMIN;
  d :=1.0/d;
  h :=d;
  for m:= 1 to Cfn_ITMAX do  // iterations
  begin
    m2 := 2*m;
    aa := m*(b-m)*x/((qam+m2)*(a+m2));
    d  := 1.0+aa*d;
    if abs(d) < Cfn_FPMIN then
      d := Cfn_FPMIN;
    c:=1.0+aa/c;
    if abs(c) < Cfn_FPMIN then
      c:= Cfn_FPMIN;
    d  :=1.0/d;
    h  := h*d*c;
    aa := -(a+m)*(qab+m)*x/((a+m2)*(qap+m2));
    d  := 1.0+aa*d;
    if abs(d) < Cfn_FPMIN then
      d:= Cfn_FPMIN;
    c := 1.0+aa/c;
    if abs(c) < Cfn_FPMIN then
      c:= Cfn_FPMIN;
    d   :=1.0/d;
    del :=d*c;
    h   := h*del;
    if abs(del-1.0) < Cfn_EPS then
      Break;
  end;
  if m > Cfn_ITMAX then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in BetaCfn function');
  Result := h;
end;

function Betai(a,b,x: Double): Double;
var
  bt: Double;
begin
  if (x < 0.0) or (x > 1.0) then
    RaiseErrorMessage(HC_Unexpected_Error, 'Error in Betai function');
  if (x = 0.0) or (x = 1.0) then
    bt :=0.0
  else
    bt := Exp(GammaLn(a+b)- GammaLn(a) - GammaLn(b)+ a*ln(x)+ b*ln(1.0-x));
  if (x < (a+1.0)/(a+b+2.0)) then
    Result := bt*BetaCfn(a,b,x)/a
  else
    Result := 1.0-bt*BetaCfn(b,a,1.0-x)/b;
end;

function DistanceFormula(point1: TPoint; point2: TPoint): Double;
begin
  Result := sqrt((point2.X - point1.X)*(point2.X - point1.X) + (point2.Y - point1.Y)*(point2.Y - point1.Y));
end;

function tTest(x: Double): Double;  // mean/stderr; one tailed result
begin
  Result := Betai(60,  0.5, 120/(120 + x*x)); // assumes 120 df to approximate normal
end;

function BinomialP(n, s: Double; p: Double): Double;
var
  sFrac: Double;
begin
  sFrac := s - Floor(s);
  n := Floor(n);
  s := Floor(s);
  // n=trials; s=successes; p=prob_success
  // Result := P(X >= k);
  if n = 0 then
    RaiseErrorMessage(HC_Unexpected_Error, 'Zero trials in BinomialP function');

  if n = 1 then  // this means just burnoulli trial
  begin
    if s = 0 then Result := 1  // Prob (X >= 0) = 1
    else          Result := p;
  end
  else if s = 0 then  // Prob (X >= 0) = 1
    Result := 1
  else if s = n then // all trials were successes
    Result := power(p,s)
  else
    Result := Betai(s, n-s+1, p);  // this is incomplete beta
  if sFrac > 0.0001 then
  begin
    Result := Result - (Result - Betai(s+1, n-(s+1)+1, p))*sFrac;
  end;
  if Result > 0.999999999 then
    Result := 0.999999999;
end;

function CoefficientOfVariation(Values: ArrayOfExtended): Extended;
var
  AStdDev: Extended;
  AMean: Extended;
  i: Integer;
begin
  AStdDev := 0.0;
  AMean := 0.0;
  Result := 0.0;
  if Length(Values) = 0 then
    Exit;
  {$IFDEF DEBUG}
  for i := 0 to Length(Values) - 1 do
    if Values[i] < 0 then
      Assert(False, 'Negative divergence time: ' + FloatToStr(Values[i]));
  {$ENDIF}
  try
     AMean := Mean(Values);
     AStdDev := CustomStdDev(Values);
  except
    on E:Exception do
    {$IFDEF VISUAL_BUILD}
       ShowMessage('On no! An error has occurred: ' + E.Message);
    {$ELSE}
       error_nv('An error has occurred: ' + E.Message + LineEnding + SysErrorMessage(GetLastOSError));
    {$ENDIF}
  end;
  if AMean > 0.000000000001 then
    Result := AStdDev / AMean;
end;

function ComputeBootstrapStdDev(trueBcl: Double; replicateBclVals: TArrayOfExt): Double;
var
  i: Integer = -1;
  aSum: Double = 0;
  aVariance: Double = 0;
  numVals: Integer = -1;
begin
  Assert(Length(replicateBclVals) = NUM_BCL_PRECISION_SAMPLES, Format('expected %d values but got %d', [NUM_BCL_PRECISION_SAMPLES, Length(replicateBclVals)]));
  Result := 0;
  numVals := Length(replicateBclVals);
  if Length(replicateBclVals) > 0 then
  begin
    for i := Low(replicateBclVals) to High(replicateBclVals) do
      aSum += Sqr((trueBcl - replicateBclVals[i]));
    aVariance := aSum/(numVals - 1);
    if CompareValue(aVariance, 0, FP_CUTOFF) = 0 then
      Result := 0
    else
      Result := Sqrt(aVariance);
  end;
end;

function ExpectedBclPrecision(aBcl: Double; numReplicates: Integer): Double;
begin
  if CompareValue(aBCL, 100, FP_CUTOFF) > 0 then
    raise Exception.Create(Format('bad BCL value = %.8f is > 100', [aBCL]));
  if numReplicates > 0 then
    Result := sqrt(aBcl*(100-aBCL)/numReplicates)
  else
    Result := 0;
end;

function CheckSupportValuesMakeSense(const trueBcl: Double; const replicateBclVals: TArrayOfExt; var aMedian: Double; var aMean: Double): Boolean;
var
  i: Integer = 0;
begin
  aMedian := -1;
  aMean := -1;
  Result := True;
  if CompareValue(trueBcl, 0, FP_CUTOFF) <= 0 then
  begin
    for i := Low(replicateBclVals) to High(replicateBclVals) do
    begin
      if CompareValue(replicateBclVals[i], 0, FP_CUTOFF) > 0 then
      begin
        Result := False;
        GetMeanAndMedian(replicateBclVals, aMean, aMedian);
        Exit;
      end;
    end;
  end
  else if CompareValue(trueBcl, 1, FP_CUTOFF) >= 0 then
  begin
    for i := Low(replicateBclVals) to High(replicateBclVals) do
    begin
      if CompareValue(replicateBclVals[i], 1, FP_CUTOFF) < 0 then
      begin
        Result := False;
        GetMeanAndMedian(replicateBclVals, aMean, aMedian);
        Exit;
      end;
    end;
  end;
end;

{
    Compare 2 values to see if they are close to each other within some tolerance
    relativeTol is a value between 0.0 and 1.0, e.g. for a relative tolerance of 5%, set relativeTol = 0.05
    absoluteTol is a value >= 0.0 and determines what small values should be considered close to zero

    absoluteTol needs to be carefully selected for the use case at hand. For values significantly smaller
    than 1, false positives can occur if absoluteTol is not set appropriately

    see
       https://docs.python.org/3/library/math.html#math.isclose
}
function ValuesAreClose(v1: Extended; v2: Extended; relativeTol: Extended = 1E-9; absoluteTol: Extended = 1E-8): Boolean;

begin
  Result := False;
  if IsNan(v1) or IsNan(v2) then
    Exit;
  if IsInfinite(v1) or IsInfinite(v2) then
    Exit;
  Result := (CompareValue(abs(v1 - v2), max(relativeTol*max(abs(v1), abs(v2)), absoluteTol), FP_CUTOFF) <= 0);
end;

function DebugIntArrayString(aIntArray: PArrayOfLongint; n: Integer): String;
var
  sb: TMegaStringBuilder = nil;
  i: Integer = 0;
begin
  Result := EmptyStr;
  try
    sb := TMegaStringBuilder.Create;
    for i := 0 to n - 1 do
    begin
      sb.Add(IntToStr(aIntArray^[i]));
      sb.Add('_');
    end;
    Result := sb.GenerateString;
  finally
    if Assigned(sb) then
      sb.Free;
  end;
end;

function MLInitTreeTypeToString(aType: Integer): String;
begin
  case aType of
    NJInitTreeMethod: Result := 'Neighbor-Joining';
    BioNJInitTreeMethod: Result := 'BIONJ';
    MEInitTreeMethod: Result := 'Minimum Evolution';
    MPInitTreeMethod: Result := 'Maximum Parsimony';
    DefaultInitTreeMethod: Result := 'Default (NJ/MP)';
    UserProvidedInitTree: Result := 'User Tree';
    MultipleMPTreesMethod: Result := 'Maximum Parsimony (multiple)';
    else
      Result := 'Undefined';
    end;
end;

function PointInRect(p: TPoint; r: TRect; inclusive: Boolean = True): Boolean;
begin
  if inclusive then
    Result := (p.X >= r.Left) and (p.X <= r.Right) and (p.Y >= r.Top) and (p.Y <= r.Bottom)
  else
    Result := (p.X > r.Left) and (p.X < r.Right) and (p.Y > r.Top) and (p.Y < r.Bottom);
end;

function ScalingFactor: Double;
begin
  Result := 1;
  {$IFDEF VISUAL_BUILD}
  if Screen.PixelsPerInch > MegaForm.DesignTimePPI then
    Result := Screen.PixelsPerInch/MegaForm.DesignTimePPI;
  {$ENDIF}
end;

function ValidPenStyle(aPenStyle: TPenStyle): TPenStyle;
begin
 case aPenStyle of
   psSolid: Result := psSolid;
   psDash: Result := psDash;
   psDot: Result := psDot;
   psDashDot: Result := psDashDot;
   psDashDotDot: Result := psDashDotDot;
   psinsideFrame: Result := psinsideFrame;
   psPattern: Result := psPattern;
   psClear: Result := psClear;
   else
      Result := psSolid;
 end;
end;

function ValidBrushStyle(aBrushStyle: TBrushStyle): TBrushStyle;
begin
 case aBrushStyle of
   bsSolid: Result := bsSolid;
   bsClear: Result := bsClear;
   bsHorizontal: Result := bsHorizontal;
   bsVertical: Result := bsVertical;
   bsFDiagonal: Result := bsFDiagonal;
   bsBDiagonal: Result := bsBDiagonal;
   bsCross: Result := bsCross;
   bsDiagCross: Result := bsDiagCross;
   bsImage: Result := bsImage;
   bsPattern: Result := bsPattern;
   else
      Result := bsClear;
 end;
end;

function ValidFontStyles(aFontStyle: TFontStyles): TFontStyles;
begin
 Result := [];
 if fsBold in aFontStyle then
   include(Result, fsBold);
 if fsItalic in aFontStyle then
   include(Result, fsItalic);
 if fsUnderline in aFontStyle then
   include(Result, fsUnderline);
 if fsStrikeout in aFontStyle then
   include(Result, fsStrikeout);
end;

function FindAvailableFontFromCssRule(css: String): String;
var
  fontsList: TStringList = nil;
  cssFonts: TStringList = nil;
  aFontName: String;
  i, j: Integer;
  fontFound: Boolean = False;
begin
  Result := EmptyStr;
  try
    fontsList := TStringList.Create;
    fontsList.AddStrings(Screen.Fonts);
    cssFonts := TStringList.Create;
    if SplitOnSingleCharFaster(css, ',', cssFonts) then
    begin
      if cssFonts.Count > 0 then
        for i := 0 to cssFonts.Count - 1 do
        begin
          if fontFound then
            break;
          aFontName := cssFonts[i];
          aFontName := StringReplace(aFontName, #39, '', [rfReplaceAll]);
          aFontName := StringReplace(aFontName, #34, '', [rfReplaceAll]);
          if fontsList.Count > 0 then
            for j := 0 to fontsList.Count - 1 do
            begin
              if SameText(aFontName, fontsList[j]) then
              begin
                Result := fontsList[j];
                fontFound := True;
                break;
              end;
            end;
        end;
    end;
  finally
    if Assigned(fontsList) then
      fontsList.Free;
    if Assigned(cssFonts) then
      cssFonts.Free;
  end;
end;

function UpdateFontFromCssString(var aFont: TFont; const css: String): Boolean;
var
  rules: TStringList = nil;
  tokens: TStringList = nil;
  aRule, aName: String;
  i: Integer;
  tempFont: TFont = nil;
begin
  try
   try
     rules := TStringList.Create;
     tokens := TStringList.Create;
     tempFont := TFont.Create;
     tempFont.Assign(aFont);
     tempFont.Style := [];

     Result := SplitOnSingleCharFaster(css, ';', rules);
     if Result then
     begin
       if rules.Count > 0 then
       begin
         for i := 0 to rules.Count - 1 do
         begin
           aRule := rules[i];
           aRule := StringReplace(aRule, ';', '', [rfReplaceAll]);
           Result := SplitOnSingleCharFaster(aRule, ':', tokens);
           if (not Result) or (tokens.Count <> 2) then
           begin
             if Result then
               Result := False;
             break;
           end;
           if SameText(tokens[0], 'font-family') then
           begin
             aName := FindAvailableFontFromCssRule(tokens[1]);
             if aName <> EmptyStr then
               tempFont.Name := aName;
           end
           else if SameText(tokens[0], 'font-size') then
           begin
             tempFont.Size := StrToInt(Copy(tokens[1], 1, Length(tokens[1]) - 2))
           end
           else if SameText(tokens[0], 'text-decoration') then
           begin
             if Pos('underline', tokens[1]) > 0 then
               tempFont.Style := tempFont.Style + [fsUnderline];
             if Pos('line-through', tokens[1]) > 0 then
               tempFont.Style := tempFont.Style + [fsStrikeOut];
           end
           else if SameText(tokens[0], 'font-style') then
           begin
             if Pos('italic', tokens[1]) > 0 then
               tempFont.Style := tempFont.Style + [fsItalic];
           end
           else if SameText(tokens[0], 'font-weight') then
           begin
             if Pos('bold', tokens[1]) > 0 then
               tempFont.Style := tempFont.Style + [fsBold];
           end;
         end;
       end;
     end;
     if Result then
       aFont.Assign(tempFont);
   except
     Result := False;
   end;
  finally
    if Assigned(rules) then
      rules.Free;
    if Assigned(tokens) then
      tokens.Free;
    if Assigned(tempFont) then
      tempFont.Free;
  end;
end;

function CssStringForFont(aFont: TFont): String;
begin
  //fontSettings := Format('"font-family: %s%s%s; font-size: %dpx; text-decoration: %s; line-height: 2Z00%; letter-spacing: 2px; font-style: %s;"', [#39, aFont.Name, #39, aFont.Size, CssStringForFont(aFont)])
  Result := Format('font-family: %s; font-size: %dpx;', [#39 + aFont.Name + #39, aFont.Size]);
  if (fsUnderline in aFont.Style) and (fsStrikeOut in aFont.Style) then
    Result := Result + ' text-decoration: underline line-through;'
  else if fsUnderline in aFont.Style then
    Result := Result + ' text-decoration: underline;'
  else if fsStrikeOut in aFont.Style then
    Result := Result + ' text-decoration: line-through;';
  if fsBold in aFont.Style then
    Result := Result + ' font-weight: bold;';
  if fsItalic in aFont.Style then
    Result := Result + 'font-style: italic;';
end;

function BoolToStrLC(aBool: Boolean; asString: Boolean = True): String;
begin
  Result := LowerCase(BoolToStr(aBool, True));
end;

function HtmlStringToTNodeMarkerShape(str: String): TNodeMarkerShape;
begin
  if SameText(str, 'msNone') then
    Result := msNone
  else if SameText(str, 'msOpenCircle') then
    Result := msOpenCircle
  else if SameText(str, 'msFilledCircle') then
    Result := msFilledCircle
  else if SameText(str, 'msOpenSquare') then
    Result := msOpenSquare
  else if SameText(str, 'msFilledSquare') then
    Result := msFilledSquare
  else if SameText(str, 'msOpenUpTriangle') then
    Result := msOpenUpTriangle
  else if SameText(str, 'msFilledUpTriangle') then
    Result := msFilledUpTriangle
  else if SameText(str, 'msOpenDownTriangle') then
    Result := msOpenDownTriangle
  else if SameText(str, 'msFilledDownTriangle') then
    Result := msFilledDownTriangle
  else if SameText(str, 'msOpenDiamond') then
    Result := msOpenDiamond
  else if SameText(str, 'msFilledDiamond') then
    Result := msFilledDiamond
  else
  begin
    Assert(False, 'invalid marker shape string');
    Result := msNone;
  end;
end;

function NodeMarkerShapeToHtmlString(shape: TNodeMarkerShape): String;
begin
  case Shape of
    msNone: Result := 'msNone';
    msOpenCircle: Result := 'msOpenCircle';
    msFilledCircle: Result := 'msFilledCircle';
    msOpenSquare: Result := 'msOpenSquare';
    msFilledSquare: Result := 'msFilledSquare';
    msOpenUpTriangle: Result := 'msOpenUpTriangle';
    msFilledUpTriangle: Result := 'msFilledUpTriangle';
    msOpenDownTriangle: Result := 'msOpenDownTriangle';
    msFilledDownTriangle: Result := 'msFilledDownTriangle';
    msOpenDiamond: Result := 'msOpenDiamond';
    msFilledDiamond: Result := 'msFilledDiamond';
    else
      begin
        Assert(False, 'invalid TNodeMarkerShape');
        Result := 'None';
      end;
  end;
end;

function MarkerToAnchorString(aShape: TNodeMarkerShape; aColor: TColor): String;
var
  aStr: String;
  rgbStr: String;
begin
  case aShape of
    msNone: Result := '&emsp;';
    else
      begin
        rgbStr := ColorToRgbCssString(aColor);
        aStr := NodeMarkerShapeToUnicodeString(aShape);
        Result := Format('<a href=%s#%s style=%scolor: %s%s>%s</a>', [#34, #34, #34, rgbStr, #34, aStr]);
      end;
  end;
end;

function NodeMarkerShapeToUnicodeString(shape: TNodeMarkerShape): String;
begin
  Result := '&emsp;';
  case shape of
    msNone: Result := '&emsp;';
    msOpenCircle: Result := '&#9675';
    msFilledCircle: Result := '&#9679';
    msOpenSquare: Result := '&#9633';
    msFilledSquare: Result := '&#9632';
    msOpenUpTriangle: Result := '&#9651';
    msFilledUpTriangle: Result := '&#9650';
    msOpenDownTriangle: Result := '&#96761';
    msFilledDownTriangle: Result := '&#9660';
    msOpenDiamond: Result := '&#9671';
    msFilledDiamond: Result := '&#9670';
  end;
end;

function SColorToHtmlColor(Color: TColor): string;
var
  N: Longint;
begin
  if Color=clNone then
  begin
    Result:= EmptyStr;
    exit
  end;
  N:= ColorToRGB(Color);
  Result:= '#' + IntToHex(Red(N), 2) + IntToHex(Green(N), 2) + IntToHex(Blue(N), 2);
end;

function SHtmlColorToColor(var s: string; out Len: integer; Default: TColor): TColor;
var
  N1, N2, N3: integer;
  i: integer;
begin
  Result:= Default;
  Len:= 0;
  if (s<>'') and (s[1]='#') then System.Delete(s, 1, 1);
  if (s='') then exit;

  //delete after first nonword char
  i:= 1;
  while (i<=Length(s)) and IsCharWord(s[i]) do Inc(i);
  System.Delete(s, i, Maxint);

  //allow only #rgb, #rrggbb
  Len:= Length(s);
  if (Len<>3) and (Len<>6) then exit;

  for i:= 1 to Len do
    if not IsCharHex(s[i]) then exit;

  if Len=6 then
  begin
    N1:= StrToInt('$'+Copy(s, 1, 2));
    N2:= StrToInt('$'+Copy(s, 3, 2));
    N3:= StrToInt('$'+Copy(s, 5, 2));
  end
  else
  begin
    N1:= StrToInt('$'+s[1]+s[1]);
    N2:= StrToInt('$'+s[2]+s[2]);
    N3:= StrToInt('$'+s[3]+s[3]);
  end;

  Result:= RGBToColor(N1, N2, N3);
end;

function ColorToRgbCssString(aColor: TColor): String;
var
  N: Longint;
begin
  if aColor=clNone then
  begin
    Result:= EmptyStr;
    exit
  end;
  N:= ColorToRGB(aColor);
  Result:= Format('rgb(%d, %d, %d);', [Red(N), Green(N), Blue(N)]);
end;

function IsCharWord(ch: char): boolean;
begin
  Result:= ch in ['a'..'z', 'A'..'Z', '_', '0'..'9'];
end;

function IsCharHex(ch: char): boolean;
begin
  Result:= ch in ['0'..'9', 'a'..'f', 'A'..'F'];
end;

function StringToBranchOption(str: String): TBranchOption;
begin
  if SameText(str, 'boFullBranch') then
    Result := boFullBranch
  else if SameText(str, 'boHalfBranch') then
    Result := boHalfBranch
  else if SameText(str, 'boNoBranch') then
    Result := boNoBranch
  else if SameText(str, 'boBranchOnly') then
    Result := boBranchOnly
  else
    raise Exception.Create('invalid branch option string');
end;

function StringToBracketStyle(str: String): TBracketStyle;
begin
  if SameText(str, 'brsSquare') then
    Result := brsSquare
  else if SameText(str, 'brsLine') then
    Result := brsLine
  else if SameText(str, 'brsNone') then
    Result := brsNone
  else
  begin
    Assert(False, 'invalid bracket style string');
    Result := brsNone;
  end;
end;

function BracketStyleToString(style: TBracketStyle): String;
begin
  case style of
    brsSquare: Result := 'brsSquare';
    brsLine: Result := 'brsLine';
    brsNone: Result := 'brsNone';
    else
    begin
      Assert(False, 'invalid bracket style');
      Result := 'brsNone';
    end;
  end;
end;

function BranchOptionToStr(option: TBranchOption): String;
begin
  Result := 'boFullBranch';
  case option of
    boFullBranch: Result := 'boFullBranch';
    boHalfBranch: Result := 'boHalfBranch';
    boNoBranch: Result := 'boNoBranch';
    boBranchOnly: Result := 'boBranchOnly';
  end;
end;

function StringToBrushStyle(str: String): TBrushStyle;
begin
  if SameText(str, 'bsSolid') then
    Result := bsSolid
  else if SameText(str, 'bsClear') then
    Result := bsClear
  else if SameText(str, 'bsHorizontal') then
    Result := bsHorizontal
  else if SameText(str, 'bsVertical') then
    Result := bsVertical
  else if SameText(str, 'bsFDiagonal') then
    Result := bsFDiagonal
  else if SameText(str, 'bsBDiagonal') then
    Result := bsBDiagonal
  else if SameText(str, 'bsCross') then
    Result := bsCross
  else if SameText(str, 'bsDiagCross') then
    Result := bsDiagCross
  else
  begin
    Assert(False, 'invalid brush style string');
    Result := bsClear;
  end;
end;

function BrushStyleToString(style: TBrushStyle): String;
begin
  case style of
    bsSolid: Result := 'bsSolid';
    bsClear: Result := 'bsClear';
    bsHorizontal: Result := 'bsHorizontal';
    bsVertical: Result := 'bsVertical';
    bsFDiagonal: Result := 'bsFDiagonal';
    bsBDiagonal: Result := 'bsBDiagonal';
    bsCross: Result := 'bsCross';
    bsDiagCross: Result := 'bsDiagCross';
    else
    begin
      Assert(False, 'invalid brush style');
      Result := 'bsClear';
    end;
  end;
end;

function GraphicAlignToString(aAlign: TGraphicAlign): String;
begin
  Result := EmptyStr;
  case aAlign of
    gaLeft: Result := 'gaLeft';
    gaRight: Result := 'gaRight';
    gaTop: Result := 'gaTop';
    gaBottom: Result := 'gaBottom';
  end;
end;

function StringToGraphicAlign(str: String): TGraphicAlign;
begin
  if SameText(str, 'gaLeft') then
    Result := gaLeft
  else if SameText(str, 'gaRight') then
    Result := gaRight
  else if SameText(str, 'gaTop') then
    Result := gaTop
  else if SameText(str, 'gaBottom') then
    Result := gaBottom
  else
  begin
    Assert(False, 'invalid graphic align string');
    Result := gaLeft;
  end;
end;

function HtmlLineTypeStrToPenStyle(str: String): TPenStyle;
begin
  Result := psSolid;
  if SameText(str, 'psSolid') then
    Result := psSolid
  else if SameText(str, 'psDash') then
    Result := psDash
  else if SameText(str, 'psDot') then
    Result := psDot
  else if SameText(str, 'psDashDot') then
    Result := psDashDot
  else if SameText(str, 'psDashDotDot') then
    Result := psDashDotDot
  else if SameText(str, 'psinsideFrame') then
    Result := psinsideFrame
  else if SameText(str, 'psPattern') then
    Result := psPattern
  else if SameText(str, 'psClear') then
    Result := psClear
  else
  begin
    Assert(False);
    Result := psSolid;
  end;
end;

function PenStyleToHtmlLineTypeStr(style: TPenStyle): String;
begin
  Result := EmptyStr;
  case style of
    psSolid: Result := 'psSolid';
    psDash: Result := 'psDash';
    psDot: Result := 'psDot';
    psDashDot: Result := 'psDashDot';
    psDashDotDot: Result := 'psDashDotDot';
    psinsideFrame: Result := 'psinsideFrame';
    psPattern: Result := 'psPattern';
    psClear: Result := 'psClear';
  end;
end;

function SessionIsSameTargetPlatform(const sessionTarget: LongInt; var msg: String
  ): Boolean;
var
  SizeOfExtended: Integer;
begin
  { Differences in implementation of floating point types across platforms
    can make MEGA session files incompatible between them.

    Previously we blocked use of MEGA session files on different platforms,
    but the only difference we need to focus on is the Extended type.
    MEGA session files should be compatible with all platforms using the same implementation.

    On platforms which support 80-bit extended floating type, the size of Extended is 10 bytes.
    This includes Intel CPUs on most platforms except WIN64, for which Microsoft deprecated support.

    On platforms lacking support, Extended is an alias for Double, 8 bytes big.
    This includes ARM CPUs like Apple Silicon (M1, etc.).
    These are now internally marked EXTENDED_UNSUPPORTED_TARGET.

    No other floating point types are a concern.
    Single, Comp, and Real might differ but we don't use them in session files.
    Doubles on all relevant platforms comply with the IEEE-754 standard.

    We mark session files with the corresponding platform unless the platform is
    unknown or is mismatched with the expected Extended size.  In these cases,
    we use either EXTENDED_SUPPORTED_TARGET or EXTENDED_UNSUPPORTED_TARGET. }

  SizeOfExtended := sizeof(Extended);
  if (SizeOfExtended = 8) then
    case sessionTarget of
      EXTENDED_UNSUPPORTED_TARGET, WIN64_TARGET: Result := True;
      EXTENDED_SUPPORTED_TARGET, WIN32_TARGET, MAC32_TARGET, MAC64_TARGET, LINUX32_TARGET, LINUX64_TARGET: Result := False;
    end
  else if (SizeOfExtended = 10) then
    case sessionTarget of
      EXTENDED_UNSUPPORTED_TARGET, WIN64_TARGET: Result := False;
      EXTENDED_SUPPORTED_TARGET, WIN32_TARGET, MAC32_TARGET, MAC64_TARGET, LINUX32_TARGET, LINUX64_TARGET: Result := True;
    end
  else
  begin
    msg := 'Incompatible session file - unknown extended floating point type';
    Result := False;
    Exit;
  end;

  if Result then
    Exit;
  msg := Format('Incompatible session file. The file was generated by a %s system but this is a %s system', [TargetPlatformString(sessionTarget), TargetPlatformString(VerifiedTargetPlatform)]);
end;

function VerifiedTargetPlatform: Integer;
var
  SizeOfExtended: Integer;
begin
  SizeOfExtended := SizeOf(Extended);

  { for backwards compatibility, we record the target platform in session files
    as long as they match the expected size for Extended.

    Otherwise, we mark either EXTENDED_SUPPORTED_TARGET or EXTENDED_UNSUPPORTED_TARGET. }

  if (SizeOfExtended = 8) then
    case TARGET_PLATFORM of
      EXTENDED_UNSUPPORTED_TARGET, WIN64_TARGET: Result := TARGET_PLATFORM;
      EXTENDED_SUPPORTED_TARGET, WIN32_TARGET, MAC32_TARGET, MAC64_TARGET, LINUX32_TARGET, LINUX64_TARGET: Result := EXTENDED_UNSUPPORTED_TARGET;
    else
      Result := EXTENDED_UNSUPPORTED_TARGET;
    end
  else if (SizeOfExtended = 10) then
    case TARGET_PLATFORM of
      EXTENDED_UNSUPPORTED_TARGET, WIN64_TARGET: Result := EXTENDED_SUPPORTED_TARGET;
      EXTENDED_SUPPORTED_TARGET, WIN32_TARGET, MAC32_TARGET, MAC64_TARGET, LINUX32_TARGET, LINUX64_TARGET: Result := TARGET_PLATFORM;
    else
      Result := EXTENDED_SUPPORTED_TARGET;
    end
  else
    raise Exception.Create('Unknown extended floating point type')
end;

function TargetPlatformString(const sessionTarget: LongInt): String;
begin
  case sessionTarget of
    WIN32_TARGET: Result := 'Windows 32-bit';
    WIN64_TARGET: Result := 'Windows 64-bit';
    MAC32_TARGET: Result := 'macOS 32-bit';
    MAC64_TARGET: Result := 'macOS Intel 64-bit';
    LINUX32_TARGET: Result := 'Linux 32-bit';
    LINUX64_TARGET: Result := 'Linux 64-bit';
    UNKNOWN_TARGET: Result := 'Unknown OS';
    EXTENDED_UNSUPPORTED_TARGET: Result := 'extended precision unsupporting';
    EXTENDED_SUPPORTED_TARGET: Result := 'extended precision supporting';
  end;
end;

function IsInRangeInclusive(testValue: Extended; rangeMin, rangeMax: Extended): Boolean;
begin
  Result := ((CompareValue(testValue, rangeMin, FP_CUTOFF) >= 0) and (CompareValue(testValue, rangeMax, FP_CUTOFF) <= 0));
end;

function CustomStdDev(const AData : ArrayOfExtended) : Extended;
var
  Variance: Extended;
  n: Integer;
begin
  n := Length(AData);
  if n <= 1 then
    Result := 0.0
  else
  begin
    Variance := CustomVariance(AData, n);
    if CompareValue(Variance, 0, FP_CUTOFF) <= 0 then
      Result := 0.0
    else
      Result := Sqrt(Variance);
  end;
end;

function CustomVariance(const AData: ArrayOfExtended; const n: Integer): Extended;
begin
  If n <= 1 then
    Result := 0.0
  else
    Result := CustomTotalVariance(AData,n)/(n-1);
end;

function CustomTotalVariance(const AData: ArrayOfExtended; const n: Integer
  ): Extended;
 var
   Sum: Extended = 0;
   SumOfSquares: Extended = 0;
begin
  If n <= 1 then
    Result := 0.0
  else
    begin
      CustomSumsAndSquares(AData, n, Sum, SumOfSquares);
      Result := SumOfSquares - Sqr(Sum) / n;
      if Result < 0.0 then
        Result := 0.0;
    end;
end;

procedure CustomSumsAndSquares(const AData: ArrayOfExtended; const n: Integer;
  var Sum, SumOfSquares: Extended);
var
  i : Integer;
  Temp : Extended;
begin
  SumOfSquares := 0.0;
  Sum := 0.0;
  for i := 0 to n - 1 do
   begin
    Temp := AData[i];
    SumOfSquares := SumOfSquares + Sqr(Temp);
    Sum := Sum + Temp;
   end;
end;

// a percentage in the range of 0.0 to 100.0
function PercentDifference(Value1, Value2: Double): Double;
begin
  if CompareValue(abs(Value1) + abs(Value2), 0, FP_CUTOFF) = 0 then
    Exit(0);
  Result := abs(Value1 - Value2) / ((abs(Value1) + abs(Value2)) / 2);
  Result := Result*100;
end;

// poisson random number with mean xm and seed idum
function PoiDev(xm: Double): Double;
var
  em,t,y: Double;
begin
  if xm < 12.0 then
  begin
    if xm <> PoiDev_oldm then
    begin
      PoiDev_oldm := xm;
      PoiDev_g    := exp(-xm);
    end;
    em :=  -1;
    t  := 1.0;
    repeat
      em := em + 1.0;
      t  := t*Ran1;
    until t <= PoiDev_g;
  end
  else
  begin
    if xm <> PoiDev_oldm then
    begin
      PoiDev_oldm := xm;
      PoiDev_sq   := sqrt(2.0*xm);
      PoiDev_alxm := ln(xm);
      PoiDev_g    := xm*PoiDev_alxm - GammaLn(xm+1.0);
    end;
    repeat
      repeat
        y  := tan(PI*ran1);
        em := PoiDev_sq*y + xm ;
      until em >= 0.0;
      em := floor(em);
      t  := 0.9*(1.0+ y*y)*exp(em*PoiDev_alxm-GammaLn(em+1.0) -PoiDev_g);
    until ran1 <= t;
  end;
  Result := em;
end;

function FisherExactTestOneTailed(Sd, S, Nd, N: Integer): Double;      // if input is not integer than fix it
var
  a, b, c, d: Integer;
  ConstSum, ASum: Double;
begin
  //              Subst.   NoSubst  Total
  //     Type
  //      Syn     Sd                 S
  //   Nonsyn     Nd                 N
  //              Sd+Nd              S+N
  a := Sd;     c := Nd;
  b := S -Sd;  d := N - Nd;
  Result := 0;

  ConstSum :=  FactLn(a+b)+ FactLn(c+d)+
               FactLn(b+d)+FactLn(a+c) - FactLn(a+b+c+d);
  while True do
  begin
    ASum := ConstSum - FactLn(a)-FactLn(b)-FactLn(c)-FactLn(d);
    Result := Result + exp(Asum);
    if (a=0) or (b=0) or (c=0) or (d=0) then
      break;
    if (a*d - b*c) < 0 then
    begin
      Dec(a); Dec(d); Inc(b); Inc(c);
    end
    else
    begin
      Dec(b); Dec(c); Inc(a); Inc(d);
    end
  end;
end;

function FisherExactTestTwoTailed(Sd, S, Nd, N: Integer): Double;      // if input is not integer than fix it
var
  a, b, c, d: Integer;
  StartingFirstTailProb, FirstTailprob, SecondTailProb,ConstSum, ASum: Double;
  ia, ib, ic, id: Integer;

  procedure GenerateNewABCD;
  var
    asumb,csumd,asumc,bsumd :Integer;
  begin
    asumb := ia+ib;
    csumd := ic+id;
    asumc := ia+ic;
    bsumd := ib+id;
    if (ia*id - ib*ic) < 0 then
    begin
      if ib < ic then  // set b to be zero
      begin
        b :=0;
        a := asumb;  // row with b is affected
        d := bsumd;  // col with b is affected
        c := csumd-d;  // d changed so row with d is affected
      end
      else // set c to be zero
      begin
        c :=0;
        d := csumd;  // row with c is affected
        a := asumc;  // col with a is affected
        b := asumb-a;  // a changed so row with b is affected
      end;
    end
    else
    begin
      if ia < id then  // set a to be zero
      begin
        a :=0;
        b := asumb;  // row with b is affected
        c := asumc;  // col with c is affected
        d := csumd-c;  // c changed so row with d is affected
      end
      else // set d to be zero
      begin
        d :=0;
        c := csumd;  // row with c is affected
        b := bsumd;  // col with b is affected
        a := asumb-b;  // b changed so a is affected
      end;
    end;
  end;

begin
  //              Subst.   NoSubst  Total
  //     Type
  //      Syn     Sd                 S
  //   Nonsyn     Nd                 N
  //              Sd+Nd              S+N
  a := Sd;     c := Nd;
  b := S -Sd;  d := N - Nd;

  ConstSum :=  FactLn(a+b)+ FactLn(c+d)+
               FactLn(b+d)+FactLn(a+c) - FactLn(a+b+c+d);

  StartingFirstTailProb := -1;
  // Do the first tail prob
  FirstTailProb := 0;
  while True do
  begin
    ASum := ConstSum - FactLn(a)-FactLn(b)-FactLn(c)-FactLn(d);
    FirstTailProb := FirstTailProb + exp(Asum);
    if StartingFirstTailProb < 0 then
      StartingFirstTailProb := exp(ASum);
    if (a=0) or (b=0) or (c=0) or (d=0) then
      break;
    if (a*d - b*c) < 0 then
    begin
      Dec(a); Dec(d); Inc(b); Inc(c);
    end
    else
    begin
      Dec(b); Dec(c); Inc(a); Inc(d);
    end
  end;

  // Do the second tail prob
  SecondTailProb := 0;
  ia := Sd;     ic := Nd;
  ib := S -Sd;  id := N - Nd;

  GenerateNewABCD;
  while True do
  begin
    ASum := ConstSum - FactLn(a)-FactLn(b)-FactLn(c)-FactLn(d);
    if exp(Asum) > StartingFirstTailProb then
      break;
    SecondTailProb := SecondTailProb + exp(Asum);
    if (ia*id - ib*ic) < 0 then // use original abcd's
    begin
      Dec(a); Dec(d); Inc(b); Inc(c);
    end
    else
    begin
      Dec(b); Dec(c); Inc(a); Inc(d);
    end;
    if (a = 0) or (b = 0) or (c = 0) or (d = 0) then
      break;
  end;
  Result := FirstTailProb+SecondTailProb;
end;

function CalculateCollapseClusterSizeIncrement(numOtus: Integer): Integer;
begin
  if numOtus <= 20 then
    Result := 1
  else if numOtus <= 50 then
    Result := 2
  else if numOtus <= 75 then
    Result := 5
  else if numOtus <= 100 then
    result := 10
  else if numOtus <= 500 then
    Result := 20
  else if numOtus <= 1000 then
    Result := 50
  else if numOtus <= 2000 then
    Result := 100
  else if numOtus <= 4000 then
    Result := 200
  else
    Result := 250;
end;

function CalculateCollapseClusterSizeInitialValue(numOtus: Integer): Integer;
begin
  if numOtus <= 10 then
    Result := 2
  else if numOtus <= 100 then
    Result := 10
  else if numOtus <= 500 then
    Result := 20
  else if numOtus <= 1000 then
    Result := 50
  else if numOtus <= 2000 then
    Result := 100
  else if numOtus <= 4000 then
    Result := 200
  else
    Result := 250;
end;

//=== Parsimony analysis stuff to Counts things
function CountBits(x: Longint): Longint;
begin
  Result := 0;
  while x <> 0 do
  begin
    if (x and 1) = 1 then
      Inc(Result);
    x := x shr 1;
  end;
end;

//=========
procedure SortIntArray(p : PArrayOfInt; L, R : integer);
var
  i,j,x,t : integer;
begin
    x := p[(L+R) div 2];
    i := L-1;
    j := R+1;
    repeat
        repeat Inc(i) until x <= p[i];
        repeat dec(j) until p[j] <= x;
        t := p[i]; p[i] := p[j]; p[j] := t;
    until i >= j;
    p[j] := p[i]; p[i] := t;
    if L < i-1 then SortIntArray(p, L, i-1);
    if j+1 < R then SortIntArray(p, J+1, R);
end;

procedure SortExtArray(p: TArrayOfExt; L, R: Integer);
var
  i,j : integer;
  x, t: Extended;
begin
  x := p[(L+R) div 2];
  i := L-1;
  j := R+1;
  repeat
    repeat
      Inc(i)
    until x <= p[i];

    repeat
      dec(j)
    until p[j] <= x;
    t := p[i];
    p[i] := p[j];
    p[j] := t;
  until i >= j;
  p[j] := p[i];
  p[i] := t;
  if L < i-1 then
    SortExtArray(p, L, i-1);
  if j+1 < R then
    SortExtArray(p, J+1, R);
end;

function GetStandardGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;


function GetVertebrateMtGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG';
end;


function GetInvertebrateMtGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSSSVVVVAAAADDEEGGGG';
end;

function GetYeastMtGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIMMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetMoldMtGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetCoelentrateMtGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetMycoplasmaGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetSpiroplasmaGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetCiliateNcGeneticCode: String;
begin
  Result := 'FFLLSSSSYYQQCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetPasycladaceanNcGeneticCode: String;
begin
  Result := 'FFLLSSSSYYQQCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetHexamitaNcGeneticCode: String;
begin
  Result := 'FFLLSSSSYYQQCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function  GetEchinodermGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNNKSSSSVVVVAAAADDEEGGGG';
end;

function GetEuplotidNcGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCCWLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetBacterialPlastidGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetPlantPlastidGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetAlternativeYeastMtGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CC*WLLLSPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;


function GetChlorophyceanMtGeneticCode: String;
begin
  Result := 'FFLLSSSSYY*LCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function GetAscidianMtGeneticCode: String;
begin
  Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSGGVVVVAAAADDEEGGGG';
end;

function GetFlatwormMtGeneticCode: String;
begin
  Result := 'FFLLSSSSYYY*CCWWLLLLPPPPHHQQRRRRIIIMTTTTNNNKSSSSVVVVAAAADDEEGGGG';
end;

function GetBlepharismaGeneticCode: String;
begin
  Result := 'FFLLSSSSYY*QCC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
end;

function NextAvailableFileNumber(const targetDir: String; defaultsNamesList: TStringList): Integer;
const
  MAX_TRIES = 1000;
var
  i: Integer;
  proposedNames: TStringList = nil;
  name: String;
  ext: String;

  function AtLeastOneProposedNameExists: Boolean;
  var
    j: Integer;
    fname: String;
  begin
    Result := False;
    if proposedNames.Count > 0 then
      for j := 0 to proposedNames.Count - 1 do
      begin
        fname := targetDir + PathDelim + proposedNames[j];
        if FileExists(fname) then
        begin
          Result := True;
          break;
        end;
      end;
  end;

begin
  Result := -1;
  if defaultsNamesList.Count = 0 then
    Exit;

  try
    proposedNames := TStringList.Create;
    proposedNames.AddStrings(defaultsNamesList);
    while AtLeastOneProposedNameExists and (Result <= MAX_TRIES) do
    begin
      inc(Result);
      proposedNames.Clear;
      for i := 0 to defaultsNamesList.Count - 1 do
      begin
        ext := ExtractFileExt(defaultsNamesList[i]);
        name := ExtractFileNameWithoutExt(defaultsNamesList[i]);
        proposedNames.Add(name + '-' + IntToStr(Result) + ext);
      end;
    end;
  finally
    if Assigned(proposedNames) then
      proposedNames.Free;
  end;
end;

function GetNoOfDefaultCodeTables: Integer;
var
  AList: TStringList;
begin
  AList := nil;
  try try
    AList := TStringList.Create;
    AList.LoadFromFile(GetPrivateFile(mfCodeTables));
    Result := AList.Count;
    if Result = 0 then
      raise Exception.Create('codetables files missing');
  except
    On E: Exception do
    begin
      Result := 4;
    end;
  end
  finally
    AList.Free;
  end;
end;

function GetDefaultCodeTableName(Index: Integer): String;
var
  AList: TStringList;
begin
  AList := nil;

  try try
    AList := TStringList.Create;
    AList.LoadFromFile(GetPrivateFile(mfCodeTables));
    if AList.Count = 0 then
      raise Exception.Create('Code Tables files missing');
    Result := AList.Names[Index];
  except
    On E: Exception do
    begin
      case Index of
        0: Result := 'Standard';
        1: Result :='Vertebrate_Mitochondrial';
        2: Result :='Yeast_Mitochondrial';
        3: Result :='Invertebrate_Mitochondrial';
      end;
    end;
  end
  finally
    AList.Free;
  end;
end;

function GetDefaultCodeTable(Index: Integer): String;
var
  AList: TStringList;
begin
  AList := nil;
  try try
    AList := TStringList.Create;
    AList.LoadFromFile(GetPrivateFile(mfCodeTables));
    if AList.Count = 0 then
      raise Exception.Create('Code Tables files missing');
    Result := AList.Values[AList.Names[Index]];
  except
    On E: Exception do
    begin
      case Index of
        0: Result := 'FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
        1: Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG';
        2: Result := 'FFLLSSSSYY**CCWWTTTTPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG';
        3: Result := 'FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSSSSVVVVAAAADDEEGGGG';
      end;
    end;
  end
  finally
    AList.Free;
  end;
end;



{$IFNDEF FPC}
function GetSystemPath(Folder: Integer): String;
var
  PIDL: PItemIDList;
  Path: Char;
  AMalloc: IMalloc;
begin
  //Path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Application.Handle, Folder, PIDL);
  if SHGetPathFromIDList(PIDL, PChar(Path)) then
    Result := Path;
  SHGetMalloc(AMalloc);
  AMalloc.Free(PIDL);
  //StrDispose(Path);    
end;
{$ENDIF}

//======= Used in MViewSeqDataForm.DrawCell (to set brush color), and in MViewStatSeqForm to export to Excel with colors
function GetCellColor(x: AnsiChar): TColor;  // Borrowed from MViewSetDataForm.DataGridDrawCell
  begin
    case Upcase(x) of
     'A'     : result := clYellow;
     'M'     : result := clYellow;
     'L'     : result := clYellow;
     'V'     : result := clYellow;
     'I'     : result := clYellow;
     'F'     : result := clYellow;

     'C'     : result := clOlive; // cystein
     'H'     : result := clTeal;  // histidine

     'E'     : result := clRed;
     'D'     : result := clRed;

     'K'     : result := clBlue;
     'R'     : result := clBlue;

     'G'     : result := clFuchsia;
     'P'     : result := clBlue;
     'Y'     : result := clLime;

     'T','U' : result := clGreen;
     'N'     : result := clGreen;
     'S'     : result := clGreen;
     'Q'     : result := clGreen;
     'W'     : result := clGreen;
     else
       result := clWhite;
    end;
  end;


//=================================
//  Added by K. T. for Z-test
//=================================

function zTest(x: Double): Double;  // mean/stderr; one tailed result
type
    TStatsTable = array[0..362] of double;

const
    value : TStatsTable =(
        0.00,  0.01,  0.02,  0.03,  0.04,  0.05,  0.06,  0.07,  0.08,  0.09,  0.10,
        0.11,  0.12,  0.13,  0.14,  0.15,  0.16,  0.17,  0.18,  0.19,  0.20,  0.21,
        0.22,  0.23,  0.24,  0.25,  0.26,  0.27,  0.28,  0.29,  0.30,  0.31,  0.32,
        0.33,  0.34,  0.35,  0.36,  0.37,  0.38,  0.39,  0.40,  0.41,  0.42,  0.43,
        0.44,  0.45,  0.46,  0.47,  0.48,  0.49,  0.50,  0.51,  0.52,  0.53,  0.54,
        0.55,  0.56,  0.57,  0.58,  0.59,  0.60,  0.61,  0.62,  0.63,  0.64,  0.65,
        0.66,  0.67,  0.68,  0.69,  0.70,  0.71,  0.72,  0.73,  0.74,  0.75,  0.76,
        0.77,  0.78,  0.79,  0.80,  0.81,  0.82,  0.83,  0.84,  0.85,  0.86,  0.87,
        0.88,  0.89,  0.90,  0.91,  0.92,  0.93,  0.94,  0.95,  0.96,  0.97,  0.98,
        0.99,  1.00,  1.01,  1.02,  1.03,  1.04,  1.05,  1.06,  1.07,  1.08,  1.09,
        1.10,  1.11,  1.12,  1.13,  1.14,  1.15,  1.16,  1.17,  1.18,  1.19,  1.20,
        1.21,  1.22,  1.23,  1.24,  1.25,  1.26,  1.27,  1.28,  1.29,  1.30,  1.31,
        1.32,  1.33,  1.34,  1.35,  1.36,  1.37,  1.38,  1.39,  1.40,  1.41,  1.42,
        1.43,  1.44,  1.45,  1.46,  1.47,  1.48,  1.49,  1.50,  1.51,  1.52,  1.53,
        1.54,  1.55,  1.56,  1.57,  1.58,  1.59,  1.60,  1.61,  1.62,  1.63,  1.64,
        1.65,  1.66,  1.67,  1.68,  1.69,  1.70,  1.71,  1.72,  1.73,  1.74,  1.75,
        1.76,  1.77,  1.78,  1.79,  1.80,  1.81,  1.82,  1.83,  1.84,  1.85,  1.86,
        1.87,  1.88,  1.89,  1.90,  1.91,  1.92,  1.93,  1.94,  1.95,  1.96,  1.97,
        1.98,  1.99,  2.00,  2.01,  2.02,  2.03,  2.04,  2.05,  2.06,  2.07,  2.08,
        2.09,  2.10,  2.11,  2.12,  2.13,  2.14,  2.15,  2.16,  2.17,  2.18,  2.19,
        2.20,  2.21,  2.22,  2.23,  2.24,  2.25,  2.26,  2.27,  2.28,  2.29,  2.30,
        2.31,  2.32,  2.33,  2.34,  2.35,  2.36,  2.37,  2.38,  2.39,  2.40,  2.41,
        2.42,  2.43,  2.44,  2.45,  2.46,  2.47,  2.48,  2.49,  2.50,  2.51,  2.52,
        2.53,  2.54,  2.55,  2.56,  2.57,  2.58,  2.59,  2.60,  2.61,  2.62,  2.63,
        2.64,  2.65,  2.66,  2.67,  2.68,  2.69,  2.70,  2.71,  2.72,  2.73,  2.74,
        2.75,  2.76,  2.77,  2.78,  2.79,  2.80,  2.81,  2.82,  2.83,  2.84,  2.85,
        2.86,  2.87,  2.88,  2.89,  2.90,  2.91,  2.92,  2.93,  2.94,  2.95,  2.96,
        2.97,  2.98,  2.99,  3.00,  3.01,  3.02,  3.03,  3.04,  3.05,  3.06,  3.07,
        3.08,  3.09,  3.10,  3.11,  3.12,  3.13,  3.14,  3.15,  3.16,  3.17,  3.18,
        3.19,  3.20,  3.21,  3.22,  3.23,  3.24,  3.25,  3.26,  3.27,  3.28,  3.29,
        3.30,  3.31,  3.32,  3.33,  3.34,  3.35,  3.36,  3.37,  3.38,  3.39,  3.40,
        3.41,  3.42,  3.43,  3.44,  3.45,  3.46,  3.47,  3.48,  3.49,  3.50,  3.51,
        3.52,  3.53,  3.54,  3.55,  3.56,  3.57,  3.58,  3.59,  3.60,  3.61,  3.62);

    prob : TStatsTable =(
        0.5000, 0.5040, 0.5080, 0.5120, 0.5160, 0.5199, 0.5239, 0.5279, 0.5319, 0.5359, 0.5398,
        0.5438, 0.5478, 0.5517, 0.5557, 0.5596, 0.5636, 0.5675, 0.5714, 0.5753, 0.5793, 0.5832,
        0.5871, 0.5910, 0.5948, 0.5987, 0.6026, 0.6064, 0.6103, 0.6141, 0.6179, 0.6217, 0.6255,
        0.6293, 0.6331, 0.6368, 0.6406, 0.6443, 0.6480, 0.6517, 0.6554, 0.6591, 0.6628, 0.6664,
        0.6700, 0.6736, 0.6772, 0.6808, 0.6844, 0.6879, 0.6915, 0.6950, 0.6985, 0.7019, 0.7054,
        0.7088, 0.7123, 0.7157, 0.7190, 0.7224, 0.7257, 0.7291, 0.7324, 0.7357, 0.7389, 0.7422,
        0.7454, 0.7486, 0.7517, 0.7549, 0.7580, 0.7611, 0.7642, 0.7673, 0.7704, 0.7738, 0.7764,
        0.7794, 0.7823, 0.7852, 0.7881, 0.7991, 0.7939, 0.7967, 0.7995, 0.8023, 0.8051, 0.8078,
        0.8106, 0.8133, 0.8159, 0.8186, 0.8212, 0.8238, 0.8264, 0.8289, 0.8315, 0.8340, 0.8365,
        0.8389, 0.8413, 0.8438, 0.8461, 0.8485, 0.8508, 0.8531, 0.8554, 0.8577, 0.8599, 0.8621,
        0.8643, 0.8665, 0.8686, 0.8708, 0.8729, 0.8749, 0.8770, 0.8790, 0.8810, 0.8830, 0.8849,
        0.8869, 0.8888, 0.8907, 0.8925, 0.8944, 0.8962, 0.8980, 0.8997, 0.9015, 0.9032, 0.9049,
        0.9066, 0.9082, 0.9099, 0.9115, 0.9131, 0.9147, 0.9162, 0.9177, 0.9192, 0.9207, 0.9222,
        0.9236, 0.9251, 0.9265, 0.9279, 0.9292, 0.9306, 0.9319, 0.9332, 0.9332, 0.9345, 0.9357,
        0.9370, 0.9382, 0.9406, 0.9418, 0.9429, 0.9441, 0.9452, 0.9463, 0.9474, 0.9484, 0.9495,
        0.9505, 0.9515, 0.9525, 0.9535, 0.9545, 0.9554, 0.9564, 0.9573, 0.9582, 0.9591, 0.9599,
        0.9608, 0.9616, 0.9625, 0.9633, 0.9641, 0.9649, 0.9656, 0.9664, 0.9671, 0.9678, 0.9686,
        0.9693, 0.9699, 0.9706, 0.9713, 0.9719, 0.9726, 0.9732, 0.9738, 0.9744, 0.9750, 0.9756,
        0.9761, 0.9767, 0.9772, 0.9778, 0.9783, 0.9788, 0.9793, 0.9798, 0.9803, 0.9808, 0.9812,
        0.9817, 0.9821, 0.9826, 0.9830, 0.9834, 0.9838, 0.9842, 0.9846, 0.9850, 0.9854, 0.9857,
        0.9861, 0.9864, 0.9868, 0.9871, 0.9875, 0.9878, 0.9881, 0.9884, 0.9887, 0.9890, 0.9893,
        0.9896, 0.9898, 0.9901, 0.9904, 0.9906, 0.9909, 0.9911, 0.9913, 0.9916, 0.9918, 0.9920,
        0.9922, 0.9925, 0.9927, 0.9929, 0.9931, 0.9932, 0.9934, 0.9936, 0.9938, 0.9940, 0.9941,
        0.9943, 0.9945, 0.9946, 0.9948, 0.9949, 0.9950, 0.9952, 0.9953, 0.9955, 0.9956, 0.9957,
        0.9959, 0.9960, 0.9961, 0.9962, 0.9963, 0.9964, 0.9965, 0.9967, 0.9967, 0.9968, 0.9969,
        0.9970, 0.9971, 0.9972, 0.9972, 0.9974, 0.9974, 0.9975, 0.9976, 0.9977, 0.9977, 0.9977,
        0.9978, 0.9979, 0.9980, 0.9980, 0.9981, 0.9981, 0.9982, 0.9983, 0.9983, 0.9984, 0.9985,
        0.9985, 0.9986, 0.9986, 0.9987, 0.9987, 0.9987, 0.9988, 0.9988, 0.9989, 0.9989, 0.9989,
        0.9990, 0.9990, 0.9990, 0.9990, 0.9991, 0.9991, 0.9992, 0.9992, 0.9992, 0.9992, 0.9993,
        0.9993, 0.9993, 0.9993, 0.9994, 0.9994, 0.9994, 0.9994, 0.9994, 0.9995, 0.9995, 0.9995,
        0.9995, 0.9995, 0.9996, 0.9996, 0.9996, 0.9996, 0.9996, 0.9996, 0.9996, 0.9997, 0.9997,
        0.9997, 0.9997, 0.9997, 0.9997, 0.9997, 0.9997, 0.9997, 0.9998, 0.9998, 0.9998, 0.9998,
        0.9998, 0.9998, 0.9998, 0.9998, 0.9998, 0.9998, 0.9998, 0.9998, 0.9998, 0.9998, 0.9999);
var
    i : integer;
begin
  if x < 0.0 then
    raise Exception.Create('Calculation error - test statistic cannot be negative');
    i := 0;
    while (i < 363) and  (value[i] <= x) do Inc(i);

      Dec(i);
    try
      Result := 1 -prob[i];
    Except on E: Exception do
      MessageDlg('Invalid Index i=' + IntToStr(i) + '.  ' + E.Message, mtError, [mbOK], 0);
    end;
end;

{$IFNDEF FPC}
function UnixToDateTime(Value: Longword; InUTC: Boolean): TDateTime;
var dwValue: LongWord;
     Days: LongWord;
     Hour: Word;
     Min: Word;
     Sec: Word;
     tz: TTimeZoneInformation;
begin

  // Get time zone information
  GetTimeZoneInformation(tz);

  // Offset by time zone
  if InUTC then
     // UTC time
     dwValue:=Value
  else
     // Local time to UTC
     dwValue:=LongWord(Integer(Value) - (tz.Bias * 60));

  // Decode days and time part
  Days:=dwValue div SecsPerDay;
  dwValue:=dwValue mod SecsPerDay;
  Hour:=dwValue div 3600;
  dwValue:=dwValue mod 3600;
  Min:=dwValue div 60;
  Sec:=dwValue mod 60;

  // Return encoded date time
  result:=EncodeDate(1970, 1, 1)+Days+EncodeTime(Hour, Min, Sec, 0);
end;

function FindVolumeSerial(const Drive : PAnsiChar) : AnsiString;
var
   VolumeSerialNumber : DWORD;
   MaximumComponentLength : DWORD;
   FileSystemFlags : DWORD;
   SerialNumber : AnsiString;
begin
   Result:='';

   GetVolumeInformationA(
        Drive,
        nil,
        0,
        @VolumeSerialNumber,
        MaximumComponentLength,
        FileSystemFlags,
        nil,
        0) ;
   SerialNumber :=
         IntToHex(HiWord(VolumeSerialNumber), 4) +
         ' - ' +
         IntToHex(LoWord(VolumeSerialNumber), 4) ;

   Result := SerialNumber;
end;

function GetSystemDrive: AnsiString;
begin
  SetLength(Result, MAX_PATH);
  if GetWindowsDirectoryA(PAnsiChar(Result), MAX_PATH) > 0 then
  begin
    SetLength(Result, StrLen(PAnsiChar(Result)));
    Result := ExtractFileDrive(Result);
  end else
    RaiseLastOSError;
end;
{$ENDIF}

function BuildNumberFormat(Precision: Integer): AnsiString;
var
 i: Integer;
begin
  result := '0';
  if Precision > 0 then
    Result := Result + '.';
  for i:= 1 to Precision do
  begin
    Result := Result + '0';
  end;
end;

function GetIEVersion: Integer;
var
  Reg: TRegistry;
  VerStr: AnsiString;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey('Software' + PathDelim + 'Microsoft' + PathDelim + 'Internet Explorer', False);
    try
      VerStr := Reg.ReadString('Version');
      setLength(VerStr, 1);
      result := StrToInt(VerStr);
    except
      on E:Exception do
      begin
        result := -1; // Returning -1 because the version is piror to 5
      end;
    end;
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

{$IFNDEF FPC}
function IsFileInUse(fName: String): Boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(fName) then exit;
  HFileRes := CreateFile(PChar(fName),GENERIC_READ or
    GENERIC_WRITE, 0 {this is the key}, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    FileClose(HFileRes); { *Converted from CloseHandle*  }
end;

function KillTask(ExeFileName: String): integer;
const
  PROCESS_TERMINATE=$0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  result := 0;

  FSnapshotHandle := CreateToolhelp32Snapshot
                     (TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := Sizeof(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle,
                                 FProcessEntry32);

  while integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
         UpperCase(ExeFileName))
     or (UpperCase(FProcessEntry32.szExeFile) =
         UpperCase(ExeFileName))) then
      Result := Integer(TerminateProcess(OpenProcess(
                        PROCESS_TERMINATE, BOOL(0),
                        FProcessEntry32.th32ProcessID), 0));
    ContinueLoop := Process32Next(FSnapshotHandle,
                                  FProcessEntry32);
  end;

  FileClose(FSnapshotHandle); { *Converted from CloseHandle*  }
end;
{$ENDIF}


{ TODO 1 -oDan -creltime : This needs to be fixed for a quoted label at the root. When
   there is a quoted label at the root, the counts for left and right parenthesis is
   thrown off somehow. }
function IsPerfectTreeFile(AFileName: String): Boolean;
var
  filebuffer: String;
  data      : TextFile;
  buffer    : String;

  function CheckFormat(buffer: String): boolean;
  var
//    numchar: set of AnsiChar;
    n1,n2,n3,i,n: integer;
    flag: boolean;
  begin
//    numchar := ['0'..'9'];
    result := true;
    n1 := 0;
    n2 := 0;
    n3 := 0;
    flag := true;
   n := length(buffer);
    i := 1;
      repeat
      if not flag then
      begin
        if buffer[i] = ']' then
          flag := true;
        inc(i);
        continue;
      end
      else if buffer[i] = '[' then
      begin
        flag := false;
        inc(i);
        continue;
      end;
      if flag and (buffer[i] = '''') then
      begin
        inc(i);
        repeat
          if buffer[i] = '''' then
            if (i < n-1) and (buffer[i+1] = '''') then
              inc(i,2)
            else
              break
          else
            inc(i);
        until i >= n;
        inc(i);
      end;

      if buffer[i] = ';' then
      begin
        if (n1 = 0) then
        begin
          result := false;
          break;
        end;
        if n1 <> n2 then
        begin
          result := false;
          break;
        end;
        n1 := 0;
        n2 := 0;
      end
      else if buffer[i] = '(' then
      begin
        n3 := 0;
        inc(n1);
      end
      else if buffer[i] = ')' then
      begin
        if n3 = 0 then
        begin
          result := false;
          break;
        end;
          inc(n2);
      end
      else if buffer[i] = ',' then
        inc(n3);

      inc(i);
    until i >= n;

    if n1 <> n2 then
      result := false;
  end;

begin
  try
    filebuffer := '';
    if FileExists(AFileName) then
    begin
      AssignFile(data, AFileName);
      Reset(data);

      while not Eof(data) do
      begin
        Readln(data, buffer);
        filebuffer := filebuffer +buffer;
      end;
    end
    else
    begin
      filebuffer := AFileName; // In this case it's not actually a filename, it's an actual tree
    end;
  finally
    if FileExists(AFileName) then
      CloseFile(data);
  end;
  result := false;
  // now we need to get rid of any extra white space which is allowed under the newick format, but would make this difficult to validate.
  StringReplace(filebuffer, ' ', '', [rfReplaceAll]);  // remove spaces
  StringReplace(filebuffer, AnsiChar(9), '', [rfReplaceAll]); // remove tabs
  StringReplace(filebuffer, AnsiChar(10), '', [rfReplaceAll]); // remove newlines (enter)
  StringReplace(filebuffer, AnsiChar(13), '', [rfReplaceAll]); // remove carriage return

  if Pos(';', filebuffer) = 0 then
  begin
    if (MessageBox(0, 'While reading in your newick tree file we noticed it did not terminate with a semicolon as the newick standard requires.'+LineEnding+'This could indicate a corrupted file, would you like to continue?', 'Semicolon Missing', MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON1) = idYes) then
    begin
      filebuffer := filebuffer + ';'; // just fix it for them so the parser doesn't freak.
    end
    else
    begin
      result := false;
      exit;
    end;
  end;
  Result := CheckFormat(filebuffer);
end;

function IsPerfectTreeFileWithInternalLabels(FileName: String): Boolean;
var
  FileBuffer: String;
  Data      : TextFile;
  Buffer    : String;

  function CheckFormat(NewickStr: String): boolean;
  var
    NumLeftParens: Integer;
    NumRightParens: Integer;
    NumCommas: Integer; // not the total number of commas, just so to check that there are at least two children for each node
    i: Integer;
    FinalCharIndex: integer;
    IsOutsideOfComment: boolean; // so we can skip past any comments (they are enclosed in [] brackets)
  begin
    Result := True;
    NumLeftParens := 0;
    NumRightParens := 0;
    NumCommas := 0;
    IsOutsideOfComment := true;
    FinalCharIndex := Length(NewickStr);
    i := 1;
    repeat
      if not IsOutsideOfComment then // skip comments
      begin
        if NewickStr[i] = ']' then
          IsOutSideOfComment := true;
        inc(i);
        continue;
      end
      else if NewickStr[i] = '[' then
      begin
        IsOutSideOfComment := false;
        inc(i);
        continue;
      end;

      if IsOutsideOfComment and (NewickStr[i] = '''') then // skip quoted labels
      begin
        inc(i);
        repeat
          if NewickStr[i] = '''' then
            if (i < FinalCharIndex - 1) and (NewickStr[i+1] = '''') then
              inc(i,2)
            else
              break
          else
            inc(i);
        until i = FinalCharIndex;
        inc(i);
      end;

      if NewickStr[i] = ';' then // encountered the end of a newick string, however there may be multiple newick strings
      begin
        if (NumLeftParens = 0) or (NumLeftParens <> NumRightParens) then
        begin
          Result := False;
          break;
        end;

        NumLeftParens := 0; // reset for any other trees that may be represented in this file
        NumRightParens := 0;
      end
      else if NewickStr[i] = '(' then
      begin
        NumCommas := 0;
        inc(NumLeftParens);
      end
      else if NewickStr[i] = ')' then
      begin
        if NumCommas = 0 then // then we encountered an empty pair of matching parens
        begin
          Result := False;
          break;
        end;
        inc(NumRightParens);
      end
      else if NewickStr[i] = ',' then
        inc(NumCommas);

      inc(i);
    until i >= FinalCharIndex;

    if NumLeftParens <> NumRightParens then
      Result := False;
  end;

begin
  try
    FileBuffer := '';
    if FileExists(FileName)  then
    begin
      AssignFile(Data, FileName);
      Reset(Data);

      while not Eof(Data) do
      begin
        Readln(Data, Buffer);
        Filebuffer := Filebuffer + Buffer;
      end;
    end
    else
    begin
      Filebuffer := FileName; // In this case it's not actually a filename, it's an actual tree
    end;
  finally
    if FileExists(FileName) then
      CloseFile(Data);
  end;

  Result := False;
  // now we need to get rid of any extra white space which is allowed under the newick format, but would make this difficult to validate.
  StringReplace(FileBuffer, ' ', '', [rfReplaceAll]);  // remove spaces
  StringReplace(FileBuffer, AnsiChar(9), '', [rfReplaceAll]); // remove tabs
  StringReplace(FileBuffer, AnsiChar(10), '', [rfReplaceAll]); // remove newlines (enter)
  StringReplace(FileBuffer, AnsiChar(13), '', [rfReplaceAll]); // remove carriage return

  if Pos(';', FileBuffer) = 0 then
    FileBuffer := FileBuffer + ';'; // just fix it for them so the parser doesn't freak.

  Result := CheckFormat(FileBuffer);
end;

function FloatToStrNP( val : extended) : AnsiString ;  // Nicks' strange custom function.  Try and figure out what this is doing and make it simpler.
var
  a : integer ;
  txt,retval : AnsiString ;
begin
  txt := FloatToStrF( val,ffFixed, 5,5) ;
  retval := '' ;
  for a:=Length(txt) downto 1 do
  begin
    if (txt[a]='0') and (retval <> '') then retval := txt[a]+retval
    else if txt[a]<>'0' then retval := txt[a]+retval ;
  end ;
  if Length(retval) = 2 then
    Result := retval[1]
  else
    Result := retval;
end;

function RegexReplaceAStringList(const theList: TStringList; const RegExpr: AnsiString; const ReplaceStr: AnsiString): TStringList; // lets you run a regex replace on a StringList without modifying the source stringlist.
var
  i: Integer;
  NamePortion, ValuePortion: AnsiString;
begin
  Result := TStringList.Create;
  for i:=0 to theList.Count-1 do
  begin
    NamePortion := theList.Names[i];
    if NamePortion <> EmptyStr then
      NamePortion := ReplaceRegExpr(RegExpr, theList.Names[i], ReplaceStr, False);
    if NamePortion = EmptyStr then
    begin
      Result.Add(ReplaceRegExpr(RegExpr, theList.Strings[i], ReplaceStr, False));
    end
    else
    begin
      ValuePortion := theList.Values[NamePortion];
      if ValuePortion <> EmptyStr then
        ValuePortion := ReplaceRegExpr(RegExpr, ValuePortion, ReplaceStr, False);
      Result.Add(NamePortion + '=' + ValuePortion);
    end;
  end;
end;

function StripGaps(Str: AnsiString): AnsiString;
begin
  result := ReplaceRegExpr('[-]', Str, '', False); // remove all '-' from the string.
end;

/// <summary>Make the name of our MUSCLE input file unique in case the user is
/// running multiple instances of MEGA in batch mode.</summary>
function GetMuscleInputFile: String;
begin
  Result := GetPrivateOutputFile('Private' + PathDelim + 'MUSCLE' + pathDelim + MuscleInput + IntToStr(GetCurrentProcessId) + '.fas');
end;

{$IFDEF FPC}

function ExcelExportToFileType(aType: TExportType): TOutputFileType;
begin
  Result := ExportNone;
  case aType of
  EXInvalid: Result := ExportInvalid;
  EXnone:  Result := ExportNone;
  EXtext, EXtextSave: Result := ExportText;
  EXcsvDisp, EXcsvSave: Result := ExportCSV;
  EXexcelDisp, EXexcelSave: Result := ExportExcel;
  EXexcelXmlDisp, EXexcelXmlSave: Result := ExportExelXML;
  EXfasta: Result := ExportFasta;
  EXodsSave, EXodsDisp: Result := ExportODS;
  end;
end;

function FileExtensionForExportType(aType: TExportType): String;
begin
  case aType of
    EXInvalid, EXnone: Result := EmptyStr;
    EXtext, EXtextSave: Result := '.txt';
    EXcsvDisp, EXcsvSave: Result := '.csv';
    EXexcelDisp, EXexcelSave: Result := '.xls';
    EXexcelXmlDisp, EXexcelXmlSave:Result := '.xlsx' ;
    EXfasta: Result := '.fas';
    EXodsDisp, EXodsSave: Result := '.ods';
  end;
end;

function FileExtensionForOutputFileType(aType: TOutputFileType): String;
begin
  case aType of
    ExportExelXML: Result := '.xlsx';
    ExportExcel: Result := '.xls';
    ExportODS: Result := '.ods';
    ExportCSV: Result := '.csv';
    ExportText: Result := '.txt';
    ExportMega: Result := '.meg';
    ExportFasta: Result := '.fas';
    ExportNone, ExportInvalid: Result := EmptyStr;
  end;
end;

function ExportIsWorkbookDisplay(aType: TExportType): Boolean;
begin
  Result := ((aType = ExexcelDisp) or
             (aType = EXexcelXmlDisp) or
             (aType = EXodsDisp) or
             (aType = EXcsvDisp));
end;

function ExportIsWorkbookSave(aType: TExportType): Boolean;
begin
  Result := ((aType = EXexcelSave) or
             (aType = EXexcelXmlSave) or
             (aType = EXodsSave) or
             (aType = EXcsvSave));
end;

function ExportIsExcel8(aType: TExportType): Boolean;
begin
  Result := ((aType = EXexcelDisp) or (aType = EXexcelSave));
end;

function ExportIsExcelXml(aType: TExportType): Boolean;
begin
  Result := ((aType = EXexcelXmlDisp) or (aType = EXexcelXmlSave));
end;

function ExportIsOds(aType: TExportType): Boolean;
begin
  Result := ((aType = EXodsDisp) or (aType = EXodsSave));
end;

function ExportIsCsv(aType: TExportType): Boolean;
begin
  Result := ((aType = EXcsvDisp) or (aType = EXcsvSave));
end;

function ExportIsText(aType: TExportType): Boolean;
begin
  Result := ((aType = EXtextSave) or (aType = EXtext));
end;

function ExportIsTextEditorDisplay(aType: TExportType): Boolean;
begin
  Result := ((aType = EXtext) or (aType = EXtextSave));
end;

function IsWorkbookFormat(aType: TOutputFileType): Boolean;
begin
  Result := False;
  case aType of
    ExportExelXML, ExportExcel, ExportODS, ExportCSV: Result := True;
  end;
end;

function StringToOutputFileType(aString: String): TOutputFileType;
begin
  if aString = MEF_MEGA then
    Result := ExportMega
  else if aString = MEF_FASTA then
    Result := ExportFasta
  else if aString = MEF_EXCEL_XML then
    Result := ExportExelXML
  else if aString = MEF_EXCEL then
    Result := ExportExcel
  else if aString = MEF_ODS then
    Result := ExportODS
  else if aString = MEF_CSV then
    Result := ExportCSV
  else if aString = MEF_TEXT then
    Result := ExportText
  else
    Result := ExportInvalid;
end;

function GetCurrentProcessId: DWord;
begin
  Result := System.GetProcessId;
end;
{$ENDIF}

function Base64ToStream(const ABase64: String; var AStream: TMemoryStream): Boolean;
var
  Str: String;
begin
  Result := False;
  if Length(Trim(ABase64)) = 0 then
    Exit;
  Str := DecodeStringBase64(ABase64);
  AStream.Write(Pointer(Str)^, Length(Str) div SizeOf(Char));
  AStream.Position := 0;
  Result := True;
end;

function GetGuidString: String;
var
  UserGuidFile: AnsiString;
  IdFile: TextFile;
  Guid: TGuid;
  IdString: AnsiString;
  TempList: TStringList;
begin
  Result := EmptyStr;
  TempList := nil;
  try
    try
      UserGuidFile := GetMegaGlobalFile('') + 'guid.txt';
      if not FileExists(UserGuidFile) then
      begin
        CreateGuid(Guid);
        IdString := GUIDToString(Guid);
        AssignFile(IdFile, UserGuidFile);
        Rewrite(IdFile);
        WriteLn(IdFile, IdString);
        CloseFile(IdFile);
        Result := IdString;
      end
      else
      begin
        TempList := TStringList.Create;
        TempList.LoadFromFile(UserGuidFile);
        if TempList.Count > 0 then
          Result := TempList[0];
      end;
    Except
      Result := EmptyStr;
    end;
  finally
    if TempList <> nil then
      FreeAndNil(TempList);
  end;
end;

/// <summary>Make the name of our MUSCLE output file unique in case the user is
/// running multiple instances of MEGA in batch mode.</summary>
function GetMuscleOutputFile: String;
begin
  Result := GetPrivateOutputFile('Private' + PathDelim + 'MUSCLE' + pathDelim + MuscleOutput + IntToStr(GetCurrentProcessId) + '.fas');
end;

function GetMuscleRunLog: String;
begin
  Result := GetPrivateOutputFile('Private' + PathDelim + 'MUSCLE' + pathDelim + MuscleRunLog + IntToStr(GetCurrentProcessId) + '.log');
end;

function GetMuscleExe: String;
begin
  {$IFDEF MSWINDOWS}
  Result := ExtractFilePath(Application.ExeName) +  MUSCLE_EXE;
    {$IFDEF DEBUG}
    if not FileExists(Result) then
    begin
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Missing MUSCLE executable. NOTE TO DEVELOPER - you need to copy the MEGA7_Install\MUSCLE\muscleWin64.exe file to your WIN64 or WIN32 directory');
      {$ELSE}
      error_nv('Missing MUSCLE executable. NOTE TO DEVELOPER - you need to copy the MEGA7_Install\MUSCLE\muscleWin64.exe file to your WIN64 or WIN32 directory');
      {$ENDIF}
    end;
    {$ENDIF}
  {$ELSE}
    {$IFDEF LINUX}
    Result := '/usr/lib/mega/' + MUSCLE_EXE;
    {$ELSE}
    Result := GetPrivateExecutableFile('Private' + PathDelim + 'MUSCLE' + PathDelim + MUSCLE_EXE);
    {$ENDIF}
  {$ENDIF}
end;


function GetHomeDirectory: String;
begin
  {$IFDEF MSWINDOWS}
  Result := GetWindowsSpecialDir(CSIDL_PERSONAL);
  {$ELSE}
  Result := GetUserDir;
  if DirectoryExists(Result + 'Documents') then
    Result := Result + 'Documents' + PathDelim;
  {$ENDIF}
end;

function StripSpaces(Str: AnsiString): AnsiString;
begin
  result := ReplaceRegExpr('[ ]', Str, '', false); // remove all spaces from the string.
end;

function GetPrivateExecutableFile(AValue: String; CopyIfNeeded: Boolean): String;
{$IFDEF UNIX}
var
  ExitStatus: LongInt;
  ACmd: AnsiString;
  IsExecutable: Boolean;
  Permissions: LongInt;
  StatBuf: TStat;
{$ENDIF}
begin
  Result := GetPrivateFile(AValue, CopyIfNeeded);
  {$IFDEF UNIX}
  if CopyIfNeeded then
  begin
    { More robust Unix executable permissions checking
      use of fpStat\fpChmod is more portable since fpSystem invokes bash, which isn't guaranteed
      we also want to chmod ONLY if executable bit is not set 
      previously this would fail if chmod failed, even if the file was actually executable }    
    IsExecutable := IsFileExecutable(Result);
    if (not IsExecutable) then
    begin
      if fpStat(Result, StatBuf) <> 0 then
         raise Exception.Create('Unable to retrieve file permissions for ' + Result);
      Permissions :=  StatBuf.st_mode;
      ExitStatus := fpChmod(Result, Permissions or S_IXUSR or S_IXGRP or S_IXOTH);
      if ExitStatus <> 0 then
      begin
        ExitStatus := wexitStatus(ExitStatus);
        raise Exception.Create('Unable to set executable file permissions for ' + Result);
      end;
    end;
  end;
  {$ENDIF}
end;

function IsFileExecutable(const FileName: string): Boolean;
var
  Permissions: Longint;
begin
  {$IFDEF UNIX}
  Permissions := fpAccess(FileName, X_OK);
  Result := (Permissions = 0);
  {$ENDIF}
end;

function NextAvailableFilename(DefaultName: String): String;
var
  Path: String = '';
  Ext: String = '';
  Name: String = '';
  ResFilename: String = '';
  i: Integer = 0;
  Found: Boolean = False;
begin
  Result := DefaultName;
  Path := ExtractFilePath(DefaultName);
  Ext := ExtractFileExt(DefaultName);
  Name := ExtractFileName(DefaultName);
  if DirectoryExists(Path) then
  begin
    ResFilename := DefaultName;
    i := 0;
    while not Found do
    begin
      if FileExists(ResFileName) or DirectoryExists(ResFileName) then
      begin
       ResFilename := Path + Copy(Name, 0, Length(Name) - Length(Ext)) + '-' + IntToStr(i) + Ext;  // changed ExtractFilePath(ResFilename) to path, since path never changes and we don't need to contstantly re-extract the FilePath which takes longer.
       inc(i);
      end
      else
      begin
        Result := ResFileName;
        Found := True;
      end;
    end;
  end
  else
    raise Exception.Create('Invalid file path provided to NextAvailableFileName().  Filename provided was: ' + DefaultName);
  if (Length(Result) > 0) and (Result[1] = '-') then
    Result[1] := '_';
end;

function NextAvailableFileNumber(DefaultName: String): String;
var
  Path: String;
  Ext: String;
  Name: String;
  ResFilename: String;
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  Path := ExtractFilePath(DefaultName);
  Ext := ExtractFileExt(DefaultName);
  Name := ExtractFileName(DefaultName);
  if DirectoryExists(Path) then
  begin
    ResFilename := DefaultName;
    i:=0;
    while not Found do
    begin
      if FileExists(ResFileName) then
      begin
        ResFilename := Path + Copy(Name, 0, Length(Name) - Length(Ext)) + IntToStr(i) + Ext;  // changed ExtractFilePath(ResFilename) to path, since path never changes and we don't need to contstantly re-extract the FilePath which takes longer.
        inc(i);
      end
      else
      begin
        if i = 0 then
          result := EmptyStr
        else
          result := IntToStr(i-1);
        Found := True;
      end;
    end;
  end
  else
    raise Exception.Create('Invalid file path provided to NextAvailableFileName().  Filename provided was: ' + DefaultName);
end;


function NewickCompatibleName(Name: AnsiString): AnsiString;
begin
  result := trim(Name);
  while Pos('(', result) > 0 do
    System.Delete(result, Pos('(', result), 1);
  while Pos(')', result) > 0 do
    System.Delete(result, Pos(')', result), 1);
  while Pos(':', result) > 0 do
    System.Delete(result, Pos(':', result), 1);
  while Pos(';', result) > 0 do
    System.Delete(result, Pos(';', result), 1);
  while Pos(' ', result) > 0 do
    result[Pos(' ', result)] := '_';
end;

procedure AddMaoFileHeader(var AList: TStringList; const FLStr: String);
begin
  AList.Insert(0, Format(FLStr, ['ver', VER_MEGA_BUILD + ' ' + MY_PLATFORM]));
  AList.Insert(0, '[ MEGAinfo ]');
  AList.Insert(0, '; Instead of modifying this file, simply create a new MEGA Analysis Options file by using MEGA.');
  AList.Insert(0, '; Please do not edit this file! If this file is modified, results are unpredictable.');
end;

function IsRelTimeAnalysis(Option: TDistTreeDlgOption): Boolean;
begin
  Result := (IsReltimeMLAnalysis(Option) or IsReltimeNonMLAnalysis(Option));
end;

function IsRelTimeAnalysis(ProcessPack: TProcessPack): Boolean;
begin
  Result := False;
  if ProcessPack.ContainsProcessType(ppRelTimeML) or
     ProcessPack.ContainsProcessType(ppRelTimeBLens) or
     ProcessPack.ContainsProcessType(ppRelTimeLS) or
     ProcessPack.ContainsProcessType(ppRtdtML) or
     ProcessPack.ContainsProcessType(ppRtdtBLens) or
     ProcessPack.ContainsProcessType(ppRtdtLS) or
     ProcessPack.ContainsProcessType(ppLbsTiming) or
     ProcessPack.ContainsProcessType(ppCorrTestML) or
     ProcessPack.ContainsProcessType(ppCorrTestBlens) then
  begin
    Result := True;
  end;
end;

procedure CalcCRC32(p: pointer; ByteCount: DWORD; var CRCValue: DWORD);
  // The following is a little cryptic (but executes very quickly).
  // The algorithm is as follows:
  // 1. exclusive-or the input byte with the low-order byte of
  // the CRC register to get an INDEX
  // 2. shift the CRC register eight bits to the right
  // 3. exclusive-or the CRC register with the contents of Table[INDEX]
  // 4. repeat steps 1 through 3 for all bytes
VAR
  i:  DWORD;
  q: ^BYTE;
BEGIN
  q := p;
  FOR i := 0 TO ByteCount-1 DO
  BEGIN
    CRCvalue := (CRCvalue SHR 8) XOR
    Table[ q^ XOR (CRCvalue AND $000000FF) ];
    INC(q)
  END
END;
{$IFDEF VISUAL_BUILD}
function MessageDlgCheck(aParent: TCustomForm; const Msg, ACaption, ACheckCaption: AnsiString; var AChecked : boolean; DlgType: TMsgDlgType = mtInformation; Buttons: TMsgDlgButtons = [mbOK]; HelpCtx: Longint = 0): Integer;
begin
  if mbYes in Buttons then
    MegaCustomMsgDlg.UsingYesNoButtons := True
  else
    MegaCustomMsgDlg.UsingYesNoButtons := False;
  if Msg = CONFIRM_CLOSE_MEGA_STRING then
    MegaCustomMsgDlg.SetToUseCloseMegaButtons(True)
  else
    MegaCustomMsgDlg.SetToUseCloseMegaButtons(False);
  {$IFNDEF DARWIN}
  if Assigned(aParent) then
    MegaCustomMsgDlg.PopupParent := aParent;
  {$ENDIF}
  Result := MegaCustomMsgDlg.DoCustomShowModal(ACaption, ACheckCaption, Msg, HelpCtx, AChecked);
end;

{$ENDIF}

function SubSetOptionsToStringList(options: TDataSubsetOptions): TStringList;
var
  x: dsoDataSubsetOption;
  temp: String;
begin
  Result := TStringList.Create;
  for x in options do
  begin
    temp := GetEnumName(TypeInfo(dsoDataSubsetOption), Integer(x));
    Result.Add(temp);
  end;
end;

function SubSetOptionsToString(options: TDataSubsetOptions): String;
var
  aList: TStringList = nil;
begin
  try
    aList := SubSetOptionsToStringList(options);
    Result := aList.Text;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function validEmail(email: AnsiString): boolean;
var
  reg: TRegExpr;
begin
  result := false;
  reg := TRegExpr.Create;
  reg.Expression := '.+@.+\..+';
  if reg.Exec(email) then
    result := true;
end;

function ResourceTypeNew(Filename: String): String;
begin
  Result := UpperCase(ExtractFileExt(Filename));
  while Pos('.', Result) > 0 do
    Result := Copy(Result, Pos('.', Result) + 1, Length(Result));
end;

function GetNoOfProcessors: integer;
var
  {$IFDEF MSWINDOWS}
  SysInfo: TSystemInfo;
  {$ENDIF}
  {$IFDEF UNIX}
  AList: TStringList = nil;
  AFile: String = '';
  i: Integer;
  status: Integer;
  len: size_t;
  {$ENDIF}
begin
  Result := -1;
  {$IFDEF MSWINDOWS}
  try
     GetSystemInfo(SysInfo);
     result := SysInfo.dwNumberOfProcessors;
  except
    Result := -1;
  end;
  {$ELSE}
     {$IFDEF UNIX}
        {$IFDEF DARWIN}
          len := SizeOf(Result);
          status := fpSysCtlByName('hw.ncpu', @Result, @len, Nil, 0);
          if status <> 0 then RaiseLastOSError;
        {$ELSE}
           try
            try
              AList := TStringList.Create;
              AFile := '/proc/cpuinfo';
              if FileExists(AFile) then
              begin
                AList.LoadFromFile(AFile);
                if AList.Count > 0 then
                begin
                  Result := 0;
                  for i := 0 to AList.Count - 1 do
                    if Trim(AList[i]) = EmptyStr then
                      inc(Result);
                  if Result = 0 then { then it did not work as expected}
                    Result := -1;
                end;
              end;
            except
              Result := -1;
            end;
           finally
             if Assigned(AList) then
               AList.Free;
           end;
        {$ENDIF}
     {$ENDIF}
  {$ENDIF}
end;

function GetDefaultNoOfProcessors(curOperation: TDistTreeDlgOption): Integer;
begin
  Result := GetNoOfProcessors - 1;
  case curOperation of
    dtdoMLClockTest, dtdoMLClockTestLocal, dtdoMLTree, dtdoRelTimeML, dtdoMLInferAncSeqMyPeg, dtdoCorrTestML, dtdoEpML, dtdoRtdtML, dtdoBEAM:
      begin
        Result := min(Result, 4);
      end;
    dtdoMLInferAncSeq, dtdoMLPredictLivingSeq, dtdoMLComputePattern, dtdoMLTsTvBias, dtdoMLGammRates, dtdoMLInferSiteBySiteRates, dtdoMLComputeUserTreeBLens:
      begin
        Result := min(Result, 4);
      end;
    dtdoMLModelTest, dtdoMLModelTamer:
      begin
        if D_InputSeqData.IsAmino or D_InputSeqData.IsCoding then
          Result :=  min(Result, 8)
        else
          Result := min(Result, 6);
      end;
    dtdoNJTree, dtdoMETree, dtdoMPTree, dtdoUPGMATree:
      begin
        Result := min(Result, 8);
      end;
  end;
end;

function GetOperatingSystem: String;
begin
   {$IFDEF MSWINDOWS}
  Result := 'Windows';
  {$ENDIF}
  {$IFDEF UNIX}
     {$IFDEF DARWIN}
     Result := 'macOS';
     {$ELSE}
     Result := 'Linux';
     {$ENDIF}
  {$ENDIF}
end;

function GetTechnicalInfoJson: TJSONObject;
var
  tempInt: Int64 =  -1;
  tempStr: String = '';
begin
  Result := TJSONObject.Create;
  tempStr := GetGuidString;
  Result.Add('GUID', tempStr);
  Result.Add('User Type', USER_TYPE);
  Result.Add(VERSION_STR, VER_MEGA_MAJOR_CHAR + VER_MEGA_MINOR_STR);
  Result.Add(BUILD_STR, VER_MEGA_BUILD);
  Result.Add(INTERFACE_STR, USER_INTERFACE);
  Result.Add(OS_PLATFORM_STR, GetOperatingSystem);
  tempInt := GetNoOfProcessors;
  Result.Add('No. of Cores', tempInt);
  if GetAvailablePhysicalMemory(tempInt) then
    Result.Add('Available RAM  (MB)', Round(tempInt/1024/1024))
  else
    Result.Add('Available RAM (MB)', 'n/a');
end;

function CheckStopCodons(SeqList: TSequenceList; var Row: Integer; var Col: Integer): Boolean;
var
  i, j: Integer;
begin
  row := -1;
  col := -1;
  result := false;
  for i := 0 to SeqList.Count-1 do
  begin
    for j := 1 to Length(SeqList.Items[i].SeqData)-2 do // I'm checking the length -2 because we don't need to check the very last AA.  This is because it IS ALLOWED to be a stop codon '*'.  We just don't want any AA after the * in the current sequence.
      if SeqList.Items[i].SeqData[j]  = '*' then
      begin
          Row := i+1;
          Col := j;
          result := true;
	  Exit;
      end;
  end;
end;

function MapTokenCodeToDataTypeString(TokenCode: TSnTokenCode): String;
begin
  case TokenCode of
    snNucleotide:
      Result := NucleotidePickStr;
    snCoding:
      Result := NucleotideCodingPickStr;
    snProtein:
      Result := AminoAcidPickStr;
    snDistance:
      Result := DistanceMatrixPickStr;
    else
      Result := EmptyStr;
  end;
end;

function MapTSnTokenCodeStringToTokenCode(TokenCodeString: String):TSnTokenCode;
begin
  if TokenCodeString = 'snNucleotide' then
    Result := snNucleotide
  else if TokenCodeString = 'snCoding' then
    Result := snCoding
  else if TokenCodeString = 'snProtein' then
    Result := snProtein
  else if TokenCodeString = 'snDistance' then
    Result := snDistance
  else
    Result := snNoToken;
end;

function MapDataTypeStringToTokenCode(DataTypeString: String):TSnTokenCode;
begin
  if (DataTypeString = NucleotidePickStr) or
     (DataTypeString = NucleotideCodingPickStr) or
     (LowerCase(DataTypeString) = 'dna') or
     (LowerCase(DataTypeString) = 'rna') or
     (LowerCase(DataTypeString) = 'nucleotide') or
     (LowerCase(DataTypeString) = 'snnucleotide') then
    Result := snNucleotide
  else if (DataTypeString = AminoAcidPickStr) or
          (LowerCase(DataTypeString) = 'protein') or
          (LowerCase(DataTypeString) ='snprotein') then
    Result := snProtein
  else if (DataTypeString = DistanceMatrixPickStr) or
          (LowerCase(DataTypeString) = 'distance') or
          (LowerCase(DataTypeString) = 'sndistance')then
    Result := snDistance
  else
    Result := snNoToken;
end;

function endsWith(FullString: String; EndString:String):Boolean;
var
  EndStringLength: Integer;
  FullStringLength: Integer;
  FullStringEnd: String;
begin
  EndStringLength := Length(EndString);
  FullStringLength := Length(FullString);
  FullStringEnd := Copy(FullString, FullStringLength - EndStringLength + 1, EndStringLength);
  Result := (EndString = FullStringEnd);
end;

function FileToString(FileName: string; ProcessMessages: boolean = false): string;
const BUFFER_SIZE = 256 * 1024;
var Data: string;
    Temp: string;
    LastRead: integer;
begin
 result := '';
 Data := '';

 if not FileExists(FileName) then
  exit;

 with TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone) do
 begin
  SetLength(Temp, BUFFER_SIZE);
  LastRead := Read(Temp[1], BUFFER_SIZE);
  while LastRead <> 0 do
  begin
   if ProcessMessages then Application.ProcessMessages();

   SetLength(Temp, LastRead);
   Data := Data + Temp;
   SetLength(Temp, BUFFER_SIZE);
   LastRead := Read(Temp[1], BUFFER_SIZE);
  end;

  Free();
 end;

 result := Data;
end;

{$IFDEF FPC}
function downloadURL(urlToDownload: string; saveFilename: String): string;
var
  S : String;
  aList: TStringList = nil;
begin
  with TFPHttpClient.Create(Nil) do
    try
      aList := TStringList.Create;
      S:=Get(UrlToDownload);
      aList.Text := S;
      aList.SaveToFile(SaveFilename);
      Result := SaveFilename;
    finally
      aList.Free;
      Free;
    end;
end;
{$ELSE}
function downloadURL(urlToDownload: string; saveFilename: string): string;
begin
  result := '';
  with TDownloadURL.Create(nil) do
  begin
    try
      URL := urlToDownload;
      //if not CheckURL(urlToDownload) then
      //  Exit;
      Filename := saveFilename;
      ExecuteTarget(nil);
      if FileExists(Filename) then
        result := filename;
    finally
      Free;
    end;
  end;
end;
{$ENDIF}

procedure FindAllFilesNotFolders(Path: String; Attr: Integer; List: TStringList; recursive: Boolean);
var
  {$IFNDEF VISUAL_BUILD}
  foldername: String = '';
  {$ENDIF}
   Res: TSearchRec;
   ARes: QWord;
   EOFound: Boolean;
   i: Integer;
   foundDirs: TStringList;
   starPath : String;
begin
  ARes := 0;
  if (path[length(path)] <> PathDelim) and (path[length(path)] <> PathDelim) then
    path := path + PathDelim;
  starPath := path + '*';

  {$IFNDEF VISUAL_BUILD}
  folderName := Copy(ExtractFileDir(path), LastDelimiter(PathDelim, ExtractFileDir(path))+1, length(path));
  if AnsiCompareText(folderName, 'M' + VER_MEGA_MAJOR_CHAR + 'CC_Out') = 0  then
    Warn_nv('The output folder named M' + VER_MEGA_MAJOR_CHAR + 'CC_Out was included in the directory you specified!');
  {$ENDIF}

  foundDirs := TStringList.Create;
  EOFound:= False;
  if FindFirst(starPath,Attr,Res) < 0 then
    exit
  else
    while not EOFound do begin
    if (Res.Name <> '.') and (Res.Name <> '..') then
    begin
      if (ExtractFileExt(Res.Name) = EmptyStr) and DirectoryExists(path + Res.Name) then
        foundDirs.Add(Res.Name)
      else
        List.Add(path + Res.Name);
    end;
      EOFound:= FindNext(Res) <> 0;
    end;
  {$IFDEF MSWINDOWS}
  FindClose(ARes); { TODO 1 -oglen -clazarus : fix this, after putting jwawinbase, jwawinnt, and jwapsapi in the uses clause, compiler got confused and expected a qword here }
  {$ELSE}
  FindClose(Res);
  {$ENDIF}
  if recursive then
    for i := 0 to foundDirs.Count-1 do
      FindAllFilesNotFolders(path + foundDirs.Strings[i] + PathDelim, Attr, List, recursive);
  foundDirs.Free;
end;

function fileExtValidForAnalysis(ext: String): boolean;
begin
  result := (AnsiIndexText(ext, [FastaExt1, FastaExt2, FastaExt3, FastaExt4, FastaExt5, MegaExt1, MegaExt2, MegaExt3, MasExt1]) > -1);
end;

function fileExtValidForTree(ext: String): boolean;
begin
  result := (AnsiIndexText(ext, [NewickExt1, NewickExt2, NewickExt3, NewickExt4, NewickExt5, NewickExt6]) > -1);
end;

function TwoDArrayOfExtendedToList(Data: T2DArrayOfExtended; Caption: String): TStringList;
var
  aRow, aCol: Integer;
  AStr: String;
begin
  Result := TStringList.Create;
  if Length(Data) = 0 then
    Exit;

  try
    Result.Add(Caption);
    for aRow := 0 to Length(Data) - 1 do
    begin
      AStr := Format('%4d', [aRow]) + #9;
      for aCol := 0 to Length(Data[aRow]) - 1 do
        AStr := AStr + Format('%8.8f', [Data[aRow][aCol]]) + #9;
      Result.Add(Trim(AStr));
    end;
  except
    on E:Exception do
    {$IFDEF VISUAL_BUILD}
    ShowMessage('Failed to export 2D array of double: ' + E.Message);
    {$ELSE}
    warn_nv('Failed to export 2D array of double: ' + E.Message);
    {$ENDIF}
  end;
end;

function OneDArrayOfExtendedToFile(Data: ArrayOfExtended; Filename: String; Caption: String): Boolean;
var
  i: Integer;
  AList: TStringList;
begin
  Result := False;
  AList := nil;
  if Length(Data) = 0 then
    Exit;

  try
    try
      AList := TStringList.Create;
      AList.Add(Caption);
      for i := 0 to Length(Data) - 1 do
        AList.Add(Format('%8.4f', [Data[i]]));
      AList.SaveToFile(Filename);
      Result := FileExists(Filename);
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Failed to export array of double' + E.Message);
      {$ELSE}
      warn_nv('Failed to export array of double' + E.Message);
      {$ENDIF}
    end;
  finally
    if Assigned(AList) then
      AList.Free;
  end;
end;

function ConcatenateDivTimeArrays(const minTimes: TDivTimesArray;const maxTimes: TDivTimesArray): TDivTimesArray;
var
  i: Integer;
  index: Integer = 0;
begin
  SetLength(Result, Length(minTimes) + Length(maxTimes));
  if Length(minTimes) > 0 then
    for i := Low(minTimes) to High(minTimes) do
    begin
      Result[index] := minTimes[i];
      inc(index);
    end;
  if Length(maxTimes) > 0 then
    for i := Low(maxTimes) to High(maxTimes) do
    begin
      Result[index] := maxTimes[i];
      inc(index);
    end;
end;

{$IFDEF DEBUG}
function DivTimeArraysToDebugFile(minTimes, maxTimes: TDivTimesArray;filename: String): Boolean;
var
  f: TextFile;
  i: Integer = -1;
begin
  Assert((Length(MinTimes) > 0) and (Length(MinTimes) = Length(MaxTimes)));
  try
    AssignFile(f, filename);
    Rewrite(f);
    WriteLn(f, Format('%12s %12s', ['MinTime', 'MaxTime']));
    for i := Low(MinTimes) to High(MinTimes) do
      WriteLn(f, Format('%12.2f %12.2f', [MinTimes[i], MaxTimes[i]]));
  finally
    CloseFile(f);
  end;
  Result := FileExists(filename);
end;
{$ENDIF}

procedure FindMinAndMaxTimes(const allTimes: TDivTImesArray; var minTime: Extended; var MaxTime: Extended);
var
  i: Integer;
begin
  if Length(allTimes) > 0 then
  begin
    minTime := allTimes[0];
    maxTime := allTimes[0];
    for i := Low(allTimes) to High(allTimes) do
    begin
      if allTimes[i] < minTime then
        minTime := allTimes[i];
      if allTimes[i] > maxTime then
        maxTime := allTimes[i];
    end;
  end;
end;

function ArrayOfIntToList(data: ArrayOfInteger): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if Length(data) > 0 then
    for i := 0 to Length(data) - 1 do
      Result.Add(IntToStr(data[i]));
end;

function ArrayOfIntToFile(data: ArrayOfInteger; filename: String): Boolean;
var
  aList: TStringList = nil;
begin
  Result := False;
  try
   try
     aList := ArrayOfIntToList(data);
     aList.SaveToFile(filename);
     Result := FileExists(filename);
   except
     on E:Exception do
     begin
     {$IFDEF VISUAL_BUILD}
     ShowMessage('Failed to export array of integer: ' + E.Message);
     {$ELSE}
     warn_nv('Failed to export array of integer: ' + E.Message);
     {$ENDIF}
     end;
   end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function ArrayOfDoubleToFile(data: ArrayOfDouble; filename: String): Boolean;
var
  f: TextFile;
  i: Integer = -1;
begin
  try
    AssignFile(f, filename);
    Rewrite(f);
    if Length(data) > 0 then
      for i := Low(data) to High(data) do
        Write(f, Format('%.4f,', [data[i]]));
    WriteLn(f);
  finally
    CloseFile(f);
  end;
  Result := FileExists(filename);
end;

function PArrayOfIntToList(data: PArrayOfInt; n: LongInt): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  if n > 0 then
    for i := 0 to n - 1 do
      Result.Add(IntToStr(data[i]));
end;

function PArrayOfIntToFile(data: PArrayOfInt; filename: String; n: LongInt): Boolean;
var
  aList: TStringList = nil;
begin
  Result := False;
  try
   try
     aList := PArrayOfIntToList(data, n);
     aList.SaveToFile(filename);
     Result := FileExists(filename);
   except
     on E:Exception do
     begin
     {$IFDEF VISUAL_BUILD}
     ShowMessage('Failed to export array of integer: ' + E.Message);
     {$ELSE}
     warn_nv('Failed to export array of integer: ' + E.Message);
     {$ENDIF}
     end;
   end;
  finally
    if Assigned(aList) then
      aList.Free;
  end;
end;

function TwoDArrayOfExtendedToFile(Data: T2dArrayOfExtended; Filename: String; Caption: String): Boolean;
var
  AList: TStringList = nil;
begin
  Result := False;
  if Length(Data) = 0 then
    Exit;

  try
    try
      AList := TwoDArrayOfExtendedToList(Data, Caption);
      AList.SaveToFile(Filename);
      Result := FileExists(Filename);
    except
      on E:Exception do
      {$IFDEF VISUAL_BUILD}
      ShowMessage('Failed to export 2D array of double: ' + E.Message);
      {$ELSE}
      warn_nv('Failed to export 2D array of double: ' + E.Message);
      {$ENDIF}
    end;
  finally
    if Assigned(AList) then
      AList.Free;
  end;
end;

function RectWidth(aRect: TRect): Integer;
begin
  Result := abs(aRect.Right - aRect.Left);
end;

function RectHeight(aRect: TRect): Integer;
begin
  Result := abs(aRect.Bottom - aRect.Top);
end;

function TransposeMatrixOfExtended(input: T2dArrayOfExtended): T2dArrayOfExtended;
var
  numColumns, numRows: Integer;
  i, j: Integer;
begin
  if not Length(input) > 0 then
    raise Exception.Create('cannot transpose empty matrix');
  numRows := Length(input);
  numColumns := Length(input[0]);
  if numColumns > 0 then
  begin
    for i := Low(input) to High(input) do
    begin
      if Length(input[i]) <> numColumns then
        raise Exception.Create('cannot transpose staggered matrix');
    end;
  end
  else
    raise Exception.Create('cannot transpose single dimensional matrix');
 SetLength(Result, numColumns);
 for i := 0 to NumColumns - 1 do
   SetLength(Result[i], numRows);
 for i := 0 to numRows - 1 do
   for j := 0 to numColumns - 1 do
     Result[j][i] := input[i][j];
end;

function HasIndels(const aSeq: String; const indelCharacter: Char): Boolean;
var
  i: Integer = 0;
  numSites: Integer = 0;
begin
  Result := False;
  numSites := Length(aSeq);
  if numSites > 0 then
    for i := 1 to numSites do
      if aSeq[i] = indelCharacter then
        Exit(True);
end;

procedure UpdateCoverages(const siteConfiguration: String; const indelChar: Char; var coverages: TIntArray);
var
  i: Integer = 0;
  numSeqs: Integer = 0;
begin
  numSeqs := Length(siteConfiguration);
  Assert((numSeqs > 0) and (numSeqs = Length(coverages)));
  for i := 0 to numSeqs - 1 do
    if siteConfiguration[i + 1] <> indelChar then
      coverages[i] += 1;
end;

function NumSiteConfigurations(seqStrings: TStringList; const indelCharacter: Char;  var coverages: TIntArray): Integer;
var
  hashMap: TStringHashMap = nil;
  countsList: TList = nil; { using a separate list for memory cleanup as TStringHashMap.Iterate encounters an access violation when freeing objects}
  aCount: TStringCount = nil;
  row, col: Integer;
  aStr: String;
  numSites: Integer;
  p: Pointer = nil;
  i: Integer = -1;
begin
  try
    {$IFDEF DEBUG}WriteToDevLog('begin MegaUtils.NumSiteConfigurations');{$ENDIF}
    hashMap := TStringHashMap.Create;
    countsList := TList.Create;
    numSites := Length(seqStrings[0]);
    SetLength(aStr, seqStrings.Count);
    for col := 1 to numSites do
    begin
      for row := 0 to seqStrings.Count - 1 do
        aStr[row + 1] := seqStrings[row][col];

      if not hashMap.Find(aStr, p) then
      begin
        aCount := TStringCount.Create(aStr, col);
        aCount.Increment;
        hashMap.Add(aStr, aCount);
        countsList.Add(aCount);
        UpdateCoverages(aStr, indelCharacter, coverages);
      end
      else
      begin
        aCount := TStringCount(p);
        aCount.Increment;
      end;
    end;
    Result := hashMap.Count;
    {$IFDEF DEBUG}WriteToDevLog('end MegaUtils.NumSiteConfigurations');{$ENDIF}
  finally
    if Assigned(hashMap) then
      hashMap.Free;
    if Assigned(countsList) then
    begin
      if countsList.Count > 0 then
        for i := 0 to countsList.Count - 1 do
          if Assigned(countsList[i]) then
            TStringCount(countsList[i]).Free;
      countsList.Free;
    end;
  end;
end;

function NumSiteConfigurationsFewSeqs(seqStrings: TStringList; const indelCharacter: Char;  var coverages: TIntArray): Integer;
var
  hashMap: TFPHashList = nil;
  countsList: TList = nil; { using a separate list for memory cleanup as TStringHashMap.Iterate encounters an access violation when freeing objects}
  aCount: TStringCount = nil;
  row, col: Integer;
  aStr: String = '';
  numSites: Integer = -1;
  i: Integer = -1;
  index: Integer = -1;
begin
  Assert(seqStrings.Count < 256, Format('the hash list being used only supports short strings but the provided data has strings with %d characters', [seqStrings.Count]));
  try
    {$IFDEF DEBUG}WriteToDevLog('begin MegaUtils.NumSiteConfigurationsFewSeqs');{$ENDIF}
    hashMap := TFPHashList.Create;
    countsList := TList.Create;
    numSites := Length(seqStrings[0]);
    SetLength(aStr, seqStrings.Count);
    for col := 1 to numSites do
    begin
      for row := 0 to seqStrings.Count - 1 do
        aStr[row + 1] := seqStrings[row][col];
      index := hashMap.FindIndexOf(aStr);
      if index < 0 then
      begin
        aCount := TStringCount.Create(aStr, col);
        aCount.Increment;
        hashMap.Add(aStr, aCount);
        countsList.Add(aCount);
        UpdateCoverages(aStr, indelCharacter, coverages);
      end
      else
      begin
        aCount := TStringCount(countsList[index]);
        aCount.Increment;
      end;
    end;
    Result := hashMap.Count;
    {$IFDEF DEBUG}WriteToDevLog('end MegaUtils.NumSiteConfigurationsFewSeqs');{$ENDIF}
  finally
    if Assigned(hashMap) then
      hashMap.Free;
    if Assigned(countsList) then
    begin
      if countsList.Count > 0 then
        for i := 0 to countsList.Count - 1 do
          if Assigned(countsList[i]) then
            TStringCount(countsList[i]).Free;
      countsList.Free;
    end;
  end;
end;

function GetAlignmentOfUniqueSiteConfigurations(sourceData: TStringList): TStringList;
begin
  if sourceData.Count < 256 then
  begin
    Result := GetAlignmentOfUniqueSiteConfigurationsFewSeqs(sourceData);
    Exit;
  end;

  try
    Result := GetAlignmentOfUniqueSiteConfigurationsUsingStrHashMap(sourceData);
  except
    Result := GetAlignmentOfUniqueSiteConfigurationsSlow(sourceData);
  end;
end;

function GetAlignmentOfUniqueSiteConfigurationsFewSeqs(sourceData: TStringList): TStringList;
var
  hashMap: TFPHashList = nil;
  countsList: TList = nil;
  aCount: TStringCount = nil;
  row, col: Integer;
  aConfig: String;
  numSites: Integer;
  i: Integer = -1;
  numUniqueConfigs: Integer = -1;
  aSequence: String = '';
  index: Integer = -1;
begin
  Assert(sourceData.Count < 256, Format('the hash list being used only supports short strings but the provided data has strings with %d characters', [sourceData.Count]));
  try
    hashMap := TFPHashList.Create;
    countsList := TList.Create;
    numSites := Length(sourceData[0]);
    SetLength(aConfig, sourceData.Count);
    for col := 1 to numSites do
    begin
      for row := 0 to sourceData.Count - 1 do
        aConfig[row + 1] := sourceData[row][col];
      index := hashMap.FindIndexOf(aConfig);
      if index < 0 then
      begin
        aCount := TStringCount.Create(aConfig, col);
        aCount.Increment;
        countsList.Add(aCount);
        hashMap.Add(aConfig, aCount);
      end
      else
      begin
         aCount := TStringCount(countsList[index]);
         aCount.Increment;
      end;
    end;

    numUniqueConfigs := hashMap.Count;
    SetLength(aSequence, numUniqueConfigs);
    Result := TStringList.Create;
    for row := 0 to sourceData.Count - 1 do
    begin
     for col := 1 to numUniqueConfigs do
       aSequence[col] := TStringCount(countsList[col - 1]).StringValue[row + 1];
     Result.Add(aSequence);
    end;
  finally
    if Assigned(hashMap) then
      hashMap.Free;
    if Assigned(countsList) then
    begin
      if countsList.Count > 0 then
        for i := 0 to countsList.Count - 1 do
          if Assigned(countsList[i]) then
            TStringCount(countsList[i]).Free;
      countsList.Free;
    end;
  end;
end;

function GetAlignmentOfUniqueSiteConfigurationsUsingStrHashMap(sourceData: TStringList): TStringList;
var
  hashMap: TStringHashMap = nil;
  countsList: TList = nil; { using a separate list for memory cleanup as TStringHashMap.Iterate encounters an access violation when freeing objects}
  aCount: TStringCount = nil;
  row, col: Integer;
  aConfig: String;
  numSites: Integer;
  p: Pointer = nil;
  i: Integer = -1;
  numUniqueConfigs: Integer = -1;
  aSequence: String = '';
begin
  try
    hashMap := TStringHashMap.Create;
    countsList := TList.Create;
    numSites := Length(sourceData[0]);
    SetLength(aConfig, sourceData.Count);
    for col := 1 to numSites do
    begin
      for row := 0 to sourceData.Count - 1 do
        aConfig[row + 1] := sourceData[row][col];
      if not hashMap.Find(aConfig, p) then
      begin
        aCount := TStringCount.Create(aConfig, col);
        aCount.Increment;
        hashMap.Add(aConfig, aCount);
        countsList.Add(aCount);
      end
      else
      begin
        aCount := TStringCount(p);
        aCount.Increment;
      end;
    end;

    numUniqueConfigs := hashMap.Count;
    SetLength(aSequence, numUniqueConfigs);
    Result := TStringList.Create;
    for row := 0 to sourceData.Count - 1 do
    begin
     for col := 1 to numUniqueConfigs do
       aSequence[col] := TStringCount(countsList[col - 1]).StringValue[row + 1];
     Result.Add(aSequence);
    end;
  finally
    if Assigned(hashMap) then
      hashMap.Free;
    if Assigned(countsList) then
    begin
      if countsList.Count > 0 then
        for i := 0 to countsList.Count - 1 do
          if Assigned(countsList[i]) then
            TStringCount(countsList[i]).Free;
      countsList.Free;
    end;
  end;
end;

function GetAlignmentOfUniqueSiteConfigurationsSlow(sourceData: TStringList): TStringList;
var
  tempList: TStringList = nil;
  countsList: TList = nil;
  aCount: TStringCount = nil;
  row, col: Integer;
  aConfig: String;
  numSites: Integer;
  i: Integer = -1;
  numUniqueConfigs: Integer = -1;
  aSequence: String = '';
  index: Integer = -1;
begin
  tempList := TStringList.Create;
  tempList.Sorted := True;
  tempList.Duplicates := dupError;

  try
    countsList := TList.Create;
    numSites := Length(sourceData[0]);
    SetLength(aConfig, sourceData.Count);
    for col := 1 to numSites do
    begin
      for row := 0 to sourceData.Count - 1 do
        aConfig[row + 1] := sourceData[row][col];
      index := tempList.IndexOf(aConfig);
      if index < 0 then
      begin
        aCount := TStringCount.Create(aConfig, col);
        aCount.Increment;
        countsList.Add(aCount);
        tempList.Add(aConfig)
      end
      else
      begin
         aCount := TStringCount(countsList[index]);
         aCount.Increment;
      end;
    end;

    numUniqueConfigs := tempList.Count;
    SetLength(aSequence, numUniqueConfigs);
    Result := TStringList.Create;
    for row := 0 to sourceData.Count - 1 do
    begin
     for col := 1 to numUniqueConfigs do
     begin
       aSequence[col] := TStringCount(countsList[col - 1]).StringValue[row + 1];
     end;
     Result.Add(aSequence);
    end;
  finally
    if Assigned(tempList) then
      tempList.Free;
    if Assigned(countsList) then
    begin
      if countsList.Count > 0 then
        for i := 0 to countsList.Count - 1 do
          if Assigned(countsList[i]) then
            TStringCount(countsList[i]).Free;
      countsList.Free;
    end;
  end;
end;

function DebugUniqueStringsInverted(sourceData: TStringList): TStringList;
var
  aStr: String = '';
  i: Integer = -1;
  row: Integer = -1;
  site: Integer = -1;
begin
  Result := TStringList.Create;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  SetLength(aStr, sourceData.Count);
  for i := Low(aStr) to High(aStr) do
    aStr[i] := '-';
  for site := 1 to Length(sourceData[0]) do
  begin
    for row := 0 to sourceData.Count - 1 do
      aStr[row + 1] := sourceData[row][site];
    Result.Add(aStr);
  end;
end;

function DebugUniqueConfigsSorted(sourceData: TStringList): TStringList;
var
  temp: TStringList = nil;
  col: Integer = -1;
  row: Integer = -1;
  aStr: String;
  i: Integer = -1;
begin
  try
    Result := TStringList.Create;
    temp := DebugUniqueStringsInverted(sourceData);
    SetLength(aStr, temp.Count);
    for i := 1 to Length(aStr) do
      aStr[i] := '-';

    for col := 0 to Length(temp[0]) - 1 do
    begin
      for row := 0 to temp.Count - 1 do
      begin
        aStr[row + 1] := temp[row][col + 1];
      end;
      Result.Add(aStr);
    end;
  finally
    if Assigned(temp) then
      temp.Free;
  end;
end;

function GetSitePatternFrequencies(seqData: TStringList): TArrayOfInt64;
var
  hashMap: TStringHashMap = nil;
  countsList: TList = nil; { using a separate list for memory cleanup as TStringHashMap.Iterate encounters an access violation when freeing objects}
  aCount: TStringCount = nil;
  row, col: Integer;
  aStr: String;
  numSites: Integer;
  p: Pointer = nil;
  i: Integer = -1;
  sitesList: TList = nil;
begin
  try
    SetLength(Result, Length(seqData[0]));
    for i := Low(Result) to High(Result) do
      Result[i] := 0;
    sitesList := TList.Create;
    hashMap := TStringHashMap.Create;
    countsList := TList.Create;
    numSites := Length(seqData[0]);
    SetLength(aStr, seqData.Count);
    for col := 1 to numSites do
    begin
      for row := 0 to seqData.Count - 1 do
        aStr[row + 1] := seqData[row][col];

      if not hashMap.Find(aStr, p) then
      begin
        aCount := TStringCount.Create(aStr, col);
        aCount.Increment;
        hashMap.Add(aStr, aCount);
        countsList.Add(aCount);
        sitesList.Add(aCount);
      end
      else
      begin
        aCount := TStringCount(p);
        aCount.Increment;
        sitesList.Add(aCount);
      end;
    end;
    for i := Low(Result) to High(Result) do
    begin
      aCount := TStringCount(sitesList[i]);
      Result[i] := aCount.Count;
    end;
  finally
    if Assigned(hashMap) then
      hashMap.Free;
    if Assigned(sitesList) then
      sitesList.Free;
    if Assigned(countsList) then
    begin
      if countsList.Count > 0 then
        for i := 0 to countsList.Count - 1 do
          if Assigned(countsList[i]) then
            TStringCount(countsList[i]).Free;
      countsList.Free;
    end;
  end;
end;

{$IFDEF DEBUG}
function WriteToDevLogFile(aMsg: String): Boolean;
{$IFNDEF VISUAL_BUILD}
var
  aFile: TextFile;
  filename: String;
{$ENDIF}
begin
  {$IFNDEF VISUAL_BUILD}
  try
    DevLogFileCS.Acquire;
    AssignFile(aFile, filename);
    if not FileExists(filename) then
      Rewrite(aFile)
    else
      Append(aFile);
    WriteLn(aFile, aMsg);
  finally
    CloseFile(aFile);
    DevLogFileCS.Release;
  end;
  Result := FileExists(filename);
  {$ENDIF}
end;
{$ENDIF}

procedure WriteToDevLog(aMsg: String);
begin
  {$IFNDEF VISUAL_BUILD}{$IFDEF DEBUG}
  exit;
  try
    DevLogFileCS.Acquire;
    if not Assigned(DeveloperLog) then
      DeveloperLog := TStringList.Create;
    if Pos('begin', aMsg) = 1 then
    begin
      if DEV_LOG_MARKER = BEGIN_MARKER then
        DEV_LOG_INDENT := DEV_LOG_INDENT + #9
      else
        DEV_LOG_MARKER := BEGIN_MARKER;
    end
    else if Pos('end', aMsg) = 1 then
    begin
      if DEV_LOG_MARKER = END_MARKER then
        SetLength(DEV_LOG_INDENT, max(0, Length(DEV_LOG_INDENT) - 1))
      else
        DEV_LOG_MARKER := END_MARKER;
    end;
    DeveloperLog.Add(Format('%-20s %s%s', [FormatDateTime('YYYY-MM-DD hh:mm:ss.zzz', Now), DEV_LOG_INDENT, aMsg]));
    //WriteLn(Format('DEV-LOG: %-20s %s%s', [FormatDateTime('YYYY-MM-DD hh:mm:ss.zzz', Now), DEV_LOG_INDENT, aMsg]));
  finally
    DevLogFileCS.Release;
  end;
  {$ENDIF}{$ENDIF}
end;

function DensityDistributToCalibrationCategory(
  aDensityDist: TCalibrationDensityDistribution): TCalibrationCategory;
begin
  Result := ccNone;
  case aDensityDist of
    cddNormal: Result := ccDensityNormal;
    cddLogNormal: Result := ccDensityLogNormal;
    cddExponential: Result := ccDensityExponential;
    cddUniform: Result := ccDensityUniform;
    cddNone: Result := ccNone;
  end;
end;

function DebugNucSiteCompositionToFile(seqs: TStringList; filename: String): Boolean;
const
  BaseA = 0;
  BaseT = 1;
  BaseC = 2;
  BaseG = 3;
  BaseN = 4;
  BaseR = 5;
  BaseY = 6;
  BaseM = 7;
  BaseK = 8;
  BaseS = 9;
  BaseW = 10;
  BaseH = 11;
  BaseB = 12;
  BaseV = 13;
  BaseD = 14;
  indel = 15;
  missing = 16;
  other = 17;
var
  i: Integer;
  aFile: TextFile;
  seq, site: Integer;
  numSites: Int64 = 0;
  aChar: Char;
  counts: array[BaseA..other] of Integer;
begin
  Result := False;
  if (not Assigned(seqs)) or (seqs.Count = 0) then
    Exit;
  numSites := Length(seqs[0]);
  if numSites = 0 then
    Exit;
  try
    try
      AssignFile(aFile, filename);
      Rewrite(aFile);
      WriteLn(aFile, 'site: ,A,T,C,G,R,Y,M,K,S,W,H,B,V,D,-,?,other');
      for site := 1 to numSites do
      begin
        for i := Low(counts) to High(counts) do
          counts[i] := 0;
        for seq := 0 to seqs.Count - 1 do
        begin
          aChar := seqs[seq][site];
          case upcase(aChar) of
            'A': inc(counts[BaseA]);
            'T',
            'U': inc(counts[BaseT]);
            'C': inc(counts[BaseC]);
            'G': inc(counts[BaseG]);
            'R': inc(counts[BaseR]);
            'Y': inc(counts[BaseY]);
            'M': inc(counts[BaseM]);
            'K': inc(counts[BaseK]);
            'W': inc(counts[BaseW]);
            'S': inc(counts[BaseS]);
            'B': inc(counts[BaseB]);
            'V': inc(counts[BaseV]);
            'D': inc(counts[BaseD]);
            'H': inc(counts[BaseH]);
            '-': inc(counts[indel]);
            '?', 'N': inc(counts[missing]);
            else
              inc(counts[other]);
          end;
        end;
        WriteLn(aFile, Format('%d: ,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d', [site, counts[BaseA], counts[BaseT], counts[BaseC], counts[BaseG], counts[BaseR], counts[BaseY], counts[BaseM], counts[BaseK], counts[BaseS], counts[BaseW], counts[BaseH], counts[BaseB], counts[BaseV], counts[BaseD], counts[indel], counts[missing], counts[other]]));
      end;
      Result := FileExists(filename);
    except
      on E:Exception do
        Result := False;
    end;
  finally
    CloseFile(aFile);
  end;
end;

function DebugRemoveLowCoverageSitesAndWriteToFasta(seqs: TStringList; names: TStringList; coverage: TIntArray; cutoff: Integer; filename: String): Boolean;
var
  seq, site: Integer;
  outFile: TextFile;
begin
  if not (seqs.Count = names.Count) then
    raise Exception.Create('dimension mismatch');
  if seqs.Count = 0 then
    raise Exception.Create('no data available');

  try
    try
      AssignFile(outFile, filename);
      Rewrite(outFile);
      for seq := 0 to seqs.Count - 1 do
      begin
        WriteLn(outFile, '>' +names[seq]);
        for site := 0 to Length(coverage) - 1 do
          if coverage[site] >= cutoff then
            Write(outFile, seqs[seq][site + 1]);
        WriteLn(outFile);
      end;
    except
      on E:Exception do
      begin
        {$IFDEF VISUAL_BUILD}
        ShowMessage(E.Message);
        {$ELSE}
        WriteLn(outFile, E.Message);
        {$ENDIF}
      end;
    end;
  finally
    CloseFile(outFile);
  end;
  Result := FileExists(filename);
end;

{$IFDEF VISUAL_BUILD}
function IsExcel2010OrGreater: Boolean;
var
  VersionStr: AnsiString = '';
  Period: Integer;
  Temp: AnsiString;
  Version: Integer;
begin
  Result := True;
  {$IFDEF MSWINDOWS}
  Result := False;
  try
  if GetExcelVersionStr(VersionStr) then
  begin
    Period := Pos('.', VersionStr);
    if Period > 0 then
    begin
      Temp := Copy(VersionStr, 1, Period - 1);
      Version := StrToInt(Temp);
      if Version >= 14 then
        Result := True;
    end;
  end;
  except
    // do nothing, it is not worth bothering the user
  end;
  {$ENDIF}
end;


{$IFDEF MSWINDOWS}

function GetExcelVersionStr(var Version: AnsiString): Boolean;
//type
//  PLandCodepage = ^TLandCodepage;
//  TLandCodepage = record
//    wLanguage,
//    wCodePage: word;
//  end;
//var
//  Registry: TRegistry;
//  PathToExe: String;
//  Dummy: Cardinal;
//  Len: Cardinal;
//  Buffer: Pointer;
//  Pntr: Pointer;
//  Lang: String;
begin
  Result := False;
  Assert(False, 'disabled for fpc because of GetFileVersionInfoSize');
  //{$IFDEF PROTOTYPER}
  // Assert(False, 'disabled for fpc because of GetFileVersionInfoSize');
  //{$ELSE}{$IFNDEF CALTEST}
  //Result := False;
  //Version := EmptyStr;
  //PathToExe := EmptyStr;
  //Registry := TRegistry.Create;
  //try
  //  try
  //    Registry.RootKey := HKEY_LOCAL_MACHINE;
  //    if Registry.OpenKey('SOFTWARE' + PathDelim + 'Microsoft' + PathDelim + 'Windows' + PathDelim + 'CurrentVersion' + PathDelim + 'App Paths' + PathDelim + 'excel.exe', false) then
  //    begin
  //      PathToExe := Registry.ReadString('Path') + 'excel.exe';
  //      if FileExists(PathToExe) then
  //      begin
  //        Len := GetFileVersionInfoSize(PChar(PathToExe), Dummy);
  //        if Len = 0 then
  //          RaiseLastOSError;
  //        GetMem(Buffer, Len);
  //
  //        if not GetFileVersionInfo(PChar(PathToExe), 0, Len, Buffer) then
  //          raise Exception.Create('failed to get version info');
  //
  //        if not VerQueryValue(Buffer, PathDelim + 'VarFileInfo' + PathDelim + 'Translation' + PathDelim, Pntr, Len) then
  //          raise Exception.Create('failed VerQueryValue');
  //
  //        Lang := Format('%.4x%.4x', [PLandCodepage(pntr)^.wLanguage, PLandCodepage(pntr)^.wCodePage]);
  //        if not VerQueryValue(Buffer, PChar(PathDelim + 'StringFileInfo' + PathDelim + Lang + PathDelim + 'FileVersion'), Pntr, Len) then
  //          RaiseLastOsError;
  //        Version := PChar(Pntr);
  //        Result := True;
  //      end;
  //    end;
  //  except
  //    on E: Exception do
  //    begin
  //      Version := EmptyStr;
  //      Result := False;
  //    end;
  //  end;
  //finally
  //  Registry.Free;
  //  FreeMem(Buffer);
  //end;
  //{$ENDIF}{$ENDIF CALTEST}
end;
{$ENDIF MSWINDOWS}
{$ENDIF VISUAL_BUILD}

function ExpandBootAlignmentToFasta(aTable: PArrayOfInt; bootTableLength: Integer; seqLength: Integer; seqs: TStringList; otuNames: TStringList; filename: String): Boolean;
var
  aFile: TextFile;
  index: Integer = 1;
  seq: Integer = -1;
  site: Integer = -1;
  i: Integer = -1;
  sequence: String = '';
  aCount: Integer = -1;
begin
  try
    AssignFile(aFile, filename);
    Rewrite(aFile);
    SetLength(sequence, seqLength);
    for i := 1 to Length(sequence) do
      sequence[i] := '-';
    for seq := 0 to otuNames.Count - 1 do
    begin
      index := 1;
      WriteLn(aFile, '>' + otuNames[seq]);
      for site := 1 to bootTableLength do
      begin
        aCount := aTable^[site];
        if aCount > 0 then
        begin
          for i := 1 to aCount do
          begin
            sequence[index] := seqs[seq][site];
            inc(index);
          end;
        end;
      end;
      WriteLn(aFile, sequence);
    end;
    Result := FileExists(filename);
  finally
    CloseFile(aFile);
  end;
end;

function CalculateTargetSitePatternCountFactor(const aNumSitePatterns: Integer; const isAmino: Boolean; var msg: String): Double;
begin
  if isAmino then
    //Result := (4/20*4594.2*Power(aNumSitePatterns, -1.043))*100
    Result := (10/20*4594.2*Power(aNumSitePatterns, -1.043))*100 { per Sudhir, divide by 2 instead of 5 (i.e. 10/20 instead of 4/20)}
  else
    Result := (4594.2*Power(aNumSitePatterns, -1.043))*100;

  if CompareValue(Result, 30.0, FP_CUTOFF) >= 0 then
  begin
    Result := 100.0;
    msg := Format('adaptive sampling found only %.0n site patterns which is insufficient for the sub-sampling approach. The full alignment data will be used.', [aNumSitePatterns*1.0]);
  end
  else
    msg := EmptyStr;
end;

{$IFDEF DEBUG}

function AlignmentToFasta(seqs: TStringList; otuNames: TStringList; filename: String): Boolean;
var
  aFile: TextFile;
  i: Integer;
begin
  try
    AssignFile(aFile, filename);
    Rewrite(aFile);
    for i := 0 to seqs.Count - 1 do
    begin
     WriteLn(aFile, Format('>%s', [otuNames[i]]));
     WriteLn(aFile, seqs[i]);
    end;
    Result := FileExists(filename);
  finally
    CloseFile(aFile);
  end;
end;
{$ENDIF}

function ExpandBootAlignmentToFullSeqData(aTable: PArrayOfInt; bootTableLength: Integer; seqLength: Integer; seqs: TStringList): TStringList;
var
  index: Integer = 1;
  seq: Integer = -1;
  site: Integer = -1;
  i: Integer = -1;
  sequence: String = '';
  aCount: Integer = -1;
begin
  Result := TStringList.Create;
  SetLength(sequence, seqLength);
  for i := 1 to Length(sequence) do
    sequence[i] := '-';
  for seq := 0 to seqs.Count - 1 do
  begin
    index := 1;
    for site := 1 to bootTableLength do
    begin
      aCount := aTable^[site];
      if aCount > 0 then
      begin
        for i := 1 to aCount do
        begin
          sequence[index] := seqs[seq][site];
          inc(index);
        end;
      end;
    end;
    Result.Add(sequence);
  end;
end;

initialization
begin
  Factln_a[0] := -1;
  {$IFDEF DEBUG}{$IFNDEF VISUAL_BUILD}
  DevLogFileCS := TCriticalSection.Create;
  DeveloperLog := TStringList.Create;
  {$ENDIF}{$ENDIF}
end;


{$IFNDEF VISUAL_BUILD}{$IFDEF DEBUG}
finalization
begin
  if Assigned(DevLogFileCS) then
    DevLogFileCS.Free;
  if Assigned(DeveloperLog) then
    DeveloperLog.Free;
end;
{$ENDIF}{$ENDIF}

end.



