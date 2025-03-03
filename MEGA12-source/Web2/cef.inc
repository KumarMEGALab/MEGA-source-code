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

// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright � 2023 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

 // The complete list of compiler versions is here :
 // http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Compiler_Versions

{$DEFINE DELPHI_VERSION_UNKNOW}

// Delphi 5
{$IFDEF VER130}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
{$ENDIF}

// Delphi 6
{$IFDEF VER140}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
{$ENDIF}

// Delphi 7
{$IFDEF VER150}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
{$ENDIF}

// Delphi 8
{$IFDEF VER160}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
{$ENDIF}

// Delphi 2005
{$IFDEF VER170}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
{$ENDIF}

{$IFDEF VER180}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  // Delphi 2007
  {$IFDEF VER185}
    {$DEFINE DELPHI5_UP}
    {$DEFINE DELPHI6_UP}
    {$DEFINE DELPHI7_UP}
    {$DEFINE DELPHI8_UP}
    {$DEFINE DELPHI9_UP}
    {$DEFINE DELPHI10_UP}
    {$DEFINE DELPHI11_UP}
  // Delphi 2006
  {$ELSE}
    {$DEFINE DELPHI5_UP}
    {$DEFINE DELPHI6_UP}
    {$DEFINE DELPHI7_UP}
    {$DEFINE DELPHI8_UP}
    {$DEFINE DELPHI9_UP}
    {$DEFINE DELPHI10_UP}
  {$ENDIF}
{$ENDIF}

// Delphi 2009
{$IFDEF VER200}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
{$ENDIF}

//Delphi 2010
{$IFDEF VER210}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
{$ENDIF}

// Delphi XE
{$IFDEF VER220}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
{$ENDIF}

// Delphi XE2  (First FireMonkey and 64bit compiler)
{$IFDEF VER230}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
{$ENDIF}

// Delphi XE3
{$IFDEF VER240}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
{$ENDIF}

// Delphi XE4
{$IFDEF VER250}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
{$ENDIF}

// Delphi XE5
{$IFDEF VER260}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
{$ENDIF}

// Delphi XE6
{$IFDEF VER270}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
{$ENDIF}

// Delphi XE7
{$IFDEF VER280}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
{$ENDIF}

// Delphi XE8
{$IFDEF VER290}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
{$ENDIF VER290}

// Rad Studio 10 - Delphi Seattle
{$IFDEF VER300}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
{$ENDIF}

// Rad Studio 10.1 - Delphi Berlin
{$IFDEF VER310}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI24_UP}
{$ENDIF}

// Rad Studio 10.2 - Delphi Tokyo
{$IFDEF VER320}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI24_UP}
  {$DEFINE DELPHI25_UP}
{$ENDIF}

// Rad Studio 10.3 - Delphi Rio
{$IFDEF VER330}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI24_UP}
  {$DEFINE DELPHI25_UP}
  {$DEFINE DELPHI26_UP}
{$ENDIF}

// Rad Studio 10.4 - Delphi Sydney
{$IFDEF VER340}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI24_UP}
  {$DEFINE DELPHI25_UP}
  {$DEFINE DELPHI26_UP}
  {$DEFINE DELPHI27_UP}
{$ENDIF}

// Rad Studio 11.0 - Delphi Alexandria
{$IFDEF VER350}
  {$UNDEF DELPHI_VERSION_UNKNOW}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI8_UP}
  {$DEFINE DELPHI9_UP}
  {$DEFINE DELPHI10_UP}
  {$DEFINE DELPHI11_UP}
  {$DEFINE DELPHI12_UP}
  {$DEFINE DELPHI14_UP}
  {$DEFINE DELPHI15_UP}
  {$DEFINE DELPHI16_UP}
  {$DEFINE DELPHI17_UP}
  {$DEFINE DELPHI18_UP}
  {$DEFINE DELPHI19_UP}
  {$DEFINE DELPHI20_UP}
  {$DEFINE DELPHI21_UP}
  {$DEFINE DELPHI22_UP}
  {$DEFINE DELPHI23_UP}
  {$DEFINE DELPHI24_UP}
  {$DEFINE DELPHI25_UP}
  {$DEFINE DELPHI26_UP}
  {$DEFINE DELPHI27_UP}
  {$DEFINE DELPHI28_UP}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE SUPPORTS_INLINE}
  {$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)}
    {$DEFINE FPC_VER_320}
  {$IFEND}
{$ELSE}
  {$IFDEF DELPHI_VERSION_UNKNOW}
    {$DEFINE DELPHI5_UP}
    {$DEFINE DELPHI6_UP}
    {$DEFINE DELPHI7_UP}
    {$DEFINE DELPHI8_UP}
    {$DEFINE DELPHI9_UP}
    {$DEFINE DELPHI10_UP}
    {$DEFINE DELPHI11_UP}
    {$DEFINE DELPHI12_UP}
    {$DEFINE DELPHI14_UP}
    {$DEFINE DELPHI15_UP}
    {$DEFINE DELPHI16_UP}
    {$DEFINE DELPHI17_UP}
    {$DEFINE DELPHI18_UP}
    {$DEFINE DELPHI19_UP}
    {$DEFINE DELPHI20_UP}
    {$DEFINE DELPHI21_UP}
    {$DEFINE DELPHI22_UP}
    {$DEFINE DELPHI23_UP}
    {$DEFINE DELPHI24_UP}
    {$DEFINE DELPHI25_UP}
    {$DEFINE DELPHI26_UP}
    {$DEFINE DELPHI27_UP}
	{$DEFINE DELPHI28_UP}
  {$ENDIF}
{$ENDIF}

{$IFDEF DELPHI9_UP}
  {$DEFINE SUPPORTS_INLINE}
{$ENDIF}

{$IF DEFINED(CPUX32) OR
     DEFINED(CPU32) OR
     DEFINED(CPU32BITS) OR
     DEFINED(CPUARM32) OR
     DEFINED(WIN32) OR
     DEFINED(IOS32) OR
     DEFINED(MACOS32) OR
     DEFINED(LINUX32) OR
     DEFINED(POSIX32) OR
     DEFINED(ANDROID32)}
  {$DEFINE TARGET_32BITS}
{$ELSE}
  {$IF DEFINED(CPUX64) OR
       DEFINED(CPU64) OR
       DEFINED(CPU64BITS) OR
       DEFINED(CPUARM64) OR
       DEFINED(WIN64) OR
       DEFINED(IOS64) OR
       DEFINED(MACOS64) OR
       DEFINED(LINUX64) OR
       DEFINED(POSIX64) OR
       DEFINED(ANDROID64)}
    {$DEFINE TARGET_64BITS}
  {$IFEND}
{$IFEND}

// Delphi uses MACOS for the new MacOSX and DARWIN is not defined
// FPC uses DARWIN for the new MacOSX and MACOS is defined for the classic Macintosh OS (System 7)
// We define MACOSX to avoid conflicts in both situations
{$IFDEF FPC}
  {$IFDEF DARWIN}
    {$DEFINE MACOSX}
  {$ENDIF}
{$ELSE}
  {$IFDEF MACOS}
    {$DEFINE MACOSX}
  {$ENDIF}
{$ENDIF}
