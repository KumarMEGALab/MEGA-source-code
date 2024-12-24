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

//This list of tags can be extended to add more tags for Show/Hide support
var gShowHideTagsList = ["div", "a"];

addRhLoadCompleteEvent(doShowHide);

function doShowHide(evt, bIgnoreUrlParam)
{
	var showHideMode;
	var cshModeFlag = "";
	if(bIgnoreUrlParam == true)
		cshModeFlag = "";
	else
		cshModeFlag = getUrlParameter(RHCSHMODE);
	if(cshModeFlag == TRUESTR)
	{
		showHideMode = CSHMODE;
		saveSetting(RHCSHMODE, CSHMODE);
		callbackDoShowHideCSHModeCookie(showHideMode);
	}
	else if(cshModeFlag == FALSESTR)
	{
		showHideMode = NONCSHMODE;
		saveSetting(RHCSHMODE, NONCSHMODE);
		callbackDoShowHideCSHModeCookie(showHideMode);
	}
	else
		readSetting(RHCSHMODE, callbackDoShowHideCSHModeCookie);
}

function callbackDoShowHideCSHModeCookie(showHideMode)
{		
	if(showHideMode == "")
		showHideMode = NONCSHMODE;
	
	publishCSHMode(showHideMode);

	var len = gShowHideTagsList.length;
	for(var i=0; i<len; i++)
	{
		var tagName = gShowHideTagsList[i];
		var elemsList = document.getElementsByTagName(tagName);
		showHideElems(elemsList, showHideMode);
	}
}

function publishCSHMode(showHideMode) {
	if (showHideMode === CSHMODE)
		rh.model.publish(rh.consts('KEY_CSH_MODE'), true);
	else
		rh.model.publish(rh.consts('KEY_CSH_MODE'), false);
}

function showHideElems(elemsList, showHideMode)
{
	var len = elemsList.length;
	for(var i=0; i<len; i++)
	{
		var elem = elemsList[i];
		var showinAttrib = elem.getAttribute(DATASHOWIN);
		if(showinAttrib == SHOWINCSHMODE)
		{
			if(showHideMode == CSHMODE)
				elem.style.display = "";
			else
				elem.style.display = "none";
		}
		else if(showinAttrib == SHOWINNONCSHMODE)
		{
			if(showHideMode == CSHMODE)
				elem.style.display = "none";
			else
				elem.style.display = "";
		}
	}

}
function onShowHideClick()
{
	readSetting(RHCSHMODE, callbackShowHideClickCSHModeCookie);
}

function callbackShowHideClickCSHModeCookie(showHideMode)
{
	if(showHideMode == CSHMODE)
		showHideMode = NONCSHMODE;
	else if(showHideMode == "" || showHideMode == NONCSHMODE)
		showHideMode = CSHMODE;

	saveSetting(RHCSHMODE, showHideMode);
	doShowHide(null, true);
}
