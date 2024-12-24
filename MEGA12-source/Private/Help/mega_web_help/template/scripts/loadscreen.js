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

gScreenDataFile = "screendata.js";
gScreenFolder = "";

var gScreenRelPathMap = new Object();
function redirectToScreenURL()
{
	loadScreens(SCR_INIT, ".");	
}
function loadScreenData(relPath, flowType, data)
{
	var scrFolder = undefined 
	
	if (gScreenRelPathMap[relPath]) 
	{ 
		scrFolder = gScreenRelPathMap[relPath].folder; 
	}
	
	if(scrFolder != undefined)
	{
		returnScreenDataCall(flowType, relPath, scrFolder, data);
		return;
	}
	loadScreens(flowType, relPath, data);
}
function screenDataCallBackObj()
{
	this.flowType = SCR_NONE;
	this.relPath = null;
	this.data = null;
	this.curScrFolder = null;
}
function screenObject()
{
	this.minWdith = -1;
	this.maxWidth = -1;
	this.minHeight = -1;
	this.maxHeight = -1;
	this.browserAgent = null;
	this.folder = null;
	this.defaultURL = null;
	this.isDefault = false;
	this.isDeviceMatching = null;
}
function loadScreens(flowType, relPath, data)
{
	var callBackObj = new screenDataCallBackObj();
	callBackObj.flowType = flowType;
	callBackObj.relPath = relPath;
	callBackObj.data = data;
	xmlJsReader.loadFile(relPath + "/" + gScreenDataFile, callbackScreenDataLoaded, callBackObj);	
}

function callbackScreenDataLoaded(xmlDoc, callBackObj)
{
	var screenObj = getDeviceMatchingScreen(xmlDoc);

	if(screenObj != null)
		gScreenRelPathMap[callBackObj.relPath] = { folder: screenObj.folder, defaultURL: screenObj.defaultURL };
	else
		gScreenRelPathMap[callBackObj.relPath] = { folder: null, defaultURL: null };
	
	returnScreenDataCall(callBackObj.flowType, callBackObj.relPath, gScreenRelPathMap[callBackObj.relPath].folder, callBackObj.data);	
}
	
function returnScreenDataCall(flowType, relPath, scrFolder, data)
{
	if(flowType == SCR_CHILD_TOC)
	{
		if(scrFolder != null)
			loadProjData(relPath + "/" + scrFolder, relPath, data);
		else
			loadProjData("", "", data);
	}
	else if(flowType == SCR_CHILD_IDX || flowType == SCR_CHILD_GLO || flowType == SCR_CHILD_FTS || flowType == SCR_CHILD_CSH)
	{
		if(scrFolder != null)
			collectAllChildPaths(relPath + "/" + scrFolder, relPath, flowType);
		else
		{
			var childProjUrlQueue = data;
			if(childProjUrlQueue.isEmpty())
				returnProjDataCall();
			else
			{
				var path = childProjUrlQueue.dequeue();
				loadScreenData(path, flowType, childProjUrlQueue);
			}
		}
	}
	else if(flowType == SCR_PARENT_TOC || flowType == SCR_PARENT_IDX || 
			flowType == SCR_PARENT_GLO || flowType == SCR_PARENT_FTS || flowType == SCR_NONE)
	{
		if(scrFolder != null)
		{
			gFinalRootRelPath = relPath + "/" + scrFolder;
			gFinalCommonRootRelPath = relPath;
			gFinalScrFolder = scrFolder;
			loadParentData(relPath, flowType);
		}
		else
			returnParentDataCall();
	}
	else if(flowType == SCR_PARENT_BC || flowType == SCR_PARENT_TOCSYNC)
	{
		if(scrFolder != null)
		{
			var finalRootRelPath = relPath + "/" + scrFolder;
			var finalCommonRootRelPath = relPath;
			returnParentDataCallForSyncing(flowType, data, finalRootRelPath, finalCommonRootRelPath);
		}
		else
			returnParentDataCallForSyncing(flowType, null);
			
	}
}	

function getScreenObj(screenNode)
{
	var screenObj = new screenObject();
	screenObj.minWidth=parseInt(screenNode.getAttribute(MINWIDTH));
	screenObj.maxWidth=parseInt(screenNode.getAttribute(MAXWIDTH));
	screenObj.minHeight=parseInt(screenNode.getAttribute(MINHEIGHT));
	screenObj.maxHeight=parseInt(screenNode.getAttribute(MAXHEIGHT));
	screenObj.browserAgent=screenNode.getAttribute(BROWSERAGENT);
	screenObj.folder=screenNode.getAttribute(FOLDER);		
	screenObj.defaultURL=screenNode.getAttribute(DEFAULTURL);
	var defaultFlag = screenNode.getAttribute(DEFAULT);
	if(defaultFlag == TRUESTR)
		screenObj.isDefault=true;
	return screenObj;
}

function getDeviceMatchingScreen(xmlDoc, isDefault)
{
	var currentWidth = screen.width;
	var currentHeight = screen.height;
	var szCurrentBrowserAgentString = ""+navigator.userAgent;

	var screensXmlNode = xmlDoc.getElementsByTagName(SCREENSNODE)[0];	
	var len = screensXmlNode.childNodes.length;
	var screenArray = new Array();
	var screenObj = null;
	var defaultScreenObj = null;
	for(var i=0; i<len; i++)
	{
		var screenNode = screensXmlNode.childNodes[i];	
		screenObj = getScreenObj(screenNode);
		var bMinWidth=true;
		if(screenObj.minWidth>0)
		{
			bMinWidth = currentWidth>=screenObj.minWidth;
		}
	
		var bMaxWidth=true;
		if(screenObj.maxWidth>0)
		{
			bMaxWidth = currentWidth<=screenObj.maxWidth;
		}
	
		var bMinHeight=true;
		if(screenObj.minHeight>0)
		{
			bMinHeight = currentHeight>=screenObj.minHeight;
		}
	
		var bMaxHeight=true;
		if(screenObj.maxHeight>0)
		{
			bMaxHeight = currentHeight<=screenObj.maxHeight;
		}
	
		var bBrowserAgent = true;
		
		
		if (screenObj.browserAgent != null && screenObj.browserAgent.length > 0) {
		  
			var szBrowserAgent = screenObj.browserAgent;
			  
			szBrowserAgent = trimString(szBrowserAgent);
			if (szBrowserAgent.length > 0) {
				bBrowserAgent = false;
				//we need to see if the browser agent is same or not
				//split it on the bases of |
				var screenAgentArray = szBrowserAgent.split("|");
				for (var stringIndex = 0; stringIndex < screenAgentArray.length; stringIndex++) {
					var szTemp = screenAgentArray[stringIndex];
					//check if the browser agent has this 
					if (szCurrentBrowserAgentString.indexOf(szTemp) != -1) {
						bBrowserAgent = true;
						break;
					}
				}
			}
		}
	
		//if all conditions met then we need to redirect to that URL now
		if (bMinHeight && bMinWidth && bMaxWidth && bMaxHeight && bBrowserAgent) 
		{
			screenObj.isDeviceMatching = true;
			return screenObj;
		}
		else if(screenObj.isDefault)
		{
			defaultScreenObj = screenObj;
			defaultScreenObj.isDeviceMatching = true;
		}
	}
	
	return defaultScreenObj;
}
