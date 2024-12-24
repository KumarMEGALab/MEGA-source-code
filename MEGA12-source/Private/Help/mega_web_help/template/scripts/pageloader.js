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

initPage();
function initPage()
{
	if( window.addEventListener )
	{
		window.addEventListener('load', onPageLoad, false);
	}
	else if( window.attachEvent )
	{
		window.attachEvent('onload', onPageLoad, false);
	}
	if( window.addEventListener )
	{
		window.addEventListener('unload', onPageUnload, false);
	}
	else if( window.attachEvent )
	{
		window.attachEvent('onunload', onPageUnload, false);
	}

}
function onPageUnload()
{
	//This function is deliberately set empty. 
	//Overriding onunload results in clearing of back-forward cache(also called bfcache)
}

function onPageLoad()
{
	if(gbBlockIOSScaling)
		blockIOSScaling();
	if(gbPreviewMode)
	{
		initSettings(gRootRelPath);
		fireRhLoadCompleteEvent();
		if(!rh.model.get(rh.consts('KEY_PUBLISH_MODE')))
			window.rh.model.publish(window.rh.consts('KEY_PROJECT_LIST'), ["."]);
	}
	else
		initAndLoadParentData(null, SCR_NONE);
}
