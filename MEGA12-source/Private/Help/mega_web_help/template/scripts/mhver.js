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

//	Mobile Help 1.0.0
var gbNav=false;
var gbNav6=false;
var gbNav61=false;
var gbNav7=false;
var gbNav4=false;
var gbIE4=false;
var gbIE=false;
var gbIE5=false;
var gbIE55=false;
var gbIE9=false;
var gbIE10=false;
var gbOpera6=false;
var gbOpera7=false;
var gbKonqueror3=false;
var gbSafari3=false;

var gAgent=navigator.userAgent.toLowerCase();
var gbMac=(gAgent.indexOf("mac")!=-1);
var gbSunOS=(gAgent.indexOf("sunos")!=-1);
var gbUnixOS=(gAgent.indexOf("linux")!=-1) || (gAgent.indexOf("unix")!=-1);
var gbOpera=(gAgent.indexOf("opera")!=-1);
var gbKonqueror=(gAgent.indexOf("konqueror")!= -1);
var gbSafari=(gAgent.indexOf("safari")!= -1);
var gbWindows=((gAgent.indexOf('win')!= -1)||(gAgent.indexOf('16bit')!= -1));
var gbMozilla=((gAgent.indexOf('gecko')!=-1) && (gAgent.indexOf('netscape')==-1));
var gbAIR=(gAgent.indexOf('adobeair')!=-1);
var gbChrome = (gAgent.indexOf('chrome')!=-1);
var gbAIRSSL= true ;

var gVersion=navigator.appVersion.toLowerCase();

var gnVerMajor=parseInt(gVersion);
var gnVerMinor=parseFloat(gVersion);

if(!gbOpera&&!gbKonqueror&&!gbSafari) // opera can mimic IE or Netscape by settings.
{
	gbIE=(navigator.appName.indexOf("Microsoft")!=-1);
	gbNav=(gAgent.indexOf('mozilla')!=-1) && ((gAgent.indexOf('spoofer')==-1) && (gAgent.indexOf('compatible')==-1));
	if(gnVerMajor>=4)
	{
		if(navigator.appName=="Netscape")
		{
			gbNav4=true;
			if(gnVerMajor>=5)
				gbNav6=true;
		}
		gbIE4=(navigator.appName.indexOf("Microsoft")!=-1);
	}
	if(gbNav6)
	{
		var nPos=gAgent.indexOf("gecko");
		if(nPos!=-1)
		{
			var nPos2=gAgent.indexOf("/", nPos);
			if(nPos2!=-1)
			{
				var nVersion=parseFloat(gAgent.substring(nPos2+1));
				if(nVersion>=20010726)
				{
					gbNav61=true;
					if (nVersion>=20020823)
						gbNav7=true;
				}
			}
		}
	}else if(gbIE4)
	{
		var nPos=gAgent.indexOf("msie");
		if(nPos!=-1)
		{
			var nVersion=parseFloat(gAgent.substring(nPos+5));
			if(nVersion>=5)
			{
				gbIE5=true;
				if(nVersion>=5.5)
				{
					gbIE55=true;
					if(nVersion>=9)
					{
						gbIE9 = true;
						if(nVersion>=10)
							gbIE10=true;
					}
				}
			}
		}
	}
}
else if (gbOpera)
{
	var nPos = gAgent.indexOf("opera");
	if(nPos!=-1)
	{
		var nVersion=parseFloat(gAgent.substring(nPos+6));
		if(nVersion>=6)
		{
			gbOpera6=true;
			if(nVersion>=7)
				gbOpera7=true;
		}
	}
}
else if (gbKonqueror)
{
	var nPos = gAgent.indexOf("konqueror");
	if(nPos!=-1)
	{
		var nVersion = parseFloat(gAgent.substring(nPos+10));
		if (nVersion >= 3)
		{
			gbKonqueror3=true;
		}
	}
}
if(gbSafari)
{
	var nPos = gAgent.indexOf("version/");
	if(nPos!=-1)
	{
		var nVersion = parseFloat(gAgent.substring(nPos+8,nPos+9));
		if (nVersion >= 3)
		{
			gbSafari3=true;
		}
	}
}
if(gbChrome)
{
	//for the time being use same tests as safari
	gbSafari = true ;
	gbSafari3=true;
}
var gbWhVer=true;
