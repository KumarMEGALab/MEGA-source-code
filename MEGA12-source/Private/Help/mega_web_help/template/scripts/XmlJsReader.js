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

var gXMLBuffer = null;
var gFileNameToXMLMap = new Object();
var xmlJsReader = new XmlJsReader();

function XmlInfo(xmlPath, oFunCallback, args) {
    this.sXmlPath = xmlPath;
    this.callback = oFunCallback;
    this.cbargs = args;
}

function XmlJsReader() {
    this.queue = new MhQueue();
    this.bLoading = false;

    this.getJsNameFromXmlName = function (xmlPath) {
        var indx = xmlPath.lastIndexOf(".xml");
        if (indx != -1) {
            var jsPath = xmlPath.substring(0, indx);
            jsPath += "_xml.js";
            return jsPath;
        }
		return xmlPath;
    }
    /*use relative path for xmlPath*/
    this.loadFile = function (xmlPath, oFunCallback, args) {
        this.queue.enqueue(new XmlInfo(xmlPath, oFunCallback, args));
        this.loadFromQueue();
    }

    this.loadFromQueue = function () {
        if (this.queue.isEmpty() || this.bLoading) {
            return;
        }
        else {
            var xmlInfo = this.queue.peek();
            if (typeof (gFileNameToXMLMap[xmlInfo.sXmlPath]) == 'undefined') {
                var jsPath = this.getJsNameFromXmlName(xmlInfo.sXmlPath);
                this.loadScript(jsPath, this.onScriptLoaded);
            }
            else {
                this.onScriptLoaded();
            }
        }
    }

    this.onScriptLoaded = function () {
        var xmlInfo = xmlJsReader.queue.dequeue();
        if (typeof(gFileNameToXMLMap[xmlInfo.sXmlPath]) == 'undefined' && gXMLBuffer != null) {
            gFileNameToXMLMap[xmlInfo.sXmlPath] = gXMLBuffer;
        }
        var xmlDoc = null;
        if (typeof (gFileNameToXMLMap[xmlInfo.sXmlPath]) != 'undefined') {
            if (window.DOMParser) {
                var parser = new DOMParser();
                xmlDoc = parser.parseFromString(gFileNameToXMLMap[xmlInfo.sXmlPath], "text/xml");
            }
            else {
                xmlDoc = new ActiveXObject("Microsoft.XMLDOM");
                xmlDoc.async = false;
                var indx = gFileNameToXMLMap[xmlInfo.sXmlPath].indexOf("<?xml");
                if (indx != -1) {
                    indx = gFileNameToXMLMap[xmlInfo.sXmlPath].indexOf("?>", indx);
                    if (indx != -1) {
                        var strXML = gFileNameToXMLMap[xmlInfo.sXmlPath].substr(indx + 2);
                        xmlDoc.loadXML(strXML);
                    }
                }
                else {
                    xmlDoc.loadXML(gFileNameToXMLMap[xmlInfo.sXmlPath]);
                }
            }
        }
        gXMLBuffer = null;
        xmlJsReader.bLoading = false;

        if (xmlInfo.callback)
            xmlInfo.callback(xmlDoc, xmlInfo.cbargs);

        xmlJsReader.loadFromQueue();
    }

    this.loadScript = function (sScriptSrc, onScriptLoadedCB) {
        this.bLoading = true;
        var oHead = document.getElementsByTagName('head')[0];
        var oScript = document.createElement('script');
        oScript.type = 'text/javascript';
        oScript.charset = "utf-8";
		oScript.src = sScriptSrc;

        // IE 6 & 7
        if (oScript.readyState) {
            oScript.onreadystatechange = function () {
                if (oScript.readyState == 'loaded' ||
                    oScript.readyState == 'complete') {
                    onScriptLoadedCB();
                }
            }
        }
        else {
            oScript.onload = onScriptLoadedCB;
            oScript.onerror = onScriptLoadedCB;
        }

        oHead.appendChild(oScript);
    }
}
