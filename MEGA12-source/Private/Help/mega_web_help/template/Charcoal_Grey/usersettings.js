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

//Variables to override in settings
var useTOC = true;
var useGLO = true;
var useIDX = true;
var delayLoadIdx = true;
var delayLoadGlo = true;
var useFilter = true;
var useFacebook = false;
var useTwitter = false;
var useSocial = true;
var defaultPane = 'toc';
var mobileTocDrilldown = false;
var useANDsearch = true;
var maxResults = 15;	
var desktopSidebarVisibleDefault = true;
var searchLocationBoolean = true;
var phone_max_width = 941;
var tablet_max_width = 1295;
var titleColor = "#dddddd";
var backgroundColor = "#232323";
var logo = "logo.png";
var fontFamily = "\"Trebuchet MS\", Arial, sans-serif";

(function() {
	var mobileMenu, rh, features;

	rh = window.rh;
	features = rh.model.get(rh.consts('KEY_FEATURE')) || {};

	//Publish which panes are available
	features.toc = useTOC;
	features.idx = useIDX;
	features.glo = useGLO;
	features.delay_load_idx = delayLoadIdx;
	features.delay_load_glo = delayLoadGlo;
	features.filter = useFilter;
	
	rh.model.publish(rh.consts('KEY_DEFAULT_TAB'), defaultPane);

	//If there are are no panes available
	if (!useTOC && !useGLO && !useIDX) {
		mobileMenu = false;
	} else {
		mobileMenu = true;
	}

	rh.model.publish('l.mobile_menu_enabled', mobileMenu);

	//Set the TOC type for mobile: Regular (false: default) or Drill down (true).
	rh.model.publish(rh.consts("KEY_MOBILE_TOC_DRILL_DOWN"), mobileTocDrilldown);

	//Should desktop sidebar be hidden by default?
	rh.model.publish('l.desktop_sidebar_visible', desktopSidebarVisibleDefault);

	//Does the user want search results in the sidebar or over topic content?
	var searchLocation = (searchLocationBoolean === true) ? 'tabbar' : 'content';
	rh.model.publish(rh.consts('KEY_DEFAULT_SEARCH_LOCATION'), searchLocation);

	//Number of search results to be loaded at once.
	rh.consts('MAX_RESULTS', '.l.maxResults');
	rh.model.publish(rh.consts('MAX_RESULTS'), maxResults);

	//Choose whether to use the AND search option in the layout
	features.andsearch = useANDsearch;

	/* This layout has single page and so handles search */
	rh.model.publish(rh.consts("KEY_CAN_HANDLE_SEARCH"), true);

	//Hide the sidebar when there are no panes
	if (!useTOC && !useGLO && !useIDX && !searchLocationBoolean && !useFilter) {
		rh.model.publish('l.desktop_sidebar_available', false);
	} else {
		rh.model.publish('l.desktop_sidebar_available', true);
	}

	var desktop = 'screen and (min-width: '+ (tablet_max_width + 1) +'px)';
	var tablet = 'screen and (min-width: '+ (phone_max_width + 1) +'px) and (max-width: '+ tablet_max_width +'px)';
	var phone = 'screen and (max-width: '+ phone_max_width +'px)';
	var screens = {
	  desktop: { media_query: desktop },
	  tablet: { media_query: tablet },
	  phone: { media_query: phone },
	  ios: {user_agent: /(iPad|iPhone|iPod)/g}
	};
	rh.model.publish(rh.consts('KEY_SCREEN'), screens);
	
	//Social widgets
	if(document.location.toString().indexOf("file:///") != -1) {//Always disable buttons for local output
		useFacebook = false;
		useTwitter = false;
		useSocial = false;
	}
	if(!useFacebook && !useTwitter) {
		useSocial = false;
	}
	
	features.facebook = useFacebook;
	features.twitter = useTwitter;
	features.social = useSocial;

	if(useFacebook) {//Facebook Button
		rh.model.subscribe(rh.consts('KEY_TOPIC_TITLE'), updateFacebookButton);
		rh.model.subscribe('l.social_opened', updateFacebookButton);
	}
	if(useTwitter) {//Twitter button
		rh.model.subscribe(rh.consts('KEY_TOPIC_TITLE'), updateTwitterButton);
		rh.model.subscribe('l.social_opened', updateTwitterButton);
	}
	function updateFacebookButton() {
		var iframeID, url, iframe, topicUrl;
		
		topicUrl = rh.model.get(rh.consts('KEY_TOPIC_URL'));
		
		if(!rh.model.get('l.social_opened') || !topicUrl) {
			return;
		}
		
		if(document.location.toString().indexOf("file://") != -1) {
			return;//No FB button on local content
		}
		
		iframeID = "bf-iframe";
		
		//The URL for the Facebook iFrame
		url = 'http://www.facebook.com/plugins/share_button.php?href='+
			  topicUrl +
			  '&layout=button_count&action=like&show_faces=false&share=false&height=21';
		
		iframe = document.getElementById(iframeID);
		iframe.setAttribute("src", url);
		
	}
	function updateTwitterButton() {
		var holderID, holder, newLink, textNode, topicUrl;
		
		topicUrl = rh.model.get(rh.consts('KEY_TOPIC_URL'));
		
		if(!rh.model.get('l.social_opened') || !topicUrl) {
			return;
		}
		
		if(document.location.toString().indexOf("file://") != -1) {
			return;//No Tweet button on local content
		}
		
		holderID = 'twitter-holder';
		holder = document.getElementById(holderID);
		
		//Remove existing children
		while (holder.firstChild) {
			holder.removeChild(holder.firstChild);
		}
		
		//Add tweet button
		newLink = document.createElement('a');
		newLink.setAttribute("href", 'https://twitter.com/share');
		newLink.setAttribute("class", 'twitter-share-button');
		newLink.setAttribute("data-url", topicUrl);
		newLink.setAttribute("data-text", rh.model.get(rh.consts('KEY_TOPIC_TITLE')));
		
		textNode = document.createTextNode("Tweet");
		newLink.appendChild(textNode);
		
		holder.appendChild(newLink);
		
		if(window.twttr) {
			window.twttr.widgets.load();
		}
	}
	
	rh.model.publish(rh.consts('KEY_FEATURE'), features);
	rh.model.publish(rh.consts("KEY_LAYOUT_VERSION"), "2.0");
}.call(this));

