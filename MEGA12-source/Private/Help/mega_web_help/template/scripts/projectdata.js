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

// Publish project specific data
(function() {
rh = window.rh;
model = rh.model;

rh.consts('DEFAULT_TOPIC', encodeURI("#Preface.htm".substring(1)));
rh.consts('HOME_FILEPATH', encodeURI("index.htm"));
rh.consts('START_FILEPATH', encodeURI('index.htm'));
rh.consts('HELP_ID', 'D5F6EA97-2434-4A91-928A-65B4CF27229C' || 'preview');
rh.consts('LNG_STOP_WORDS', ["a", "about", "after", "against", "all", "also", "among", "an", "and", "are", "as", "at", "be", "became", "because", "been", "between", "but", "by", "can", "come", "do", "during", "each", "early", "for", "form", "found", "from", "had", "has", "have", "he", "her", "his", "however", "in", "include", "into", "is", "it", "its", "late", "later", "made", "many", "may", "me", "med", "more", "most", "near", "no", "non", "not", "of", "on", "only", "or", "other", "over", "several", "she", "some", "such", "than", "that", "the", "their", "then", "there", "these", "they", "this", "through", "to", "under", "until", "use", "was", "we", "were", "when", "where", "which", "who", "with", "you"]);
rh.consts('LNG_SUBSTR_SEARCH', 0);

model.publish(rh.consts('KEY_DIR'), "ltr");
model.publish(rh.consts('KEY_LNG_NAME'), "en_US");
model.publish(rh.consts('KEY_LNG'), {"SearchResultsPerScreen":"Search results per page","Reset":"Reset","SyncToc":"SyncToc","HomeButton":"Home","WebSearchButton":"WebSearch","GlossaryFilterTerms":"Filter Terms","HighlightSearchResults":"Highlight Search Results","ApplyTip":"Apply","Welcome_header":"Welcome to our Help Center","WebSearch":"WebSearch","Show":"Show","ShowAll":"All","EnableAndSearch":"Display results with all search words","Welcome_text":"What can we help you with today?","Next":"Next","PreviousLabel":"Previous","NoScriptErrorMsg":"Enable JavaScript support in the browser to view this page.","Print":"Print","Contents":"Contents","Search":"-Search-","Hide":"Hide","Canceled":"Canceled","ShowHide":"Show/Hide","Loading":"Loading...","EndOfResults":"End of search results.","favoritesLabel":"Favorites","Logo":"Logo","ContentFilterChanged":"Content filter is changed, search again","SidebarToggleTip":"Expand/Collapse","Logo/Author":"Powered By","JS_alert_LoadXmlFailed":"Failed to load XML file","Searching":"Searching...","SearchTitle":"Search","Copyright":"© Copyright 2017. All rights reserved.","favoritesNameLabel":"Name","Disabled Next":">>","JS_alert_InitDatabaseFailed":"Failed to initialize database","Cancel":"Cancel","unsetAsFavorite":"Unset as favorite","nofavoritesFound":"You have not marked any pages as favorite.","UnknownError":"Unknown error","ResultsFoundText":"%1 result(s) found for %2","FilterIntro":"Please select your filter(s):","Index":"Index","Seperate":"|","SearchPageTitle":"Search Results","TopicsNotFound":"No results found","setAsFavorites":"Add to Favorites","setAsFavorite":"Set as favorite","Glossary":"Glossary","Filter":"Filter","SearchButtonTitle":"Search for...","NextLabel":"Next","TableOfContents":"Table of Contents","HideAll":"Hide All","Disabled Prev":"<<","SearchOptions":"Search Options","Back":"Back","Prev":"Previous","OpenLinkInNewTab":"Open in new tab","JS_alert_InvalidExpression_1":"The search string you typed is not valid.","IndexFilterKewords":"Filter Keywords","IeCompatibilityErrorMsg":"This page cannot be viewed in Internet Explorer 8 or earlier version.","NavTip":"Menu","ToTopTip":"Go to top","FavoriteBoxTitle":"Favorites","ShowTopicInContext":"Click here to see this page in full context"});

model.publish(rh.consts('KEY_HEADER_DEFAULT_TITLE_COLOR'), "#ffffff");
model.publish(rh.consts('KEY_HEADER_DEFAULT_BACKGROUND_COLOR'), "#025172");
model.publish(rh.consts('KEY_LAYOUT_DEFAULT_FONT_FAMILY'), "\"Trebuchet MS\", Arial, sans-serif");

model.publish(rh.consts('KEY_HEADER_TITLE'), "MEGAX-Help");
model.publish(rh.consts('KEY_HEADER_TITLE_COLOR'), "#dddddd");
model.publish(rh.consts('KEY_HEADER_BACKGROUND_COLOR'), "#232323");
model.publish(rh.consts('KEY_HEADER_LOGO_PATH'), "template/Charcoal_Grey/logo.png");
model.publish(rh.consts('KEY_LAYOUT_FONT_FAMILY'), "\"Trebuchet MS\", Arial, sans-serif");
model.publish(rh.consts('KEY_HEADER_HTML'), "<div class='topic-header'>\
  <div class='logo' onClick='rh._.redirectToLayout()'>\
    <img src='#{logo}' />\
  </div>\
  <div class='nav'>\
    <div class='title' title='#{title}'>\
      <span onClick='rh._.redirectToLayout()'>#{title}</span>\
    </div>\
    <div class='gotohome' title='#{tooltip}' onClick='rh._.redirectToLayout()'>\
      <span>#{label}</span>\
    </div></div>\
  </div>\
<div class='topic-header-shadow'></div>\
");
model.publish(rh.consts('KEY_HEADER_CSS'), ".topic-header { background-color: #{background-color}; color: #{color}; width: calc(100%); height: 3em; position: fixed; left: 0; top: 0; font-family: #{font-family}; display: table; box-sizing: border-box; }\
.topic-header-shadow { height: 3em; width: 100%; }\
.logo { cursor: pointer; padding: 0.2em; height: calc(100% - 0.4em); text-align: center; display: table-cell; vertical-align: middle; }\
.logo img { max-height: 100%; display: block; }\
.nav { width: 100%; display: table-cell; }\
.title { width: 40%; height: 100%; float: left; line-height: 3em; cursor: pointer; }\
.gotohome { width: 60%; float: left; text-align: right; height: 100%; line-height: 3em; cursor: pointer; }\
.title span, .gotohome span { padding: 0em 1em; white-space: nowrap; text-overflow: ellipsis; overflow: hidden; display: block; }");

})();
