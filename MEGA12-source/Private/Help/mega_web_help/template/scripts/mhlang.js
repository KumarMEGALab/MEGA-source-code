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

var S=new Array();
var A=new Array();
var gaFtsStop=["a", "about", "after", "against", "all", "also", "among", "an", "and", "are", "as", "at", "be", "became", "because", "been", "between", "but", "by", "can", "come", "do", "during", "each", "early", "for", "form", "found", "from", "had", "has", "have", "he", "her", "his", "however", "in", "include", "into", "is", "it", "its", "late", "later", "made", "many", "may", "me", "med", "more", "most", "near", "no", "non", "not", "of", "on", "only", "or", "other", "over", "several", "she", "some", "such", "than", "that", "the", "their", "then", "there", "these", "they", "this", "through", "to", "under", "until", "use", "was", "we", "were", "when", "where", "which", "who", "with", "you"];
var gaFtsStem=new Array();
var gbWhLang=false;
var gsSubstrSrch = 0;

S[0]=0
S[1]=0
S[2]=0
S[3]=0
S[4]=0
S[5]=0
S[6]=0
S[7]=0
S[8]=0
S[137]=0
S[131]=0
S[145]=0
S[132]=0
S[146]=0
S[14]=0
S[15]=0
S[16]=0
S[17]=0
S[18]=0
S[19]=0
S[20]=0
S[21]=0
S[22]=0
S[23]=0
S[24]=0
S[25]=0
S[26]=0
S[27]=0
S[28]=0
S[29]=0
S[30]=0
S[31]=0
S[147]=0
S[138]=0
S[127]=0
S[128]=0
S[129]=0
S[130]=0
S[139]=0
S[159]=0
S[148]=0
S[158]=0
S[149]=0
S[140]=0
S[150]=0
S[141]=0
S[136]=0
S[135]=0
S[144]=0
S[157]=0
S[153]=0
S[134]=0
S[156]=0
S[155]=0
S[154]=0
S[152]=0
S[143]=0
S[151]=0
S[142]=0
S[9]=59
S[10]=60
S[11]=61
S[12]=62
S[13]=63
S[133]=64
S[160]=65
S[32]=65
S[96]=67
S[180]=68
S[94]=69
S[175]=70
S[168]=71
S[184]=72
S[95]=73
S[173]=74
S[45]=75
S[44]=76
S[59]=77
S[58]=78
S[33]=79
S[161]=80
S[63]=81
S[191]=82
S[46]=83
S[183]=84
S[39]=85
S[34]=86
S[171]=87
S[187]=88
S[40]=89
S[41]=90
S[91]=91
S[93]=92
S[123]=93
S[125]=94
S[167]=95
S[182]=96
S[169]=97
S[174]=98
S[64]=99
S[42]=100
S[47]=101
S[92]=102
S[38]=103
S[35]=104
S[37]=105
S[176]=106
S[43]=107
S[177]=108
S[247]=109
S[215]=110
S[60]=111
S[61]=112
S[62]=113
S[172]=114
S[124]=115
S[166]=116
S[126]=117
S[164]=118
S[162]=119
S[36]=120
S[163]=121
S[165]=122
S[48]=123
S[185]=124
S[49]=124
S[189]=126
S[188]=127
S[178]=128
S[50]=128
S[179]=130
S[51]=130
S[190]=132
S[52]=133
S[53]=134
S[54]=135
S[55]=136
S[56]=137
S[57]=138
S[97]=139
S[65]=139
S[170]=139
S[193]=139
S[225]=139
S[192]=139
S[224]=139
S[259]=139
S[258]=139
S[194]=139
S[226]=139
S[461]=139
S[462]=139
S[197]=139
S[229]=139
S[228]=139
S[196]=139
S[479]=139
S[478]=139
S[227]=139
S[195]=139
S[481]=139
S[480]=139
S[260]=139
S[261]=139
S[257]=139
S[256]=139
S[198]=166
S[230]=166
S[482]=166
S[483]=166
S[66]=170
S[98]=170
S[384]=172
S[385]=173
S[387]=174
S[386]=174
S[99]=176
S[67]=176
S[263]=176
S[262]=176
S[264]=176
S[265]=176
S[269]=176
S[268]=176
S[267]=176
S[266]=176
S[199]=176
S[231]=176
S[392]=188
S[391]=188
S[100]=190
S[68]=190
S[270]=190
S[271]=190
S[273]=190
S[272]=190
S[240]=190
S[208]=190
S[499]=198
S[497]=198
S[498]=198
S[454]=198
S[452]=198
S[453]=198
S[393]=204
S[394]=205
S[396]=206
S[395]=206
S[101]=208
S[69]=208
S[233]=208
S[201]=208
S[232]=208
S[200]=208
S[277]=208
S[276]=208
S[234]=208
S[202]=208
S[282]=208
S[283]=208
S[203]=208
S[235]=208
S[279]=208
S[278]=208
S[280]=208
S[281]=208
S[274]=208
S[275]=208
S[477]=228
S[398]=228
S[399]=230
S[400]=231
S[70]=232
S[102]=232
S[401]=234
S[402]=234
S[103]=236
S[71]=236
S[287]=236
S[286]=236
S[285]=236
S[284]=236
S[487]=236
S[486]=236
S[288]=236
S[289]=236
S[291]=236
S[290]=236
S[485]=248
S[484]=248
S[403]=250
S[404]=251
S[418]=252
S[419]=252
S[104]=254
S[72]=254
S[292]=254
S[293]=254
S[294]=254
S[295]=254
S[405]=260
S[73]=261
S[105]=261
S[205]=261
S[237]=261
S[236]=261
S[204]=261
S[300]=261
S[301]=261
S[206]=261
S[238]=261
S[464]=261
S[463]=261
S[239]=261
S[207]=261
S[296]=261
S[297]=261
S[304]=261
S[303]=261
S[302]=261
S[299]=261
S[298]=261
S[306]=282
S[307]=282
S[305]=284
S[407]=285
S[406]=286
S[106]=287
S[74]=287
S[309]=287
S[308]=287
S[496]=287
S[75]=292
S[107]=292
S[488]=292
S[489]=292
S[311]=292
S[310]=292
S[408]=298
S[409]=298
S[108]=300
S[76]=300
S[313]=300
S[314]=300
S[318]=300
S[317]=300
S[315]=300
S[316]=300
S[321]=300
S[322]=300
S[320]=310
S[319]=310
S[457]=312
S[455]=312
S[456]=312
S[410]=315
S[411]=316
S[77]=317
S[109]=317
S[110]=319
S[78]=319
S[323]=319
S[324]=319
S[327]=319
S[328]=319
S[209]=319
S[241]=319
S[325]=319
S[326]=319
S[458]=329
S[459]=329
S[460]=329
S[413]=332
S[414]=333
S[331]=334
S[330]=334
S[111]=336
S[186]=336
S[79]=336
S[211]=336
S[243]=336
S[242]=336
S[210]=336
S[335]=336
S[334]=336
S[212]=336
S[244]=336
S[465]=336
S[466]=336
S[214]=336
S[246]=336
S[337]=336
S[336]=336
S[213]=336
S[245]=336
S[216]=336
S[248]=336
S[490]=336
S[491]=336
S[492]=336
S[493]=336
S[333]=336
S[332]=336
S[417]=336
S[416]=336
S[339]=365
S[338]=365
S[390]=367
S[415]=368
S[112]=369
S[80]=369
S[420]=371
S[421]=371
S[81]=373
S[113]=373
S[312]=375
S[114]=376
S[82]=376
S[340]=376
S[341]=376
S[344]=376
S[345]=376
S[342]=376
S[343]=376
S[422]=384
S[83]=385
S[115]=385
S[346]=385
S[347]=385
S[348]=385
S[349]=385
S[352]=385
S[353]=385
S[350]=385
S[351]=385
S[383]=385
S[223]=396
S[425]=397
S[426]=398
S[84]=399
S[116]=399
S[356]=399
S[357]=399
S[354]=399
S[355]=399
S[446]=405
S[359]=406
S[358]=406
S[427]=408
S[429]=409
S[428]=409
S[430]=411
S[85]=412
S[117]=412
S[218]=412
S[250]=412
S[249]=412
S[217]=412
S[365]=412
S[364]=412
S[251]=412
S[219]=412
S[467]=412
S[468]=412
S[367]=412
S[366]=412
S[220]=412
S[252]=412
S[471]=412
S[472]=412
S[475]=412
S[476]=412
S[473]=412
S[474]=412
S[470]=412
S[469]=412
S[368]=412
S[369]=412
S[361]=412
S[360]=412
S[370]=412
S[371]=412
S[362]=412
S[363]=412
S[431]=412
S[432]=412
S[412]=446
S[433]=447
S[86]=448
S[118]=448
S[434]=450
S[119]=451
S[87]=451
S[372]=451
S[373]=451
S[120]=455
S[88]=455
S[121]=457
S[89]=457
S[253]=457
S[221]=457
S[374]=457
S[375]=457
S[376]=457
S[255]=457
S[436]=465
S[435]=465
S[90]=467
S[122]=467
S[377]=467
S[378]=467
S[381]=467
S[382]=467
S[379]=467
S[380]=467
S[397]=475
S[438]=476
S[437]=476
S[439]=478
S[495]=478
S[494]=478
S[441]=481
S[440]=481
S[442]=483
S[254]=484
S[222]=484
S[447]=486
S[443]=487
S[423]=488
S[424]=488
S[444]=490
S[445]=490
S[388]=492
S[389]=492
S[329]=494
S[448]=495
S[449]=496
S[450]=497
S[451]=498
S[181]=499

gaFtsStem[0] = "ed";
gaFtsStem[1] = "es";
gaFtsStem[2] = "er";
gaFtsStem[3] = "e";
gaFtsStem[4] = "s";
gaFtsStem[5] = "ingly";
gaFtsStem[6] = "ing";
gaFtsStem[7] = "ly";


var gsBiggestChar="䶮";
function getBiggestChar()
{
	return gsBiggestChar;
}

function getCharCode(code)
{
	var charCode = S[code];
	if ((typeof(charCode) != 'undefined')&&(charCode != null))
	{
		return charCode ;
	}
	return code;
}

function getAccentCharOrder(str, i)
{
	var code=str.charCodeAt(i);
	if ((typeof(A[code]) != 'undefined')&&(A[code] != null))
	{
		return A[code] ;
	}
	else if ((typeof(S[code]) != 'undefined')&&(S[code] != null))
	{
		return S[code] ;
	}
	return code;	
}

function compare(strText1,strText2)
{
	for(var i=0;i<strText1.length && i<strText2.length;i++)
	{
		var code1 = strText1.charCodeAt(i);
		var code2 = strText2.charCodeAt(i);
		if(code1 == code2)
			continue;
		var charCode1 = getCharCode(code1);
		var charCode2 = getCharCode(code2);
			
		if(charCode1<charCode2) return -1;
		if(charCode1>charCode2) return 1;
	}
	if(strText1.length<strText2.length) return -1;
	if(strText1.length>strText2.length) return 1;
	
	//compare accent
	/*for(var i=0;i<strText1.length ;i++)
	{
		var charCode1 = getAccentCharOrder(strText1,i);
		var charCode2 = getAccentCharOrder(strText2,i);
			
		if(charCode1<charCode2) return -1;
		if(charCode1>charCode2) return 1;
	}*/	
	return 0;	
}

gbWhLang=true;
