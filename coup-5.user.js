// ==UserScript==
// @name			Coup d'Bungie 5 for Firefox
// @namespace	   http://userscripts.org/scripts/show/97979
// @description		Personlize your bungie.net experience
// @version	 		5.3.5.2
// @include		 http*://*bungie.net/*
// @exclude		 http*://*bungie.net/*createpost.aspx*
// @exclude		 http*://*bungie.net/Account/Playtest/*
// @exclude		 http*://*bungie.net/Account/Settings.aspx*
// @author	  		dazarobbo
// @copyright		2011, dazarobbo
// @contributor		Iggyhopper
// @contributor		Tidus Zero
// @require			http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js
// @require			http://dohpaz.com/flydom/js/jquery.flydom-3.1.1.js
// @require			http://shou.dyndns-server.com/file/upload/coup5/jquerywheelcolorpickermin.js
// @license 		(CC) Attribution Non-Commercial Share Alike; http://creativecommons.org/licenses/by-nc-sa/3.0/
// ==/UserScript==

//New Console
var Console = {
	Log:function(a){
		if(CoupDBungie.Debug){
			GM_log(a);
		}
	}
}

//New String members
String.HTMLEncode = function(a){
	return a.replace(/&/g,"&amp;").replace(/"/g,"&quot;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/'/g,"&#39;");
}
String.prototype.HTMLEncode = function(){
	return String.HTMLEncode(this);
}

String.ToHex = function(s){
	s=s.toString().toLowerCase().replace(/[^a-f0-9]/g,"");
	if(s.length==3){
		s=s[0].concat(s[0],s[1],s[1],s[2],s[2]);
	}
	return parseInt(s,16);
}
String.prototype.ToHex = function(){
	return String.ToHex(this);
}


//New Number members
Number.prototype.HTMLEncode = function(){
	return String.HTMLEncode(this.toString());
}

Number.ToRGB = function(n){
	var r=(n&0xff0000)>>16;
	var g=(n&0x00ff00)>>8;
	var b=(n&0x0000ff);
	return {R:r,G:g,B:b}
}
Number.prototype.ToRGB = function(){
	return Number.ToRGB(this);
}


//New Array members
Array.Add = function(a, o){
	a.push(o);
}
Array.prototype.Add = function(o){
	Array.Add(this,o);
}

Array.Contains = function(a, o){
	var l=a.length;
	for(var i=0;i<l;i++){
		if(a[i]===o){
			return true;
		}
	}
	return false;
}
Array.prototype.Contains = function(o){
	return Array.Contains(this,o);
}

Array.DeleteAll = function(a, o){
	var r=false;
	for(var i=a.length-1;i>=0;i--){
		if(a[i]===o){
			a.splice(i,1);
			r=true;
		}
	}
	return r;
}
Array.prototype.DeleteAll = function(o){
	return Array.DeleteAll(this,o);
}

Array.Delete = function(a, o){
	var l=a.length;
	for(var i=0;i<l;i++){
		a.splice(i,1);
		return true;
	}
	return false;
}
Array.prototype.Delete = function(o){
	return Array.Delete(this,o);
}

Array.PickRandom = function(a){
	return a[Math.floor(Math.random()*a.length)];
}
Array.prototype.PickRandom = function(){
	return Array.PickRandom(this);
}

Array.Clear = function(a){
	a.length=0;
}
Array.prototype.Clear = function(){
	Array.Clear(this);
}

Array.Distinct = function(a){
	var h={};
	var r=[];
	var l=a.length;
	for(var i=0;i<l;++i){
		if(!h.hasOwnProperty(a[i])) {
			h[a[i]]=true;
			r.push(a[i]);
		}
	}
	return r;
}
Array.prototype.Distinct = function(){
	var r=Array.Distinct(this);
	var l=r.length;
	this.Clear();
	for(var i=l-1;i>=0;i--){
		this.Add(r[i]);
	}
}

Array.Filter = function(a, b){
	var l=b.length;
	for(var i=0;i<l;i++){
		a.DeleteAll(a[i]);
	}
}
Array.prototype.Filter = function(arr){
	Array.Filter(this,arr)
}

Object.size = function(obj) {
	var size = 0, key;
	for (key in obj) {
		if (obj.hasOwnProperty(key)) size++;
	}
	return size;
};


//Browser: Revised 21st August, 2011
var Browser = {
	
	Memory:{
		Type:localStorage['coup5storagetype'],
		
		Set:function(name, value){
			Console.Log("Setting value (" + value + ") under name (" + name + ")");
			if(this.Type){
				try {
					GM_setValue(name, value);
				} catch(e) {
					alert('GreaseMonkey storage not supported by your userscript engine.');
				}
			} else {
				localStorage.setItem(name, value);
			}
			Console.Log("Finished setting value (" + value + ") under name (" + name + ")");
		},
		
		Get:function(name, defaultValue){
			Console.Log("Getting value named (" + name + ")");
			if(!this.Exists(name)){
				Console.Log(name + " was not found. Returning default value (" + defaultValue + ")");
				return defaultValue;
			}
			if(this.Type){
				try {
					var val = GM_getValue(name);
				} catch(e) {
					alert('GreaseMonkey storage not supported by your userscript engine.');
				}
			} else {
				var val = localStorage.getItem(name);
			}
			Console.Log("Found value named (" + name + "). Returning value (" + val + ")");
			return val;
		},
		
		Delete:function(name){
			Console.Log("Removing value named (" + name + ")");
			if(this.Type){
				try {
					GM_deleteValue(name);
				} catch(e) {
					alert('GreaseMonkey storage not supported by your userscript engine.');
				}
			} else {
				localStorage.removeItem(name);
			}
			Console.Log("Finished removing value named (" + name + ")");
		},
		
		Exists:function(name){
			Console.Log("Checking if value named (" + name + ") exists");
			if(this.Type){
				try {
					var temp = GM_listValues();
					var list = new Array();
				
					for(i = 0; i < temp.length; i++) {
						list[temp[i]] = true;
					}
					delete temp;
				} catch(e) {
					alert('GreaseMonkey storage not supported by your userscript engine.');
				}
			} else {
				var list = localStorage;
			}
			if(list[name]){
				Console.Log("Value named (" + name + ") exists");
				return true;
			}
			Console.Log("Value named (" + name + ") does not exist");
			return false;
		},
		
		DeleteAll:function(){
			Console.Log("Deleting all values");
			if(this.Type){
				try {
					var temp = GM_listValues();
				
					for(i = 0; i < temp.length; i++) {
						GM_deleteValue(temp[i]);
					}
				} catch(e) {
					alert('GreaseMonkey storage not supported by your userscript engine.');
				}
			} else {
				localStorage.clear();
			}
			Console.Log("Finished deleting all values");
		}
	},
	SupportsCoupDBungie:function(){
		Console.Log("Determining whether browser supports Coup d'Bungie");
		if(localStorage && XMLHttpRequest && JSON){
			Console.Log("Browser supports Coup d'Bungie 5");
			return true;
		}
		Console.Log("Browser does not support Coup d'Bungie 5");
		return false;
	},
	XHR:function(method, url, async, headers, onload, onerror, onreadystatechange){
		this._Init(method, url, async, headers, onload, onerror, onreadystatechange);
	}
	
}

Browser.XHR.prototype = {
	_Method:null,
	_Url:null,
	_Async:null,
	_RequestHeaders:null,
	_OnLoad:null,
	_OnError:null,
	_OnReadyStateChange:null,
	_ReadyState:null,
	_Status:null,
	_ResponseHeaders:null,
	_ResponseText:null,

	_Init:function(method, url, async, headers, onload, onerror, onreadystatechange){
		_Method = method;
		_Url = url;
		_Async = async;
		_RequestHeaders = headers;
		_OnLoad = onload ? onload : function(){};
		_OnError = onerror ? onerror : function(){};
		_OnReadyStateChange = onreadystatechange ? onreadystatechange : function(){};
	},

	Go:function(){

		var self = this;
		self.__OnLoad = _OnLoad;
		self.__OnError = _OnError;
		self.__OnReadyStateChange = _OnReadyStateChange;
		self.__Update = function(readyState, status, responseText){
		
			_ReadyState = readyState;
			_Status = status;
			_ResponseText = responseText;
		
			self.__OnReadyStateChange();
		
			if(_ReadyState == 4){
				if(_Status == 200){
					self.__OnLoad();
				}
				else{
					self.__OnError();
				}
			}
		
		}
		
		if(typeof GM_xmlhttpRequest == "function"){
		
			GM_xmlhttpRequest({
				method:_Method,
				url:_Url,
				synchronos:!_Async,
				header:_RequestHeaders,
				onload:function(response){
					//May need to add self.__Update here
				},
				onerror:function(response){
					//May need to add self.__Update here
				},
				onreadystatechange:function(response){
					self.__Update(response.readyState, response.status, response.responseText);
				}
			});
			
		}
		else if(typeof XMLHttpRequest == "function"){
		
			var xhr = new XMLHttpRequest();
			xhr.open(_Method, _Url, _Async);
			xhr.onreadystatechange = function(){
				self.__Update(this.readyState, this.status, this.responseText);
			}
			for(var key in _RequestHeaders){
				if(_RequestHeaders.hasOwnProperty(key)){
					xhr.setRequestHeader(key, _RequestHeaders[key]);
				}
			}
			xhr.send(null);
		
		}
		else{
			throw new Error("XHR not available");
		}
	
	},
	GetResponseText:function(){
		return _ResponseText;
	},
	GetStatus:function(){
		return _Status;
	},
	GetReadyState:function(){
		return _ReadyState;
	},
	GetResponseHeaders:function(){
		return _ResponseHeaders;
	},
	GetResponseJSON:function(){
		if(typeof JSON != "undefined"){
			var obj = null;
			try{
				obj = JSON.parse(_ResponseText);
			}
			catch(e){}
			return obj;
		}
		else{
			throw new Error("JSON not available");
		}
	},
	GetResponseXML:function(){
		return (new DOMParser()).parseFromString(_ResponseText, "text/xml");
	},
	GetMethod:function(){
		return _Method;
	},
	GetURL:function(){
		return _Url;
	},
	IsAsync:function(){
		return _Async;
	},
	WasAsync:function(){
		return IsAsync();
	},
	GetRequestHeaders:function(){
		return _RequestHeaders;
	}
}


//Options: Created 30th August, 2011
var Options = {
	Add:function(e, g, o, v){
		options = JSON.parse(Browser.Memory.Get(e, "{}"));
		options[g] = options[g] ? options[g] : {};
		options[g][o] = v;
		Browser.Memory.Set(e, JSON.stringify(options));
	},
	Get:function(e, g, o, f){
		options = JSON.parse(Browser.Memory.Get(e, "{}"));
		options[g] = options[g] != undefined ? options[g] : {};
		return options[g][o] != undefined ? options[g][o] : f;
	},
	Del:function(e, g, o){
		if(o) {
			options = JSON.parse(Browser.Memory.Get(e, "{}"));
			options[g] = options[g] ? options[g] : {};
			delete options[g][o];
			Console.Log('Deleting option: ' + o);
		} else {
			options = JSON.parse(Browser.Memory.Get(e, "{}"));
			delete options[g];
			Console.Log('Deleting group: ' + g);
		}
		Browser.Memory.Set(e, JSON.stringify(options));
	},
	Empty:function(e){
		Browser.Memory.Delete(e);
	}
}

//IgnoreSpawn: Created 14th October, 2011 (temporary placement)
function IgnoreSpawn(username)
{
	$("#coup5ignorespawn").remove();
	var elem = $("<div></div>");
	
	elem.attr({'id':'coup5ignorespawn', style:"height:0px; width:700px; margin:auto;"});
	elem.createAppend(
		"div", {style:{marginTop:"10%", position:"fixed", zIndex:9001}}, [
			"fieldset", {style:{backgroundColor:"rgba(0, 0, 0, 0.9)", border:"1px solid", padding:"5px"}}, [
				"legend", {style:{marginLeft:"5px", borderTop:"1px solid", borderLeft:"1px solid", borderRight:"1px solid", backgroundColor:"rgba(0, 0, 0, 0.9)"}}, [
					"b", null, "Ignore user: ",
					"span", {id:'coup5ignorespawnusername'}, username
				],
				"div", {style:{maxHeight:"450px", width:"700px", overflow:"auto"}, value:username}, [
					"div", {style:{display:"table-cell", width:"360px", padding:"5px", maxWidth:"50%"}}, [
						"table", null, [
							"tbody", null, [
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarUsernameText"}
									],
									"td", null, "Titlebar username text"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarUsernameTextColor"}
									],
									"td", null, "Titlebar username text color"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarTitleText"}
									],
									"td", null, "Titlebar title text"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarTitleTextColor"}
									],
									"td", null, "Titlebar title text color"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarMessageText"}
									],
									"td", null, "Titlebar message text"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarMessageTextColor"}
									],
									"td", null, "Titlebar message text color"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarGroupText"}
									],
									"td", null, "Titlebar group text"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarGroupTextColor"}
									],
									"td", null, "Titlebar group text color"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarBackgroundImage"}
									],
									"td", null, "Titlebar background image"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarBackgroundColor"}
									],
									"td", null, "Titlebar background color"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarBackgroundGradient"}
									],
									"td", null, "Titlebar background gradient"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"TitlebarBorder"}
									],
									"td", null, "Titlebar border"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"AvatarImage"}
									],
									"td", null, "Avatar image"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"AvatarBorder"}
									],
									"td", null, "Avatar border"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"PostBackgroundImage"}
									],
									"td", null, "Post background image"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"PostBackgroundImageRepeat"}
									],
									"td", null, "Post background repeat"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"PostBackgroundImageAttachment"}
									],
									"td", null, "Post background image attachment"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"PostBackgroundImagePosition"}
									],
									"td", null, "Post background position"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"PostBackgroundColor"}
									],
									"td", null, "Post background color"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"PostBackgroundGradient"}
									],
									"td", null, "Post background gradient"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"PostFont"}
									],
									"td", null, "Post font"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"PostFontColor"}
									],
									"td", null, "Post font color"
								],
								"tr", null, [
									"td", null, [
										"input", {type:"checkbox", name:"PostLinkColor"}
									],
									"td", null, "Post link color"
								]
							]
						]
					],
					"div", {style:{display:"table-cell", width:"360px", padding:"5px", maxWidth:"50%"}}, [
						"div", {style:{height:"25px"}}, [
							"span", null, "Titlebar opacity:",
							"input", {type:"range", value:1, name:"TitlebarBackgroundOpacity", placeholder:"Value between 0.0 - 1.0", style:{cssFloat:"right", borderStyle:"solid", borderRadius:"2px 2px", width:"150px"}}, null
						],
						"div", {style:{height:"25px"}}, [
							"span", null, "Avatar opacity:",
							"input", {type:"range", value:1, name:"AvatarOpacity", placeholder:"Value between 0.0 - 1.0", style:{cssFloat:"right", borderStyle:"solid", borderRadius:"2px 2px", width:"150px"}}, null
						],
						"div", {style:{height:"25px"}}, [
							"span", null, "Post opacity:",
							"input", {type:"range", value:1, name:"PostBackgroundOpacity", placeholder:"Value between 0.0 - 1.0", style:{cssFloat:"right", borderStyle:"solid", borderRadius:"2px 2px", width:"150px"}}, null
						]
					]
				],
				"div", null, [
					"input", {type:"button", value:"OK", onclick:function(){
						$("#coup5ignorespawn").remove();
					}}, null,
					"input", {type:"button", value:"All", onclick:function(){
						var ignoreList = Client.GetIgnoreList();
						$("#coup5ignorespawn input[type='checkbox']").each(function(){
							Options.Add('coup5ignorelist', username, $(this).attr('name'), false);
							$(this).attr('checked', false);
						});
						if(!ignoreList.Contains(username) && $(".removeIgnoreNotice").length < 1) {
							$("#coup5ignorespawn fieldset > div:last").createAppend(
								"span", {style:{cssFloat:"right", padding:"3px"}}, [
									"a", {className:"removeIgnoreNotice", id:"removeIgnoreNotice", href:"javascript:;", onclick:function(){
										ignoreList.push(username);
										Client.SetIgnoreList(ignoreList);
										$("#removeIgnoreNotice").parent().fadeOut();
										$("#removeIgnoreNotice").parent().remove();
									}}, "[Completely ignore " + username + "]",
									"a", {href:"javascript:;", style:{color:"darkRed"}, onclick:function(){
										$("#removeIgnoreNotice").parent().fadeOut();
										$("#removeIgnoreNotice").parent().remove();
									}}, " X"
								]
							);
						}
					}}, null,
					"input", {type:"button", value:"Reset options", onclick:function(){
						var ignoreList = Client.GetIgnoreList();
						$("#coup5ignorespawn input[type='checkbox']").each(function(){
							$(this).attr('checked', true);
						});
						$("#coup5ignorespawn input[type='range']").each(function(){
							$(this).val(1);
						});
						Options.Del('coup5ignorelist', username);
						if(ignoreList.Contains(username) && $(".fullIgnoreNotice").length < 1) {
							$("#coup5ignorespawn fieldset > div:last").createAppend(
								"span", {className:"fullIgnoreNotice", style:{cssFloat:"right", padding:"3px"}}, [
									"a", {id:"fullIgnoreNotice", href:"javascript:;", onclick:function(){
										ignoreList.splice(ignoreList.indexOf(username), 1);
										Client.SetIgnoreList(ignoreList);
										$("#fullIgnoreNotice").parent().fadeOut();
										$("#fullIgnoreNotice").parent().remove();
									}}, "[Remove " + username + " from the full ignore as well]",
									"a", {href:"javascript:;", style:{color:"darkRed"}, onclick:function(){
										$("#fullIgnoreNotice").parent().fadeOut();
										$("#fullIgnoreNotice").parent().remove();
									}}, " X"
								]
							);
						}
					}}
				]
			]
		]
	);
	$("body").prepend(elem);
	$("#coup5ignorespawn input[type='checkbox']").each(function(){
		var name = $(this).attr('name');
		var jsonObj = JSON.parse(Options.Get('coup5ignorelist', username, name, true));
		$(this).attr('checked', jsonObj);
	});
	$("#coup5ignorespawn input[type='range']").each(function(){
		var name = $(this).attr('name');
		var jsonObj = JSON.parse(Options.Get('coup5ignorelist', username, name, 1.0));
		$(this).attr('value', jsonObj);
	});
}

//IgnoreLive: Created 16 October, 2011
function IgnoreLive()
{
	//IgnoreSpawn links click event
	$(".coup5ignorespawn").live('click', function(){
		var name = $(this).attr('name');
		
		IgnoreSpawn(name);
	});
	//End of IgnoreSpawn links click event
	
	//IgnoreList checkbox event
	$("#coup5ignorespawn input[type='checkbox']").live('click', function(){
		var username = $("#coup5ignorespawnusername").text();
		var ignoreList = Client.GetIgnoreList();
		if($(this).is(':checked')){
			Options.Add('coup5ignorelist', username, $(this).attr('name'), true);
		}
		else{
			Options.Add('coup5ignorelist', username, $(this).attr('name'), false);
		}
		//alert($("#coup5ignorespawn input[type='checkbox']:not(:checked)").length + '/' + $("#coup5ignorespawn input[type='checkbox']").length);
		if($("#coup5ignorespawn input[type='checkbox']:not(:checked)").length == $("#coup5ignorespawn input[type='checkbox']").length && $(".fullIgnoreNotice").length < 1){
			$("#coup5ignorespawn fieldset > div:last").createAppend(
				"span", {className:"fullIgnoreNotice", style:{cssFloat:"right", padding:"3px"}}, [
					"a", {id:"fullIgnoreNotice", href:"javascript:;", onclick:function(){
						ignoreList.push(username);
						Client.SetIgnoreList(ignoreList);
						$("#fullIgnoreNotice").parent().fadeOut();
						$("#fullIgnoreNotice").parent().remove();
					}}, "[Completely ignore " + username + "]",
					"a", {href:"javascript:;", style:{color:"darkRed"}, onclick:function(){
						$("#fullIgnoreNotice").parent().fadeOut();
						$("#fullIgnoreNotice").parent().remove();
					}}, " X"
				]
			);
		}
	});
	//End IgnoreList checkbox event
	
	//IgnoreList range input event
	$("#coup5ignorespawn input[type='range']").live('keyup blur', function(){
		var username = $("#coup5ignorespawnusername").text();
		Options.Add('coup5ignorelist', username, $(this).attr('name'), parseFloat($(this).val()));
	});
	//End IgnoreList range input event
}

//CoupDBungie: Revised 21st August, 2011
var CoupDBungie = {
	
	Debug:true,
	
	Version:"5.3.5.1",
	Platform:"Firefox",
	Author:"dazarobbo",
	AuthorMemberID:2758679,
	
	Server:{
		
		Hosts:[
			"http://coup-srv-01.comeze.com/",
		],
		Path:"api/",
		Method:"GET",
		Responses:{
			OK:1,
			Error:2
		},
		
		CreateDefaultXHR:function(path, onload, onerror, onreadystatechange){
			Console.Log("Creating default XHR object");
			return new Browser.XHR(this.Method, this.Hosts[0] + this.Path + ((path) ? path : ""), true, null, onload, onerror, onreadystatechange);
		},
		Register:function(username, memberID, onload, onerror, onreadystatechange){
			var path = "Register?";
			if(username != null){
				path += "username=" + User.EncodeUsername(username);
			}
			else if(memberID != null){
				path += "memberID=" + memberID;
			}
			var xhr = this.CreateDefaultXHR(path, onload, onerror, onreadystatechange);
			Console.Log("Making a Register request to (" + xhr.GetURL() + ")");
			xhr.Go();
		},
		Report:function(username, key, subject, reason, onload, onerror, onreadystatechange){
			var path = "Report?username=" + User.EncodeUsername(username) + "&key=" + key + "&subject=" + User.EncodeUsername(subject) + "&reason=" + encodeURIComponent(reason);
			var xhr = this.CreateDefaultXHR(path, onload, onerror, onreadystatechange);
			Console.Log("Making a Report request to (" + xhr.GetURL() + ")");
			xhr.Go();
		},	
		GetStyles:function(usernames, onload, onerror, onreadystatechange){
			var path = "GetStyles?users=" + User.EncodeUsernameArray(usernames).join(",");
			var xhr = this.CreateDefaultXHR(path, onload, onerror, onreadystatechange);
			Console.Log("Making a GetStyles request to (" + xhr.GetURL() + ")");
			xhr.Go();
		},	
		PublishStyles:function(username, key, styles, onload, onerror, onreadystatechange){
			var path = "PublishStyles?username=" + User.EncodeUsername(username) + "&key=" + key + styles.ToPublishString();
			var xhr = this.CreateDefaultXHR(path, onload, onerror, onreadystatechange);
			Console.Log("Making a PublishStyles request to (" + xhr.url + ")");
			xhr.Go();
		},
		GetFeaturedStyles:function(onload, onerror, onreadystatechange){
			//To add
		},
		GetStyleHistory:function(username, key, onload, onerror, onreadystatechange){
			//To add
		}
		
	},
	
	Initialise:function(){
		if(Browser.SupportsCoupDBungie()){
			Cache.Initialise();
			//Other components (if they exist) should be initialised here as well
			return true;
		}
		else{
			throw new this.CoupDBungie5Exception("Browser cannot support this version of Coup d'Bungie 5");
		}
	},
	
	CoupDBungie5Exception:function(message){
		throw new Error(message ? message : "", "CoupDBungie5Exception");
	}

}


//Cache: Revised 22nd August, 2011
var Cache = {

	CACHE_NAME:"Coup5Cache",
	LAST_CACHE_DUMP_NAME:"Coup5Cache_LastDump",
	LAST_CACHE_DUMP:null,
	CACHE_INTERVAL_RATE:20, //Measured in minutes per hour (so for 20 mins, three cache dumps per hour)
	MAX_PAGES:200,
	WorkingSet:[],
	
	Initialise:function(){
		Console.Log("Initialising cache");
		this.Load();
		this.Management();
		Console.Log("Finished initialising cache");
	},
	
	Load:function(){
		try{
			this.WorkingSet = JSON.parse(Browser.Memory.Get(this.CACHE_NAME, undefined));
			Console.Log("Cache loaded successfully");
		}
		catch(e){
			Console.Log("An error occurred while loading the cache (" + e + "). Cache will now be deleted");
			Browser.Memory.Delete(this.CACHE_NAME); //Assume the cache variable is corrupt, so delete it
			this.WorkingSet = []; //... and set the local copy to an empty array (hard restart)
		}
		try{
			this.LAST_CACHE_DUMP = new Date(JSON.parse(Browser.Memory.Get(this.LAST_CACHE_DUMP_NAME, undefined)));
			Console.Log("Last cache dump time loaded");
		}
		catch(e){
			Browser.Memory.Delete(this.LAST_CACHE_DUMP_NAME);
			this.LAST_CACHE_DUMP = new Date();
			this.LAST_CACHE_DUMP.setTime(0);
		}
	},
	Save:function(){
		Console.Log("Saving cache");
		try{
			Browser.Memory.Set(this.CACHE_NAME, JSON.stringify(this.WorkingSet));
			Browser.Memory.Set(this.LAST_CACHE_DUMP_NAME, JSON.stringify(this.LAST_CACHE_DUMP));
			Console.Log("Cache saved successfully");
		}
		catch(e){
			Console.Log("Cache failed to save - reason: (" + e + ")");
		}
	},
	
	Management:function(){
	
		Console.Log("Performing cache management");
	
		function _DetermineInterval(date, rate){
			return Math.floor(date.getMinutes() / rate);
		}
	
		var now = new Date();
		if(	now.getFullYear() == this.LAST_CACHE_DUMP.getFullYear() &&
			now.getMonth() == this.LAST_CACHE_DUMP.getMonth() &&
			now.getDate() == this.LAST_CACHE_DUMP.getDate() &&
			now.getHours() == this.LAST_CACHE_DUMP.getHours() ){

			if(_DetermineInterval(now, this.CACHE_INTERVAL_RATE) > _DetermineInterval(this.LAST_CACHE_DUMP, this.CACHE_INTERVAL_RATE)){
				this.DeleteAll();
				Console.Log("Dumping cache");
				this.LAST_CACHE_DUMP = new Date();
			}
		}
		else if(now > this.LAST_CACHE_DUMP){
			this.LAST_CACHE_DUMP = new Date();
		}
		
		while(this.WorkingSet && this.WorkingSet.length > this.MAX_PAGES){
			this.WorkingSet.Delete(this.WorkingSet.PickRandom());
		}
		
		this.Save();
		
		Console.Log("Finished cache management");
		
	},
	
	Add:function(page){
		Console.Log("Adding page to working set");
		var existingPage = this.Get(page.Data.Username);
		if(existingPage != null){
			Console.Log("Found an existing page");
			existingPage = page;
			Console.Log("Page replaced");
		}
		else{
			this.WorkingSet.Add(page);
			Console.Log("Page added");
		}
	},
	Exists:function(username){
		Console.Log("Checking is page exists for (" + username + ")");
		if(this.Get(username) != null){
			Console.Log("Page exists for (" + username + ")");
			return true;
		}
		Console.Log("Page doesn't exist for (" + username + ")");
		return false;
	},
	Get:function(username){
		Console.Log("Getting page for (" + username + ")");
		for(var i=0; i<this.WorkingSet.length; i++){
			if(this.WorkingSet[i].Data.Username == username){
				this.WorkingSet[i].LastHit = new Date();
				return this.WorkingSet[i];
			}
		}
		Console.Log("Couldn't find a page for (" + username + ")");
		return null;
	},
	Delete:function(page){
		Console.Log("Removing page");
		this.WorkingSet.Delete(page);
	},
	DeleteAll:function(){
		this.WorkingSet.Clear();
	},
	
	Page:function(user){
		Console.Log("Creating a new cache page");
		this.Data = user;
		this.Date = new Date();
		this.LastHit = null;
		Console.Log("Finished creating a new cache page");
	}
	
}


//User: Good, User.ToEncodedUsernameArray is unused (28th June, 2011)
function User(username, id, styles, isCoupUser){
	this.Username = username;
	this.Id = id;
	this.Styles = styles;
	this.CoupUser = isCoupUser;
}
User.EncodeUsername = function(username){
	return escape(username);
}
User.ToEncodedUsernameArray = function(users){
	//users is an array of user objects
	usernames = [];
	for(var i=0; i<users.length; i++){
		usernames.Add(this.EncodeUsername(users[i].Username));
	}
	return usernames;
}
User.EncodeUsernameArray = function(usernameArr){
	var arr = [];
	for(var i=0; i<usernameArr.length; i++){


		arr.Add(User.EncodeUsername(usernameArr[i]));
	}
	return arr;
}


//Styles: Good, Styles.Sanitise is unused (28th June, 2011)
function Styles(){
	
	this.Sanitise = function(){
		for(var s in this){
			if(this.hasOwnProperty(s) && typeof this[s] === "string"){
				this[s] = this[s].HTMLEncode();
			}
		}
	}
	this.ToPublishString = function(){
		var str = "";
		for(var s in this){
			if(typeof this[s] !== "function"){
				str += "&" + s + "=" + encodeURIComponent(this[s] != undefined ? this[s] : "");
			}
		}
		return str;
	}
	
	this.TitlebarUsernameText = undefined;
	this.TitlebarUsernameTextColor = undefined;
	this.TitlebarTitleText = undefined;
	this.TitlebarTitleTextColor = undefined;
	this.TitlebarMessageText = undefined;
	this.TitlebarMessageTextColor = undefined;
	this.TitlebarGroupText = undefined;
	this.TitlebarGroupTextColor = undefined;
	this.TitlebarBackgroundImage = undefined;
	this.TitlebarBackgroundOpacity = undefined;
	this.TitlebarBackgroundColor = undefined;
	this.TitlebarBackgroundGradientLeft = undefined;
	this.TitlebarBackgroundGradientRight = undefined;
	this.TitlebarBorderStyle = undefined;
	this.TitlebarBorderColor = undefined;

	this.AvatarImage = undefined;
	this.AvatarOpacity = undefined;
	this.AvatarBorderStyle = undefined;
	this.AvatarBorderColor = undefined;

	this.PostBackgroundOpacity = undefined;
	this.PostBackgroundImage = undefined;
	this.PostBackgroundImageRepeat = undefined;
	this.PostBackgroundImageAttachment = undefined;
	this.PostBackgroundImagePosition = undefined;
	this.PostBackgroundColor = undefined;
	this.PostBackgroundGradientLeft = undefined;
	this.PostBackgroundGradientRight = undefined;
	this.PostFont = undefined;
	this.PostFontColor = undefined;
	this.PostLinkColor = undefined;
	
}
Styles.DEFAULT_STYLE = "*";


//Client: Good (28th June, 2011)
var Client = {

	KEY_NAME:"coup5key",
	IGNORE_LIST_NAME:"coup5ignore",

	GetUsername:function(defaultVal){
		var match = /BungieDisplayName=(.*?)(?:;|$)/i.exec(document.cookie);
		return match != null && match[1] != undefined ? unescape(match[1]).replace(/&nbsp;/gi, " ") : defaultVal;
	},
	IsSignedIn:function(){
		if(document.getElementById("ctl00_dashboardNav_passportSignOutLink")){
			return true;
		}
		return false;
	},
	GetKey:function(defaultVal){
		return Browser.Memory.Get(this.KEY_NAME, defaultVal);
	},
	SetKey:function(keyValue){
		Browser.Memory.Set(this.KEY_NAME, keyValue);
	},
	GetIgnoreList:function(defaultVal){
		var jsonStr = Browser.Memory.Get(this.IGNORE_LIST_NAME, null);
		var jsonObj = [];
		if(jsonStr != null){
			try{
				jsonObj = JSON.parse(jsonStr);
			}
			catch(e){}
		}
		return jsonObj;
	},
	SetIgnoreList:function(list){
		Browser.Memory.Set(this.IGNORE_LIST_NAME, JSON.stringify(Array.Distinct(list)));
	}

}


//MainFunctions: Good, could use a tidyup/optimising though (28th June, 2011)
var MainFunctions = {
	
	ApplyStylesToElement:function(styles, element){
	
		//element should be a div.forum_item, but will probably work with div.forum_alt_item as well
		
		var rgb;
		var rgb2;
		var usernameElem = $(element).find("ul.author_header_block li.login > a");
		temp = JSON.parse(Browser.Memory.Get('coup5options', "{}"));
		var ocheckbox = temp['checkbox'] != undefined ? temp['checkbox'] : {};
		temp = JSON.parse(Browser.Memory.Get('coup5options', "{}"));
		var otext = temp['text'] != undefined ? temp['text'] : {};
		temp = JSON.parse(Browser.Memory.Get('coup5ignorelist', "{}"));
		var icheckbox = temp[usernameElem.text()] != undefined ? temp[usernameElem.text()] : {};
		delete(temp);
		var checked = 'checked="checked"';
		
		if(Options.Get('coup5ignorelist', usernameElem.text(), 'PostBackgroundOpacity', undefined) != undefined) {
			styles['PostBackgroundOpacity'] = Options.Get('coup5ignorelist', usernameElem.text(), 'PostBackgroundOpacity', 1.0);
		}
		styles['PostBackgroundOpacity'] = parseFloat(styles.PostBackgroundOpacity) < Options.Get('coup5options', 'text', 'PostBackgroundOpacity', 1.0) ? styles.PostBackgroundOpacity : Options.Get('coup5options', 'text', 'PostBackgroundOpacity', 1.0);
		if(Options.Get('coup5ignorelist', usernameElem.text(), 'TitlebarBackgroundOpacity', undefined) != undefined) {
			styles['TitlebarBackgroundOpacity'] = Options.Get('coup5ignorelist', usernameElem.text(), 'TitlebarBackgroundOpacity', 1.0);
		}
		styles['TitlebarBackgroundOpacity'] = parseFloat(styles.TitlebarBackgroundOpacity) < Options.Get('coup5options', 'text', 'TitlebarBackgroundOpacity', 1.0) ? styles.TitlebarBackgroundOpacity : Options.Get('coup5options', 'text', 'TitlebarBackgroundOpacity', 1.0);
		if(Options.Get('coup5ignorelist', usernameElem.text(), 'AvatarOpacity', undefined) != undefined) {
			styles['AvatarOpacity'] = Options.Get('coup5ignorelist', usernameElem.text(), 'AvatarOpacity', 1.0);
		}
		styles['AvatarOpacity'] = parseFloat(styles.AvatarOpacity) < Options.Get('coup5options', 'text', 'AvatarOpacity', 1.0) ? styles.AvatarOpacity : Options.Get('coup5options', 'text', 'AvatarOpacity', 1.0);
		
		function ignoreValueExists(o){
			if(icheckbox[o] != undefined){
				return icheckbox[o];
			} else {
				return true;
			}
		}
		try {
			defaultTitlebarColor = /background-color\:\s*((\S|\s)*?)\;/i.exec($(element).find("ul.author_header_block").attr('style'))[1];
		} catch(e) {
			defaultTitlebarColor = '#27282C';
		}
		$(element).find("ul.author_header_block").css({backgroundColor:"transparent", position:"relative"}); 
		$(element).find("ul.author_header_block").parent().createPrepend("div", {style:{position:"absolute", top:0, left:0, width:"100%", height:"24px"}});
		var titlebar = $(element).find("ul.author_header_block").prev();
		
		//Set titlebar
		if(styles.TitlebarBackgroundColor != Styles.DEFAULT_STYLE && ocheckbox['TitlebarBackgroundColor'] != checked && ignoreValueExists('TitlebarBackgroundColor')){
			rgb = styles.TitlebarBackgroundColor.ToHex().ToRGB();
			$(titlebar).css("background-color", "rgba(" + rgb.R + "," + rgb.G + "," + rgb.B + "," + styles.TitlebarBackgroundOpacity + ")");
		} else {
			$(titlebar).css('background-color', defaultTitlebarColor);
		}
		if(styles.TitlebarBackgroundGradientLeft != Styles.DEFAULT_STYLE && styles.TitlebarBackgroundGradientRight != Styles.DEFAULT_STYLE && ocheckbox['TitlebarBackgroundGradientLeft'] != checked && ocheckbox['TitlebarBackgroundGradientRight'] != checked && ignoreValueExists('TitlebarBackgroundGradientLeft') && ignoreValueExists('TitlebarBackgroundGradientRight')){
			rgb = styles.TitlebarBackgroundGradientLeft.ToHex().ToRGB();
			rgb2 = styles.TitlebarBackgroundGradientRight.ToHex().ToRGB();
			$(titlebar).css("background-image", "-moz-linear-gradient(left, rgba(" + rgb.R + "," + rgb.G + "," + rgb.B + "," + styles.TitlebarBackgroundOpacity + "), rgba(" + rgb2.R + "," + rgb2.G + "," + rgb2.B + "," + styles.TitlebarBackgroundOpacity + "))");
		}
		if(styles.TitlebarBackgroundImage != Styles.DEFAULT_STYLE && ocheckbox['TitlebarBackgroundImage'] != checked && ignoreValueExists('TitlebarBackgroundImage')){
			$(titlebar).css("background-image", "url(\"" + styles.TitlebarBackgroundImage.HTMLEncode() + "\")");
			$(titlebar).css("opacity", styles.TitlebarBackgroundOpacity);
		}
		if(styles.TitlebarBorderColor != Styles.DEFAULT_STYLE && styles.TitlebarBorderStyle != Styles.DEFAULT_STYLE && ocheckbox['TitlebarBorder'] != checked && ignoreValueExists('TitlebarBorder')){
			$(titlebar).css("border", "1px solid #" + styles.TitlebarBorderColor.HTMLEncode());
			$(titlebar).css("border-style", styles.TitlebarBorderStyle.HTMLEncode());
		}
		
		var groupElem = $(element).find("ul.author_header_block li.author_header_links > a:contains('groups')");
		if(styles.TitlebarGroupText != Styles.DEFAULT_STYLE && ocheckbox['TitlebarGroupText'] != checked && ignoreValueExists('TitlebarGroupText')){
			groupElem.text(styles.TitlebarGroupText);
		}
		if(styles.TitlebarGroupTextColor != Styles.DEFAULT_STYLE && ocheckbox['TitlebarGroupTextColor'] != checked && ignoreValueExists('TitlebarGroupTextColor')){
			groupElem.css("color", "#" + styles.TitlebarGroupTextColor.HTMLEncode());
		}			
		var msgElem = $(element).find("ul.author_header_block li.author_header_links > a:contains('message user')");
		if(styles.TitlebarMessageText != Styles.DEFAULT_STYLE && ocheckbox['TitlebarMessageText'] != checked && ignoreValueExists('TitlebarMessageText')){
			msgElem.text(styles.TitlebarMessageText);
		}
		if(styles.TitlebarMessageTextColor != Styles.DEFAULT_STYLE && ocheckbox['TitlebarMessageTextColor'] != checked && ignoreValueExists('TitlebarMessageTextColor')){
			msgElem.css("color", "#" + styles.TitlebarMessageTextColor.HTMLEncode());
		}
		var titleElem = $(element).find("ul.author_header_block li.title");
		if(styles.TitlebarTitleText != Styles.DEFAULT_STYLE && ocheckbox['TitlebarTitleText'] != checked && ignoreValueExists('TitlebarTitleText')){
			titleElem.text(styles.TitlebarTitleText);
		}
		if(styles.TitlebarTitleTextColor != Styles.DEFAULT_STYLE && ocheckbox['TitlebarTitleTextColor'] != checked && ignoreValueExists('TitlebarTitleTextColor')){
			titleElem.css("color", "#" + styles.TitlebarTitleTextColor.HTMLEncode());
		}
		$(element).find("div.forumavatar img, ul.author_header_block li.login > a").attr('title', usernameElem.text());
		if(styles.TitlebarUsernameText != Styles.DEFAULT_STYLE && ocheckbox['TitlebarUsernameText'] != checked && ignoreValueExists('TitlebarUsernameText')){
			usernameElem.text(styles.TitlebarUsernameText);
		}
		if(styles.TitlebarUsernameTextColor != Styles.DEFAULT_STYLE && ocheckbox['TitlebarUsernameTextColor'] != checked && ignoreValueExists('TitlebarUsernameTextColor')){
			usernameElem.css("color", "#" + styles.TitlebarUsernameTextColor.HTMLEncode());
		}
		
		//Set avatar
		$(element).find("div.forumavatar img").css("opacity", styles.AvatarOpacity);
		if(styles.AvatarImage != Styles.DEFAULT_STYLE && ocheckbox['AvatarImage'] != checked && ignoreValueExists('AvatarImage')){
			$(element).find("div.forumavatar img").attr("src", styles.AvatarImage.HTMLEncode());
		}
		if(styles.AvatarBorderStyle != Styles.DEFAULT_STYLE && styles.AvatarBorderColor != Styles.DEFAULT_STYLE && ocheckbox['AvatarBorderStyle'] != checked && ocheckbox['AvatarBorderColor'] != checked && ignoreValueExists('AvatarBorderStyle') && ignoreValueExists('AvatarBorderColor')){
			$(element).find("div.forumavatar img").css({width:"88px", height:"88px"}); //MUST be set if border style is set (screws up layout if not)
			$(element).find("div.forumavatar img").css("border", "1px " + styles.AvatarBorderStyle.HTMLEncode() + " #" + styles.AvatarBorderColor.HTMLEncode()); 
		}
		
		//Set post
		$(element).css({backgroundColor:"transparent", position:"relative"});
		$(element).parent().createPrepend("div", {style:{opacity:styles.PostBackgroundOpacity, position:"absolute", top:0, left:0, width:"670px", height:"100%"}});
		var postBg = $(element).parent().find("div:first");
		try {
			if(preview_) {
				temp = $("<span></span>");
				postBg = temp.createPrepend("div", {style:{opacity:styles.PostBackgroundOpacity, position:"absolute", top:0, left:0, width:"670px", height:"100%"}});
			}
		} catch(e) {}
		
		if(styles.PostBackgroundColor != Styles.DEFAULT_STYLE && ocheckbox['PostBackgroundColor'] != checked && ignoreValueExists('PostBackgroundColor')){
			rgb = styles.PostBackgroundColor.ToHex().ToRGB();
			$(postBg).css("background-color", "rgba(" + rgb.R + "," + rgb.G + "," + rgb.B + "," + styles.PostBackgroundOpacity + ")"); 
		}
		
		if(styles.PostBackgroundGradientLeft != Styles.DEFAULT_STYLE && styles.PostBackgroundGradientRight != Styles.DEFAULT_STYLE && ocheckbox['PostBackgroundGradientLeft'] != checked && ocheckbox['PostBackgroundGradientRight'] != checked && ignoreValueExists('PostBackgroundGradientLeft') && ignoreValueExists('PostBackgroundGradientRight')){
			rgb = styles.PostBackgroundGradientLeft.ToHex().ToRGB();
			rgb2 = styles.PostBackgroundGradientRight.ToHex().ToRGB();
			$(postBg).css("background-image", "-moz-linear-gradient(left, rgba(" + rgb.R + "," + rgb.G + "," + rgb.B + "," + styles.PostBackgroundOpacity + "), rgba(" + rgb2.R + "," + rgb2.G + "," + rgb2.B + "," + styles.PostBackgroundOpacity + "))");
		}
		if(styles.PostBackgroundImage != Styles.DEFAULT_STYLE && ocheckbox['PostBackgroundImage'] != checked && ignoreValueExists('PostBackgroundImage')){
			$(postBg).css("background-repeat", styles.PostBackgroundImageRepeat != Styles.DEFAULT_STYLE ? styles.PostBackgroundImageRepeat : "repeat");
			$(postBg).css("background-position", styles.PostBackgroundImagePosition != Styles.DEFAULT_STYLE ? styles.PostBackgroundImagePosition : "0% 0%");
			$(postBg).css("background-attachment", styles.PostBackgroundImageAttachment != Styles.DEFAULT_STYLE ? styles.PostBackgroundImageAttachment : "scroll");
			$(postBg).css("background-image", "url(\"" + styles.PostBackgroundImage.HTMLEncode() + "\")");
		}		
		
		if(styles.PostFont != Styles.DEFAULT_STYLE && ocheckbox['PostFont'] != checked && ignoreValueExists('PostFont')){
			$(element).find("div.postbody > p").css("font-family", styles.PostFont.HTMLEncode());
		}
		if(styles.PostFontColor != Styles.DEFAULT_STYLE && ocheckbox['PostFontColor'] != checked && ignoreValueExists('PostFontColor')){
			$(element).find("div.postbody > p").css("color", "#" + styles.PostFontColor.HTMLEncode());
		}
		if(styles.PostLinkColor != Styles.DEFAULT_STYLE && ocheckbox['PostLinkColor'] != checked && ignoreValueExists('PostLinkColor')){
			$(element).find("div.postbody > p a").css("color", "#" + styles.PostLinkColor.HTMLEncode());
		}
		try {
			if(preview_) {
				temp.append(element);
				return temp;
			}
		} catch(e) {
			return element;
		}
		
	},
	
	GenerateStylePreview:function(styles){
	
		var preview = "#PublishSettingsPreviewBox"
		$(preview).createAppend(
			"div", {className:"forum_item"}, [
				"div", {className:"forum_item_outer_shell"}, [
					"div", null, [
						"span", null, [
							"a", null, null,
							"div", {className:"forumpost"}, [
								"div", {className:"clear"}, null,
								"div", {className:"forumavatar"}, [
									"a", null, [
										"img", {style:{height:"90px", width:"90px", borderWidth:"0px"}, src:"/Forums/skins/default/avatars/default_avatar.gif"}, null
									]
								],
								"div", {className:"postbody"}, [
									"ul", {style:{backgroundColor:"#4C4C4C"}, className:"author_header_block"}, [
										"li", {className:"login"}, [
											"a", {href:"/Account/Profile.aspx"}, styles.TitlebarUsernameText != Styles.DEFAULT_STYLE ? styles.TitlebarUsernameText : "Username",
										],
										"li", null, "&nbsp;|&nbsp;",
										"li", {className:"title"}, "Member",
										"li", {className:"author_header_links"}, [
											"a", {className:"expanded_arrows_collapsed"}, [
												"img", {style:{width:"21px", height:"20px"}, src:"/images/spacer.gif"}
											]
										],
										"li", {className:"author_header_links"}, "&nbsp;|&nbsp;more&nbsp;",
										"li", {className:"author_header_links"}, [
											"a", {href:"/account/profile.aspx?page=Chapters"}, "groups"
										],
										"li", {className:"author_header_links"}, "&nbsp;|&nbsp;",
										"li", {className:"author_header_links"}, [
											"a", {href:"/account/profile.aspx?page=Messages"}, "message user"
										]
									],
									"div", {className:"floatingprofile"}, [
										"ul", {className:"leftside"}, [
											"li", null, [
												"span", null, "gamertag:",
												"a", {href:"http://xbox.com/"}, "Xbox LIVE"
											],
											"li", null, [
												"span", null, "user homepage:",
												"a", {target:"_blank", href:"/", title:"/"}, [
													"img", {width:"11px", height:"13px", alt:"external", className:"external_link_arrow", src:"/images/spacer.gif"}
												]
											]
										],
										"ul", {className:"rightside"}, [
											"li", null, [
												"a", {href:"/"}, "more posts by this user"
											],
											"li", null, "last post: 77.77.7777 7:77 PM PDT"
										],
										"div", {className:"signature"}, [
											"p", null, [
												"span", null, "Signature content"
											]
										]
									],
									"p", {style:'margin:0px 0px 1em;'}, "Lorem ipsum dolor sit amet, <a href=\"javascript:;\">consectetur</a> adipiscing <b>elit</b>. Curabitur condimentum, est et <i>aliquet</i> adipiscing, sem felis fermentum turpis, sed <u>tincidunt</u> erat nulla vel nisl. <a hreg=\"javascrpt:;\">Aliquam</a> tristique interdum tempor. Sed commodo ipsum a odio laoreet eleifend eleifend nisl auctor. Donec molestie scelerisque orci suscipit volutpat."
								],
								"div", {className:"post-actions"}, [
									"ul", null, [
										"li", {className:"date"}, "07.07.7777 7:07 PM PDT",
										"li", {style:{cssFloat:"right"}}, [
											"a", {href:"javascript:;", className:"forum_post_reply_button"}, [
												"img", {style:{height:"17px", width:"52px"}, src:"/images/spacer.gif", alt:"reply"}
											]
										]
									]
								]
							]
						]
					]
				]
			]
		);
		
		return MainFunctions.ApplyStylesToElement(styles, $(preview).find(".forumpost:first"), preview_=true);
	},
	
	ClientProfilePage:function(){
		
		$("#ctl00_mainContent_profilePanel").createAppend(
			"div", {className:"boxD_outer", id:"CoupDBungie5", style:{width:"100%", marginTop:"-10px"}}, [
				"div", {className:"boxD_inner"}, [
					"div", {className:"boxD", style:{width:"100%"}}, [
						
						"h3", null, [
							"span", null, "Coup d&#39;Bungie " + CoupDBungie.Version + " for " + CoupDBungie.Platform + " (by ",
							"a", {href:"?memberID=" + CoupDBungie.AuthorMemberID}, CoupDBungie.Author.HTMLEncode(),
							"span", null, ")"
						],
						
						//Profile section
						"div", {style:{margin:"5px"}}, [
							"fieldset", {style:{border:"1px solid", padding:"5px"}}, [
								"legend", {style:{marginLeft:"5px"}}, [
									"a", {href:"javascript:;", onclick:function(){
										$(this).parent().next().toggle();
										window.scrollTo(0, 700);
									}}, "Profile"
								],
								"table", {style:{width:"100%", display:"none"}}, [
								
									"tr", null, [
										"td", {colspan:"2"}, [
											"p", {style:{margin:"10px"}}, [
												"span", null, "Welcome to Coup d&#39;Bungie 5.",
												"br", null, null,
												"br", null, null,
												"span", null, "To use Coup d&#39;Bungie 5, you must first register. You only need to do this once and once only. There is more information about the registeration process in the FAQ on Registeration link below. Please note: ",
												"b", {style:{textDecoration:"underline"}}, "You will not be able to use many of the features of Coup d&#39;Bungie 5 if you have not registered"
											]
										]
									],
								
									//Key row
									"tr", null, [
										"td", null, "Key: ",
										"td", null, [
											"input", {type:"password", readOnly:"readonly", size:64, value:Client.GetKey("")}, null,
											"a", 
												{
													style:{marginLeft:"8px", cursor:"pointer", MozUserSelect:"none"},
													onclick:function(){
														var type = $(this).siblings("input").get(0).type;
														$(this).prev("input").get(0).type = type == "password" ? "input" : "password";
													}
												},
											"Show/Toggle",
											"a",
												{
													style:{marginLeft:"8px", cursor:"pointer", MozUserSelect:"none"},
													onclick:function(){
														var newkey = prompt("This option is for manually entering and saving a key. Do not put your Validation String in here.");
														if(newkey != null){
															if(/^[0-9a-f]{64}$/.test(newkey)){
																Client.SetKey(newkey);
																$(this).siblings("input").val(newkey);
																alert("Key saved successfully");
															}		
															else{
																alert("Key is not valid. It should only contain numbers and letters, and be 64 characters long.");
															}
														}
													}
												},
											"Manual Override"
										]
									],
									//End Key row
									
									//Register row
									"tr", null, [
										"td", null, "",
										"td", null, [
											"input", {type:"button", value:"Register",
												onclick:function(){
												
													if(Client.GetKey(null) != null){
														if(!confirm("It appears you already have a key stored. Do you want to begin the registration process anyway?")){
															return;
														}
													}
													
													alert("The Coup d'Bungie 5 registration process will now begin.");
													alert(
														"Please note:\n\n" +
														"- Make sure you've read the article \"FAQ on Registration\" if you're not sure about anything\n" +
														"- There are a few questions along the way\n" +
														"- You'll only ever have to do this process once\n" +
														"- It will take a few minutes at the most to complete\n" +
														"- Do not start a new browsing session during the process\n" +
														"- If you have any problems, don't hesitate to ask!"
													);
													
													if(!confirm("If your username is " + Client.GetUsername("") + ", click OK to continue")){
														return;
													}
													
													if(confirm("Click OK if you have already received a Validation String from this process, otherwise click cancel (you should probably click cancel if you haven't done this before)")){
														
														var memberID;
														do{
															memberID = prompt("Enter your memberID (not your userID)")
														}
														while(!/^[0-9]+$/g.test(memberID));
														
														CoupDBungie.Server.Register(null, memberID,
															function(){
																var obj = this.GetResponseJSON();
																if(obj != null){
																	if(obj.Status === CoupDBungie.Server.Responses.OK){
																		prompt("Success!\n\nYour key is displayed below. It will be saved in your browser automatically", obj.Key);
																		Client.SetKey(obj.Key);
																	}
																	else{
																		alert("The server said: " + obj.Reason);
																	}
																}
																else{
																	alert("JSON parsing failed.\n\nHTTP Status: " + this.GetStatus() + "\n\nBody: " + this.GetResponseText());
																}
															},
															function(){
																alert("HTTP Error.\n\nHTTP Status: " + this.GetStatus());
															},
															null
														);
														
													}
													else{
											
														CoupDBungie.Server.Register(Client.GetUsername(""), null,
															function(){
																var obj = this.GetResponseJSON();
																if(obj != null){
																	if(obj.Status === CoupDBungie.Server.Responses.OK){
																		prompt("Success!\n\nHere is your Validation String", obj.ValidationString);
																	}
																	else{
																		alert("The server said: " + obj.Reason);
																	}
																}
																else{
																	alert("HTTP Error.\n\nHTTP Status: " + this.GetStatus());
																}
															},
															function(){
															
															},
															null
														);
														
													}
												}
											},
											null,
											"a", {href:"/fanclub/coup5/Group/Resources/FAQ.aspx?cid=578645", target:"_blank", style:{marginLeft:"8px"}}, "FAQ on Registration"
										]
									]
									//End Register row
									
								]
							]
						],
						//End Profile section
						
						//Start Publish Styles section
						"div", {style:{margin:"5px"}}, [
							"fieldset", {style:{border:"1px solid", padding:"5px"}}, [
								"legend", {style:{marginLeft:"5px"}}, [
									"a", {href:"javascript:;", onclick:function(){
										$(this).parent().next().toggle();
										window.scrollTo(0,700);
									}}, "Publish Styles"
								],
								"table", {id:"PublishSettingsTable", style:{width:"100%", display:"none"}}, [
								
									"tr", null, [
										"td", {colspan:"2"}, [
											"p", {style:{margin:"10px"}}, [
												"span", null, "Here is where you can publish a new set of styles. If you haven't already, you should read up on ",
												"a", {href:"/fanclub/404459/Forums/posts.aspx?postID=60124459", target:"_blank"}, "what is and what is not permitted",
												"span", null, " to be published, as well as the restrictions. If you feel that they are too restrictive, or just want to suggest an idea or feature, please make a topic about it in the ",
												"a", {href:"/fanclub/coup5/Group/GroupHome.aspx", target:"_blank"}, "Coup d'Bungie 5 forum"
											]
										]
									],
								
									"tr", {style:{fontWeight:"bold", fontStyle:"italic", textDecoration:"underline"}}, [
										"td", {style:{width:"220px"}}, "Style Name",
										"td", null, "Style Value"
									],
								
									"tr", null, [
										"td", null, "TitlebarUsernameText: ",
										"td", null, [
											"input", {id:"TitlebarUsernameText"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarUsernameTextColor: ",
										"td", null, [
											"input", {id:"TitlebarUsernameTextColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarTitleText: ",
										"td", null, [
											"input", {id:"TitlebarTitleText"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarTitleTextColor: ",
										"td", null, [
											"input", {id:"TitlebarTitleTextColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarMessageText: ",
										"td", null, [
											"input", {id:"TitlebarMessageText"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarMessageTextColor: ",
										"td", null, [
											"input", {id:"TitlebarMessageTextColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarGroupText: ",
										"td", null, [
											"input", {id:"TitlebarGroupText"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarGroupTextColor: ",
										"td", null, [
											"input", {id:"TitlebarGroupTextColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarBackgroundImage: ",
										"td", null, [
											"input", {id:"TitlebarBackgroundImage"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarBackgroundOpacity: ",
										"td", null, [
											"input", {id:"TitlebarBackgroundOpacity"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarBackgroundColor: ",
										"td", null, [
											"input", {id:"TitlebarBackgroundColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarBackgroundGradientLeft: ",
										"td", null, [
											"input", {id:"TitlebarBackgroundGradientLeft", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarBackgroundGradientRight: ",
										"td", null, [
											"input", {id:"TitlebarBackgroundGradientRight", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarBorderStyle: ",
										"td", null, [
											"input", {id:"TitlebarBorderStyle"}, null
										]
									],
									
									"tr", null, [
										"td", null, "TitlebarBorderColor: ",
										"td", null, [
											"input", {id:"TitlebarBorderColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", {colspan:"2"}, [
											"hr", null
										]
									],
									
									"tr", null, [
										"td", null, "AvatarImage: ",
										"td", null, [
											"input", {id:"AvatarImage"}, null
										]
									],
									
									"tr", null, [
										"td", null, "AvatarOpacity: ",
										"td", null, [
											"input", {id:"AvatarOpacity"}, null
										]
									],
									
									"tr", null, [
										"td", null, "AvatarBorderStyle: ",
										"td", null, [
											"input", {id:"AvatarBorderStyle"}, null
										]
									],
									
									"tr", null, [
										"td", null, "AvatarBorderColor: ",
										"td", null, [
											"input", {id:"AvatarBorderColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", {colspan:"2"}, [
											"hr", null
										]
									],
									
									"tr", null, [
										"td", null, "PostBackgroundOpacity: ",
										"td", null, [
											"input", {id:"PostBackgroundOpacity"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostBackgroundImage: ",
										"td", null, [
											"input", {id:"PostBackgroundImage"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostBackgroundImageRepeat: ",
										"td", null, [
											"input", {id:"PostBackgroundImageRepeat"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostBackgroundImageAttachment: ",
										"td", null, [
											"input", {id:"PostBackgroundImageAttachment"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostBackgroundImagePosition: ",
										"td", null, [
											"input", {id:"PostBackgroundImagePosition"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostBackgroundColor: ",
										"td", null, [
											"input", {id:"PostBackgroundColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostBackgroundGradientLeft: ",
										"td", null, [
											"input", {id:"PostBackgroundGradientLeft", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostBackgroundGradientRight: ",
										"td", null, [
											"input", {id:"PostBackgroundGradientRight", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostFont: ",
										"td", null, [
											"input", {id:"PostFont"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostFontColor: ",
										"td", null, [
											"input", {id:"PostFontColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", null, "PostLinkColor: ",
										"td", null, [
											"input", {id:"PostLinkColor", className:"Coup5ColorWheel"}, null
										]
									],
									
									"tr", null, [
										"td", {colspan:"2"}, [
											
											"input", {type:"button", style:{margin:"8px", marginLeft:"0px"}, value:"Publish Styles",
												onclick:function(){
											
													if(Client.GetKey(null) == null){
														alert("You will need to register and have a key saved before you can publish any styles.");
														return;
													}
													if(!confirm("Are you sure you want to publish these styles?")){
														return;
													}
													
													var s = new Styles();
													
													s.TitlebarUsernameText = $("#TitlebarUsernameText").val();
													s.TitlebarUsernameTextColor = $("#TitlebarUsernameTextColor").val();
													s.TitlebarTitleText = $("#TitlebarTitleText").val();
													s.TitlebarTitleTextColor = $("#TitlebarTitleTextColor").val();
													s.TitlebarMessageText = $("#TitlebarMessageText").val();
													s.TitlebarMessageTextColor = $("#TitlebarMessageTextColor").val();
													s.TitlebarGroupText = $("#TitlebarGroupText").val();
													s.TitlebarGroupTextColor = $("#TitlebarGroupTextColor").val();
													s.TitlebarBackgroundImage = $("#TitlebarBackgroundImage").val();
													s.TitlebarBackgroundOpacity = $("#TitlebarBackgroundOpacity").val();
													s.TitlebarBackgroundColor = $("#TitlebarBackgroundColor").val();
													s.TitlebarBackgroundGradientLeft = $("#TitlebarBackgroundGradientLeft").val();
													s.TitlebarBackgroundGradientRight = $("#TitlebarBackgroundGradientRight").val();
													s.TitlebarBorderStyle = $("#TitlebarBorderStyle").val();
													s.TitlebarBorderColor = $("#TitlebarBorderColor").val();
													
													s.AvatarImage = $("#AvatarImage").val();
													s.AvatarOpacity = $("#AvatarOpacity").val();
													s.AvatarBorderStyle = $("#AvatarBorderStyle").val();
													s.AvatarBorderColor = $("#AvatarBorderColor").val();
													
													s.PostBackgroundOpacity = $("#PostBackgroundOpacity").val();
													s.PostBackgroundImage = $("#PostBackgroundImage").val();
													s.PostBackgroundImageRepeat = $("#PostBackgroundImageRepeat").val();
													s.PostBackgroundImageAttachment = $("#PostBackgroundImageAttachment").val();
													s.PostBackgroundImagePosition = $("#PostBackgroundImagePosition").val();
													s.PostBackgroundColor = $("#PostBackgroundColor").val();
													s.PostBackgroundGradientLeft = $("#PostBackgroundGradientLeft").val();
													s.PostBackgroundGradientRight = $("#PostBackgroundGradientRight").val();
													s.PostFont = $("#PostFont").val();
													s.PostFontColor = $("#PostFontColor").val();
													s.PostLinkColor = $("#PostLinkColor").val();
													
													CoupDBungie.Server.PublishStyles(Client.GetUsername(""), Client.GetKey(""), s,
														function(){
															var obj = this.GetResponseJSON();
															if(obj != null){
																if(obj.Status === CoupDBungie.Server.Responses.OK){
																	alert("Your styles were published successfully and will show up at the next caching interval (approx. every " + Cache.CACHE_INTERVAL_RATE + " mins on the hour).");
																}
																else{
																	alert("The server said: " + obj.Reason);
																}
															}
															else{
																alert("JSON parsing failed.\n\nHTTP Status: " + this.GetStatus() + "\n\nBody: " + this.GetResponseText());
															}
														},
														function(){
															alert("HTTP Error.\n\nHTTP Status: " + this.GetStatus());
														},
														null
													);
											
												}
											}, null
											
										]
									]
									
								]
							]
						],
						//End Publish Styles section
						
						//Options and ignore list section
						"div", {style:{margin:"5px"}}, [
							"fieldset", {style:{border:"1px solid", padding:"5px"}}, [
								"legend", {style:{marginLeft:"5px"}}, [
									"a", {href:"javascript:;", onclick:function(){
										$(this).parent().next().toggle();
										window.scrollTo(0,700);
									}}, "Options"
								],
								"div", {style:{margin:"10px", display:"none"}, id:"coup5options"}, [
									"div", {style:{display:"table-cell", maxWidth:"50%", width:"414px"}}, [
										"fieldset", {style:{border:"1px solid", padding:"5px"}}, [
											"legend", {style:{margin:"10px"}}, "User ignore",
											"div", {style:{margin:"10px"}}, [
												"select", {id:"IgnoreList", style:{width:"200px"}, size:5}, null
											],
											"div", {style:{margin:"10px"}}, [
												"input", {type:"button", value:"Remove All", onclick:function(){
													if(confirm("Are you sure you want to remove everyone from your ignore list?")){
														$("#IgnoreList").empty();
														Client.SetIgnoreList([]);
														Browser.Memory.Delete('coup5ignorelist');
													}
												}}, null,
												"input", {type:"button", value:"Remove", onclick:function(){
													if($("#IgnoreList option:selected").is('[style]')) {
														$("#IgnoreList option:selected").attr('style', 'color:gray;');
													} else {
														$("#IgnoreList option:selected").remove();
													}
													var usernames = [];
													$("#IgnoreList option").each(function(){
														usernames.Add($(this).val());
													});
													Client.SetIgnoreList(usernames);
												}}, null,
												"input", {type:"button", value:"Edit", onclick:function(){
													var username = $("#IgnoreList option:selected").val();
													IgnoreSpawn(username);
												}}, null,
												"input", {type:"button", value:"Add", onclick:function(){
													var username = prompt("Enter the username of the user you want to ignore", "");
													if(username){
														$("#IgnoreList").append(new Option(username.HTMLEncode(), username.HTMLEncode()));
														var usernames = [];
														$("#IgnoreList option").each(function(){
															usernames.Add($(this).val());
														});
														Client.SetIgnoreList(usernames);
													}
												}}, null
											]
										],
										"fieldset", {style:{border:"1px solid", padding:"5px"}}, [
											"legend", {style:{margin:"10px"}}, "Global maximum opacity",
											"div", null, [
												"div", {style:{height:"25px"}}, [
													"span", null, "Titlebar opacity:",
													"input", {type:"range", name:"TitlebarBackgroundOpacity", placeholder:"Value between 0.0 - 1.0", style:{cssFloat:"right", borderStyle:"solid", borderRadius:"2px 2px", width:"150px"}}, null
												],
												"div", {style:{height:"25px"}}, [
													"span", null, "Avatar opacity:",
													"input", {type:"range", name:"AvatarOpacity", placeholder:"Value between 0.0 - 1.0", style:{cssFloat:"right", borderStyle:"solid", borderRadius:"2px 2px", width:"150px"}}, null
												],
												"div", {style:{height:"25px"}}, [
													"span", null, "Post opacity:",
													"input", {type:"range", name:"PostBackgroundOpacity", placeholder:"Value between 0.0 - 1.0", style:{cssFloat:"right", borderStyle:"solid", borderRadius:"2px 2px", width:"150px"}}, null
												]
											]
										],
										"fieldset", {style:{border:"1px solid", padding:"5px"}}, [
											"legend", {style:{margin:"10px"}}, "Misc.",
											"div", {'class':'coup5miscoptions'}, [
												"div", {style:{height:"25px"}}, [
													"span", null,  "GM Storage:",
													"input", {type:"checkbox", name:"coup5storagetype", style:{cssFloat:"right", borderStyle:"solid", borderRadius:"2px 2px"}}, null
												]
											]
										]
									],
									"div", {style:{display:"table-cell", maxWidth:"50%", width:"414px"}}, [
										"fieldset", {style:{border:"1px solid", padding:"5px"}}, [
											"legend", {style:{margin:"10px"}, className:'coup5optionsglobdec'}, "Globally disabled declarations",
											"table", null, [
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarUsernameText"}
													],
													"td", null, "Titlebar username text"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarUsernameTextColor"}
													],
													"td", null, "Titlebar username text color"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarTitleText"}
													],
													"td", null, "Titlebar title text"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarTitleTextColor"}
													],
													"td", null, "Titlebar title text color"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarMessageText"}
													],
													"td", null, "Titlebar message text"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarMessageTextColor"}
													],
													"td", null, "Titlebar message text color"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarGroupText"}
													],
													"td", null, "Titlebar group text"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarGroupTextColor"}
													],
													"td", null, "Titlebar group text color"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarBackgroundImage"}
													],
													"td", null, "Titlebar background image"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarBackgroundColor"}
													],
													"td", null, "Titlebar background color"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarBackgroundGradient"}
													],
													"td", null, "Titlebar background gradient"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"TitlebarBorder"}
													],
													"td", null, "Titlebar border"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"AvatarImage"}
													],
													"td", null, "Avatar image"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"AvatarBorder"}
													],
													"td", null, "Avatar border"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"PostBackgroundImage"}
													],
													"td", null, "Post background image"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"PostBackgroundImageRepeat"}
													],
													"td", null, "Post background repeat"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"PostBackgroundImageAttachment"}
													],
													"td", null, "Post background image attachment"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"PostBackgroundImagePosition"}
													],
													"td", null, "Post background position"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"PostBackgroundColor"}
													],
													"td", null, "Post background color"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"PostBackgroundGradient"}
													],
													"td", null, "Post background gradient"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"PostFont"}
													],
													"td", null, "Post font"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"PostFontColor"}
													],
													"td", null, "Post font color"
												],
												"tr", null, [
													"td", null, [
														"input", {type:"checkbox", name:"PostLinkColor"}
													],
													"td", null, "Post link color"
												]
											]
										]
									],
									"div", null, [
									"input", {type:"button", value:"Saved automatically", disabled:"disabled"}, null,
									"input", {type:"button", value:"Reset options", onclick:function(){
										if(confirm("Are you sure you want to reset all options?")){
											Options.Empty('coup5options');
										}
									}}
									]
								]
							]
						],
						//End Ignore list section
						
						//Cache
						"div", {style:{margin:"5px"}}, [
							"fieldset", {style:{border:"1px solid", padding:"5px"}}, [
								"legend", {style:{marginLeft:"5px"}}, [
									"a", {href:"javascript:;", onclick:function(){
										$(this).parent().next().toggle();
										window.scrollTo(0,700);
									}}, "Cache"
								],
								"div", {style:{height:"700px", display:"none", overflow:"auto"}}, [
									"table", {id:"CoupCacheTable", style:{width:"100%"}}, [
										
										"tr", null, [
											"td", {colspan:"5"}, [
												"p", {style:{margin:"10px"}}, "This is the Coup d'Bungie 5 cache. Its contents are here to keep performance at a high standard - meaning, it makes things go faster, like red painted cars. It holds a series of User objects, each of which contain data about Bungie.net users and their associated Coup d'Bungie styles so you don't need to continually fetch them from the server. This script which generates the cache also has automatic cache management functionality built-in, so items will automatically be deleted when they need to be. However, you can also delete items yourself just by clicking the 'Delete' button beside each username. You can also view a user&#39;s cached styles by clicking their username. Note that the Coup d&#39;Bungie cache (the data that is listed below) is NOT the same as your browser&#39;s cache. Do not clear your browser&#39;s cache or your cookies to try to get the script to work."
											]
										],
										
										"tr", {style:{fontWeight:"bold", textDecoration:"underline", fontStyle:"italic"}}, [
											"td", {style:{width:"150px"}}, "Username",
											"td", {style:{width:"200px"}}, "Cache Date",
											"td", {style:{width:"200px"}}, "Last Hit",
											"td", {style:{width:"100px"}}, "Coup5 User",
											"td", null, ""
										]
									
									],
									"div", {style:{margin:"8px", marginLeft:"0px"}}, [
										"input", {value:"Delete All", type:"button",
											onclick:function(){
												if(!confirm("Are you sure you want to delete all items in the cache?")){
													return;
												}
												Cache.DeleteAll();
												Cache.Save();
												$("#CoupCacheTable").find("tr:gt(1)").remove();
											}
										}
									]
								]
							]
						],
						//End Cache section
						
						//Start report section
						"div", {style:{margin:"5px"}}, [
							"fieldset", {style:{border:"1px solid", padding:"5px"}}, [
								"legend", {style:{marginLeft:"5px"}}, [
									"a", {href:"javascript:;", onclick:function(){
										$(this).parent().next().toggle();
										window.scrollTo(0,700);
									}}, "Report"
								],
								"table", {style:{width:"100%", display:"none"}}, [
									"tr", null, [
										"td", {colspan:"2"}, [
											"p", {style:{margin:"10px"}}, "You can use the form below to submit a report about another user's Coup d'Bungie 5 styles. Type in the username of a user you believe has posted inappropriate content, and optionally provide a reason for the report."
										]
									],
									"tr", null, [
										"td", null, [
											"span", null, "Username: ",
											"input", {type:"text", id:"CoupReportSubject", maxLength:16}
										]
									],
									"tr", null, [
										"td", {colspan:"2"}, [
											"textarea", {rows:"6", cols:"40", id:"CoupReportReason", maxLength:255}
										]
									],
									"tr", null, [
										"td", {colspan:"2"}, [
											"input", {type:"button", style:{margin:"8px", marginLeft:"0px"}, value: "Submit Report",
												onclick:function(){
													if(!confirm("Are you sure you want to report this user?")){
														return;
													}
													var reason = $("#CoupReportReason").val();
													var subject = $("#CoupReportSubject").val();
													CoupDBungie.Server.Report(Client.GetUsername(""), Client.GetKey(""), subject, reason,
														function(){
															var obj = this.GetResponseJSON();
															if(obj != null){
																if(obj.Status === CoupDBungie.Server.Responses.OK){
																	alert("Report was successful");
																}
																else{
																	alert("The server said: " + obj.Reason);
																}
															}
															else{
																alert("JSON parsing failed.\n\nHTTP Status: " + this.GetStatus() + "\n\nBody: " + this.GetResponseText());
															}
														},
														function(){
															alert("HTTP Error.\n\nHTTP Status: " + this.GetStatus());
														},
														null
													);
												}
											}
										]
									]
								]
							]
						]
						//End report section
						
					]
				]
			]
		);
		
		var box = document.createElement("div");
		$(box).createAppend("div", {id:"PublishSettingsPreviewBox", className:"PreviewBox", style:{width:"670px", cssFloat:"left", display:"none", position:"fixed", backgroundColor:"#1B1D1F", "z-index":"11", bottom:"0px"}}, null);
		$(box).css({
			"width":"902px",
			"max-width":"902px",
			"margin":"auto"
		});
		$("body").append(box);
		
		$("#PublishSettingsTable input:text").bind("keyup focus", function(){
	
			$("#PublishSettingsPreviewBox > :not(.hideBtnBox)").remove();
			
			var s = new Styles();
			
			s.TitlebarUsernameText = $("#TitlebarUsernameText").val();
			s.TitlebarUsernameTextColor = $("#TitlebarUsernameTextColor").val();
			s.TitlebarTitleText = $("#TitlebarTitleText").val();
			s.TitlebarTitleTextColor = $("#TitlebarTitleTextColor").val();
			s.TitlebarMessageText = $("#TitlebarMessageText").val();
			s.TitlebarMessageTextColor = $("#TitlebarMessageTextColor").val();
			s.TitlebarGroupText = $("#TitlebarGroupText").val();
			s.TitlebarGroupTextColor = $("#TitlebarGroupTextColor").val();
			s.TitlebarBackgroundImage = $("#TitlebarBackgroundImage").val();
			s.TitlebarBackgroundOpacity = $("#TitlebarBackgroundOpacity").val();
			s.TitlebarBackgroundColor = $("#TitlebarBackgroundColor").val();
			s.TitlebarBackgroundGradientLeft = $("#TitlebarBackgroundGradientLeft").val();
			s.TitlebarBackgroundGradientRight = $("#TitlebarBackgroundGradientRight").val();
			s.TitlebarBorderStyle = $("#TitlebarBorderStyle").val();
			s.TitlebarBorderColor = $("#TitlebarBorderColor").val();
			
			s.AvatarImage = $("#AvatarImage").val();
			s.AvatarOpacity = $("#AvatarOpacity").val();
			s.AvatarBorderStyle = $("#AvatarBorderStyle").val();
			s.AvatarBorderColor = $("#AvatarBorderColor").val();
			
			s.PostBackgroundOpacity = $("#PostBackgroundOpacity").val();
			s.PostBackgroundImage = $("#PostBackgroundImage").val();
			s.PostBackgroundImageRepeat = $("#PostBackgroundImageRepeat").val();
			s.PostBackgroundImageAttachment = $("#PostBackgroundImageAttachment").val();
			s.PostBackgroundImagePosition = $("#PostBackgroundImagePosition").val();
			s.PostBackgroundColor = $("#PostBackgroundColor").val();
			s.PostBackgroundGradientLeft = $("#PostBackgroundGradientLeft").val();
			s.PostBackgroundGradientRight = $("#PostBackgroundGradientRight").val();
			s.PostFont = $("#PostFont").val();
			s.PostFontColor = $("#PostFontColor").val();
			s.PostLinkColor = $("#PostLinkColor").val();
			
			$("#PublishSettingsPreviewBox").prepend(MainFunctions.GenerateStylePreview(s));
			$(".PreviewBox").show();
		});
		$("#PublishSettingsTable input:text").bind("blur", function(){
			$(".PreviewBox").hide();
		});

		//Add color wheel
		$(".Coup5ColorWheel").wheelColorPicker();
		$("#PublishSettingsTable input:text").css({"border-style":"solid", "border-radius":"2px", "width":"150px"});
		$("#PublishSettingsTable input:text").val("");
		
		//Add ignored users
		$.each(Array.Distinct(Client.GetIgnoreList([])), function(){
			$("#IgnoreList").append(new Option(this, this));
		});
		var IgnoreList = JSON.parse(Browser.Memory.Get('coup5ignorelist', "{}"));
		for(key in IgnoreList) {
			if($("#IgnoreList option[value='" + key + "']").length < 1){
				$("#IgnoreList").append("<option style='color:gray;' value='" + key + "'>" + key + "</option>");
			} else {
				$("#IgnoreList option[value='" + key + "']").attr('style', 'color:black;');
			}
		}
		
		//Add cached items to table
		for(var i=0; i<Cache.WorkingSet.length; i++){
			$("#CoupCacheTable").createAppend(
				"tr", null, [
				
					"td", null, [
						"a", {href:"javascript:;", onclick:function(){
							var page = Cache.Get(this.innerHTML);
							if(page == null || !page.Data.CoupUser){
								alert("Styles unavailable for this user.");
								return;
							}
							var pre = MainFunctions.GenerateStylePreview(page.Data.Styles);
							var clearableBox = $("<div/>");
							$(clearableBox).css({position:"fixed", top:"10px", right:"10px", padding:"10px", backgroundColor:"#1B1D1F", zIndex:9000});
							$(pre).appendTo(clearableBox);
							clearableBox.createAppend(
								"input", {type:"button", value:"Close", onclick:function(){
									$(this).parent().remove();
								}}
							);
							$(clearableBox).appendTo("body");
						}}, Cache.WorkingSet[i].Data.Username
					],
					
					"td", null, new Date(Cache.WorkingSet[i].Date).toUTCString(),
					"td", null, Cache.WorkingSet[i].LastHit != null ? new Date(Cache.WorkingSet[i].LastHit).toUTCString() : "not hit",
					"td", null, Cache.WorkingSet[i].Data.CoupUser ? "<span style='color:DarkGreen;'>Yes</span>" : "<span style='color:DarkRed;'>No</span>",
					"td", null, [
						"input", {type:"hidden", value:Cache.WorkingSet[i].Data.Username}, null, 
						"input", {type:"button", value:"Delete",
							onclick:function(){
								var page = Cache.Get($(this).prev("input:hidden").val());
								if(page != null){
									Cache.Delete(page);
									Cache.Save();
								}
								$(this).closest("tr").remove();
							}
						}
					]
				]
			);
		}
		
		//Ban history parsing
		$("#ctl00_mainContent_BanHistoryPanel p").each(function(){
			$(this).html($(this).html().replace(/.+(?= \[(\d+)\])/, "<a href=\"?uid=$1\">$&</a>"));
		});
		//End Ban history parsing
		
		//Options global declarations checkbox event
		$(".coup5optionsglobdec input[type='checkbox']").live('click', function(){
			if($(this).is(':checked')){
				Options.Add('coup5options', 'checkbox', $(this).attr('name'), 'checked="checked"');
			}
			else{
				Options.Del('coup5options', 'checkbox', $(this).attr('name'));
			}
		});
		//End options checkbox event
		
		//
		$(".coup5miscoptions input[name='coup5storagetype']").live('click', function(){
			if($(this).is(':checked')){
				localStorage['coup5storagetype'] = 'true';
			} else {
				delete localStorage['coup5storagetype'];
			}
		});
		
		if(localStorage['coup5storagetype']){
			$(".coup5miscoptions input[name='coup5storagetype']").attr('checked', true);
		}
		//
		
		//IgnoreSpawn bindings
		IgnoreLive();
		
		//Options range input event
		$("#coup5options input[type='range']").live('keyup blur', function(){
			var type = $(this).attr('type');
			var name = $(this).attr('name');
			Options.Add('coup5options', type, name, parseFloat($(this).val()));
		});
		//End options range input event
		
		var options = JSON.parse(Browser.Memory.Get('coup5options', "{}"));
		for ( group in options ) {
			if ( group == 'range' ) {
				if ( options.hasOwnProperty(group) ) {
					for ( key in options[group] ) {
						if ( options[group].hasOwnProperty(key) ) {
							$('#coup5options input[type="' + group + '"][name="' + key + '"]').val(options[group][key]);
						}
					}
				}
			}
			else if ( group == 'checkbox' ) {
				if ( options.hasOwnProperty(group) ) {
					for ( key in options[group] ) {
						if ( options[group].hasOwnProperty(key) ) {
							$('#coup5options input[type="' + group + '"][name="' + key + '"]').attr('checked', 'checked');
						}
					}
				}
			}
		}
		
		//Fetch live styles from server for client
		CoupDBungie.Server.GetStyles([Client.GetUsername("")],
			function(){
				var obj = this.GetResponseJSON();
				if(obj != null && obj.Status === CoupDBungie.Server.Responses.OK && obj.Users[0]){
					var s = obj.Users[0].Styles;
					
					$("#TitlebarUsernameText").val(s.TitlebarUsernameText);
					$("#TitlebarUsernameTextColor").val(s.TitlebarUsernameTextColor);
					$("#TitlebarTitleText").val(s.TitlebarTitleText);
					$("#TitlebarTitleTextColor").val(s.TitlebarTitleTextColor);
					$("#TitlebarMessageText").val(s.TitlebarMessageText);
					$("#TitlebarMessageTextColor").val(s.TitlebarMessageTextColor);
					$("#TitlebarGroupText").val(s.TitlebarGroupText);
					$("#TitlebarGroupTextColor").val(s.TitlebarGroupTextColor);
					$("#TitlebarBackgroundImage").val(s.TitlebarBackgroundImage);
					$("#TitlebarBackgroundOpacity").val(s.TitlebarBackgroundOpacity);
					$("#TitlebarBackgroundColor").val(s.TitlebarBackgroundColor);
					$("#TitlebarBackgroundGradientLeft").val(s.TitlebarBackgroundGradientLeft);
					$("#TitlebarBackgroundGradientRight").val(s.TitlebarBackgroundGradientRight);
					$("#TitlebarBorderStyle").val(s.TitlebarBorderStyle);
					$("#TitlebarBorderColor").val(s.TitlebarBorderColor);
					
					$("#AvatarImage").val(s.AvatarImage);
					$("#AvatarOpacity").val(s.AvatarOpacity);
					$("#AvatarBorderStyle").val(s.AvatarBorderStyle);
					$("#AvatarBorderColor").val(s.AvatarBorderColor);
					
					$("#PostBackgroundOpacity").val(s.PostBackgroundOpacity);
					$("#PostBackgroundImage").val(s.PostBackgroundImage);
					$("#PostBackgroundImageRepeat").val(s.PostBackgroundImageRepeat);
					$("#PostBackgroundImageAttachment").val(s.PostBackgroundImageAttachment);
					$("#PostBackgroundImagePosition").val(s.PostBackgroundImagePosition);
					$("#PostBackgroundColor").val(s.PostBackgroundColor);
					$("#PostBackgroundGradientLeft").val(s.PostBackgroundGradientLeft);
					$("#PostBackgroundGradientRight").val(s.PostBackgroundGradientRight);
					$("#PostFont").val(s.PostFont);
					$("#PostFontColor").val(s.PostFontColor);
					$("#PostLinkColor").val(s.PostLinkColor);
					
				}
			},
			null,
			null
		);
		
	},
	
	PostsPage:function(){
		
		var ignoreList = Client.GetIgnoreList();
		var forumItems = $("#ctl00_mainColPanel div.forum_item, div.forum_alt_item");
		var usernames = [];
		var users = [];
		
		//IgnoreSpawn bindings
		IgnoreLive();
		
		function ApplyStyles(){
			forumItems.find("div.forumpost:has(li.login > a)").each(function(){
				//Permalinks
				$(this).find("ul.rightside").createAppend(
					"li", null, [
						"a", {target:"_blank", href:"?postID=" + $(this).parent().find('a[id][name][href!=""]:first').attr("name")}, "permalink to this post"
					]
				);
				//End Permalinks
				//Ignore link
				$(this).find("ul.leftside").createAppend(
					"li", null, [
						"span", null, "coup 5:&nbsp;",
						"a", {href:"javascript:;", 'class':'coup5ignorespawn', name:$(this).find("li.login > a").text()}, "Ignore " + $(this).find("li.login > a").text()
					]
				);
				//End ignore link
				var ___user = $(this).find( "li.login a" ).text(); if ( Array.Contains( Client.GetIgnoreList() , ___user ) ) return true;
				var page = Cache.Get($(this).find("li.login a").text());
				if(page != null && page.Data.Styles != null){
					$(this).css("width", "670px"); //Fix background being off (may need to add this to function below)
					MainFunctions.ApplyStylesToElement(page.Data.Styles, this);
				}
			});
		}
		
		Console.Log("Getting forumItems");
		
		//Filter for cached items, removing those which are cached
		forumItems.find("div.forumpost li.login > a").each(function(){
			var username = $(this).text();
			if(!Cache.Exists(username)){
				usernames.Add(username);
			}
		});
		
		usernames.Distinct(); //Eliminate duplicates
		usernames.Filter(ignoreList); //Filter by ignore list
		
		//Get the rest from the server
		if(usernames.length >= 1){
			CoupDBungie.Server.GetStyles(usernames,
				function(){
					var obj = this.GetResponseJSON();
					if(obj != null && obj.Status === CoupDBungie.Server.Responses.OK){
						
						//Add Coup5 users
						$.each(obj.Users, function(){
							users.Add(new User(this.Username, this.Id, this.Styles, true)); //Add to users array
							usernames.DeleteAll(this.Username); //Remove the user from the usernames array
						});
						
						//Add non Coup5 users
						$.each(usernames, function(){
							users.Add(new User(this, 0, null, false));
						});
						
						//Add everyone to cache
						$.each(users, function(){
							Cache.Add(new Cache.Page(this));
						});
						
						ApplyStyles();
						Cache.Save();
						
					}
					else{
						Console.Log("JSON parsing failed for GetStyles request");
					}
				},
				null,
				null
			);
		}
		else{
			ApplyStyles(); //This MUST be within the else block. DON'T put it under the if block
		}
		
	}

}


function Main(args){
	if(Client.IsSignedIn() && CoupDBungie.Initialise()){
		var url = location.href;
		if(/\/account\/profile\.aspx(#CoupDBungie5)?$/i.test(url)){
			MainFunctions.ClientProfilePage();
		}
		//else if(/account\/profile\.aspx?(memberID|userID)=\d+(#CoupDBungie5)?$/i.test(url)){
		//	MainFunctions.OtherUserProfilePage();
		//}
		else if(/\/(fanclub\/.+?\/)?forums\/posts\.aspx\?postID=\d+.*$/i.test(url)){
			MainFunctions.PostsPage();
		}
	}
	var stylesheet = "<style type=\"text/css\">\
		#CoupDBungie5 tr:nth-child(odd) {background-color:rgba(255,255,255,0.07);}\
		</style>"
	$("body").append(stylesheet);
}

Main();
