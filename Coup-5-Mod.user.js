// ==UserScript==
// @name            Mod d'Bungie
// @namespace       https://github.com/Shou-/Coup-5
// @description     Personlize your bungie.net experience
// @version         0.108
// @include         http*://*bungie.net/*
// @exclude         http*://*bungie.net/*createpost.aspx*
// @exclude         http*://*bungie.net/Account/Playtest/*
// @exclude         http*://*bungie.net/Account/Settings.aspx*
// @author          Shou
// @copyright       2012, Shou
// @license         (CC) Attribution Non-Commercial Share Alike; http://creativecommons.org/licenses/by-nc-sa/3.0/
// ==/UserScript==

// TODO:
// - Finish UI.
//      - Add a preview Coup function to the pop-up?
// - Change script download path.
//      - Uploading it to Github is a prerequisite to this.
// - Make sure Opera and Chrome stuff works.

// XXX:

// FIXME:

// Vim:
// :set expandtab
// :set foldenable

// {{{ Debugging

// Console :: Object
var Console = {
    // Log :: String -> IO ()
    Log:function(a){
        if(Coup.Debug){
            console.log(a);
        }
    }
}

// }}}

// {{{ Utils

// | Curry a function.
// cu :: (a -> b -> c) -> (b -> c)
function cu(f, a){
    return function(b){ return f(a, b); }
}

// | Compose two functions.
// co :: (b -> c) -> (a -> b) -> c
function co(f, g){
    return function(a){ return f(g(a)); }
}

// take :: String -> Int -> String
function take(str, n){
    return str.substr(0, n);
}

// getPOSIXTime :: IO Int
function getPOSIXTime(){
    return Math.round((new Date()).getTime() / 1000);
}

// }}}

// {{{ Browser

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
        Console.Log("Determining whether browser supports Coup-5-Mod");
        if(localStorage && XMLHttpRequest && JSON){
            Console.Log("Browser supports Coup-5-Mod");
            return true;
        }
        Console.Log("Browser does not support Coup-5-Mod");
        return false;
    },
    XHR:function(method, url, async, headers, onload, onerror, onreadystatechange){
        this._Init(method, url, async, headers, onload, onerror, onreadystatechange);
    },
    Type:{
        Get:function(){
            if (navigator.vendor === "Google Inc.") return "webkit";
            else if (window.opera) return "opera";
            else return "mozilla";
        },
        ScriptUrl:function(){
            var browser = Browser.Type.Get();

            if (browser === "webkit")
                return "https://github.com/downloads/Shou-/Coup-5/Coup-5-Chrome.crx";
            else if (browser === "opera")
                return "https://github.com/downloads/Shou-/Coup-5/Coup-5-Opera.zip";
            else
                return "https://github.com/Shou-/Coup-5/raw/master/coup-5.user.js";
        },
        Platform:function(){
            var browser = Browser.Type.Get();

            if (browser === "webkit") return "Google Chrome";
            else if (browser === "opera") return "Opera";
            else return "Firefox";
        }
    }
}

// }}}

// {{{ Coup

var Coup = { Debug: true

           , Version: "0.1"
             // Platform :: IO String
           , Platform: Browser.Type.Platform()
           , Author: "Shou"
           , AuthorMemberID: 2503535

           , Server: { Hosts: [ "http://test.coup-srv-01.heliohost.org/" ]
                     , Path: "API/Services/Styles/Admin"
                     , Method: "POST"
                     , Responses: { OK: 1
                                  , Error: 2
                                  }
                     }
             // Key :: IO Object
           , Key: function(){
                return Browser.Memory.Get("coup5key", "{}");
             }
           , Username:function(defaultVal){
                var match = /BungieDisplayName=(.*?)(?:;|$)/i.exec(document.cookie);
                return match != null && match[1] != undefined ? unescape(match[1]).replace(/&nbsp;/gi, " ") : defaultVal;
             }
           }

// }}}

// {{{ Post functions/objects

// SendPost :: (String -> IO ()) -> AdminObject -> IO ()
function SendPost(ao, f){
    var xhr;
    var url = Coup.Server.Hosts[0] + Coup.Server.Path;
    var data = JSON.stringify(ao);

    var update = function(state, status, data){
        if (state == 4 && status == 200) f(data);
        else {
            var readyState;
            switch(state){
                case 0: readyState = "UNSENT"; break;
                case 1: readyState = "OPENED"; break;
                case 2: readyState = "HEADERS_RECEIVED"; break;
                case 3: readyState = "LOADING"; break;
                case 4: readyState = "DONE"; break;
            }
            var reason = "{ readyState: " + readyState;
            reason += ", status: " + status + " }";
            Console.Log("SendPost: " + reason);
        }
    }

    try {
        xhr = new GM_xmlhttpRequest(
            { method: Coup.Server.Method
            , url: url
            , data: "json=" + data
            , synchronous: false
            , onreadystatechange: function(r){
                update(r.readyState, r.status, r.responseText);
              }
            }
        );
        Console.Log("SendPost: Using GM XHR.");
    } catch(e){
        Console.Log("SendPost: Using built-in XHR (" + e + ").");
        xhr = new XMLHttpRequest();
        xhr.onreadystatechange = function(r){
            update(r.readyState, r.status, r.responseText);
        }
        xhr.open(Coup.Server.Method, url, true);
        xhr.send(data);
    }
}

// AdminPost :: String -> String -> Int -> AdminObject
function AdminPost(u, k, o){
    this.Username = u; // :: String
    this.Key = k; // :: String
    this.Operation = o; // :: Int
}

// Blacklist :: String -> String -> Int -> Bool -> MethodObject Blacklist
function Blacklist(u, r, e, p){
    this.Username = u; // :: String
    this.Reason = r; // :: String
    this.Expiration = e; // :: Int
    this.Permanent = p; // :: Bool
}

// Unblacklist :: String -> MethodObject Unblacklist
function Unblacklist(u){
    this.Username = u; // :: String
}

// ResetStyles :: String -> MethodObject ResetStyles
function ResetStyles(u){
    this.Username = u; // :: String
}

// ResolveReport :: Int -> Int -> MethodObject ResolveReport
function ResolveReport(i, r){
    this.Id = i; // :: Int
    this.Resolution = r; // :: Int
}

// | Where n is in the 1 - 5 range.
// modSubmit :: Int -> IO Bool
function modSubmit(n){
    var uielem = document.getElementById("Coup5ModUI");
    var ins = uielem.children[0].children[0].getElementsByTagName("input");
    var Username = Coup.Username()
    var Key;
    try {
        Key = JSON.parse(Coup.Key())[Username];
    } catch(e){
        alert("modSubmit: Unable to parse `coup5key'; try re-registering?");
        return false;
    }
    var adminPost = new AdminPost(Username, Key, n);

    switch(n){
        case 1:
            var reason = ins[1];
            var expiration = ins[2];
            var perm = ins[3];
            var uname = ins[4];

            uname = uname.value;
            reason = reason.value;
            expiration = parseInt(expiration.value);
            if (expiration > 0 && expiration < 15)
                expiration = getPOSIXTime() + 60 + expiration * 86400;
            perm = perm.checked;

            adminPost.Blacklist = new Blacklist(uname, reason, expiration, perm);

            if (adminPost.Username === uname){
                var msg = [ "Committing Couppuku"
                          , "( ﾟ∀ﾟ)ｱﾊﾊ八八ﾉヽﾉヽﾉヽﾉ ＼ / ＼/ ＼"
                          ]
                if (!confirm(msg.join(' '))) return false;
            }
            break;

        case 2:
            var uname = ins[1];
            uname = uname.value;
            adminPost.Unblacklist = new Unblacklist(uname);
            break;

        case 3:
            var uname = ins[1];
            uname = uname.value;
            adminPost.ResetStyles = new ResetStyles(uname);
            break;

        case 4:
            adminPost.GetReports = {};
            break;

        case 5:
            var id = ins[1];
            var resolution = ins[2];
            var uname = ins[3];

            id = parseInt(id.value);
            resolution = parseInt(resolution.value);

            adminPost.ResolveReport = new ResolveReport(id, resolution);
            break;

        default:
            Console.Log("modSubmit: `" + n + "' is outside range (1 - 5).");
    }

    SendPost(adminPost, function(xs){
        var emsg = document.getElementById("Coup5ModSMsg");
        var obj;
        try {
            obj = JSON.parse(xs);
        } catch(e){
            obj = {};
        }
        for (e in obj)
            if (e == "Reason") emsg.textContent = obj[e];
    });
}

// }}}

// {{{ Interface

// {{{ Style
var style = "\
#Coup5ModUI form input, #Coup5ModUI form select {\
    background-color: #1b1d1f;\
    color: #b0b0b0;\
    border: 1px solid #707070;\
    font-size: 9pt;\
    font-family: arial;\
    margin: 4px;\
}\
\
#Coup5ModUI form input[type='submit']:hover, #Coup5ModUI form select:hover {\
    background-color: #17668a;\
    border: 1px solid #56aacd;\
    cursor: pointer;\
}\
\
ul.leftside { height: auto !important }\
";
// }}}

// spawnMod :: IO ()
function spawnMod(){
    var aheader = this.parentNode.parentNode.parentNode.parentNode.children[0];
    var title = aheader.children[0].children[0].title;
    var text = aheader.children[0].children[0].textContent;
    var username = title || text;
    Console.Log("spawnMod: Spawning UI for `" + username + "'.");

    var ui = mkUI(username, 1);
    var oui = document.getElementById("Coup5ModUI");
    if (oui){
        oui.parentNode.replaceChild(ui, oui);
    } else {
        var floater = mkFloater();
        floater.children[0].children[1].appendChild(ui);
        document.body.appendChild(floater);
    }
}

// mkFloater :: IO DOMObj
function mkFloater(id){
    var wrap = document.createElement("div");
    wrap.className = "coup5floater";
    wrap.style.height = "0px"
    wrap.style.width = "400px";
    wrap.style.margin = "auto";
    if (id != undefined) wrap.id = id;

    var content = document.createElement("div");
    content.style.marginTop = "10%";
    content.style.position = "fixed";
    content.style.zIndex = 9001;
    content.style.backgroundColor = "rgba(0, 0, 0, 0.8)";
    content.style.border = "1px solid";
    content.style.top = 0;
    content.style.padding = "0px";

    var cwrapper = document.createElement("div");
    cwrapper.style.margin = "30px 10px 10px 10px";

    var closer = document.createElement("span");
    closer.style.color = "darkRed";
    closer.style.cursor = "pointer";
    closer.style.cssFloat = "right";
    closer.style.padding = "0px 3px 0px 0px";
    closer.textContent = "X";
    closer.addEventListener("click", function(e){
        var e = this.parentNode.parentNode;
        e.parentNode.removeChild(e);
    });

    content.appendChild(closer);
    content.appendChild(cwrapper);
    wrap.appendChild(content);

    return wrap;
}

// | Where n is in the 1 - 5 range.
// mkUI :: String -> IO DOMObj
function mkUI(u, n){
    var boxDo = document.createElement("div");
    var boxDi = document.createElement("div");
    var boxD = document.createElement("div");
    var h3 = document.createElement("h3");
    var wrap = document.createElement("div");
    var form = document.createElement("form");
    var submit = document.createElement("input");
    var select = document.createElement("select");
    var br = document.createElement("br");
    var uinput = document.createElement("input");
    var serverMsg = document.createElement("div");
    var operations = [ "Blacklist"
                     , "Unblacklist"
                     , "Reset styles"
                     , "Get reports"
                     , "Resolve report"
                     ]

    // Add options to `select'.
    for (i = 0; i < operations.length; i++){
        var e = document.createElement("option");
        e.value = i + 1;
        e.textContent = operations[i];
        select.appendChild(e);
        if (i + 1 === n) e.selected = true;
    }

    var inputs = [];

    // Inputs appropriate to `n'.
    if (n == 1){
        var reason = document.createElement("input");
        var time = document.createElement("input");
        var perm = document.createElement("input");

        reason.placeholder = "Reason";
        reason.name = "Reason";
        time.type = "range";
        time.value = 1;
        time.min = 1;
        time.max = 14;
        time.placeholder = "Range: 1 - 14 (days)";
        time.name = "Expiration: 1 day(s).";
        time.addEventListener("change", function(){
            this.title = "Expiration: " + this.value + " day(s).";
        });
        perm.type = "checkbox";
        perm.name = "Permanent";

        inputs.push(reason);
        inputs.push(time);
        inputs.push(perm);
    } else if (n == 5){
        var id = document.createElement("input");
        var resolution = document.createElement("select");
        var ress = { "Good report": "The report was good, a rule breaker was reported."
                   , "Understandable report": "The report was understandable, but not worthy of punishment."
                   , "Bad report": "The report was a waste of time."
                   }
        id.name = "Id";

        var i = 1;
        for (r in ress){
            var e = document.createElement("option");
            e.value = i;
            e.textContent = r;
            e.title = ress[r];
            resolution.appendChild(e);
            i++;
        }

        inputs.push(id);
        inputs.push(resolution);
    }

    // Attributes
    boxDo.className = "boxD_outer";
    boxDo.id = "Coup5ModUI";
    boxDo.style.margin = "-10px 0 20px";
    boxDi.className = "boxD_inner";
    boxD.className = "boxD";
    h3.textContent = "Coup-5 Mod: " + u;
    wrap.style.margin = "10px 5px 5px 15px";
    submit.type = "submit";
    submit.value = "Submit";
    uinput.value = u;
    uinput.type = "hidden";
    uinput.name = "Username";
    serverMsg.id = "Coup5ModSMsg";
    serverMsg.style.margin = "10px 0";
    serverMsg.style.cursor = "pointer";

    // Events
    form.addEventListener("submit", function(e){
        e.preventDefault();

        var n;
        for (i = 0; i < select.children.length; i++){
            if (select.children[i].selected) n = i + 1;
        }
        if (e != undefined){
            return modSubmit(n);
        } else {
            Console.Log("Form `submit': Selected option not found.");
        }
    });
    select.addEventListener("change", function(){
        var e;
        for (i = 0; i < select.children.length; i++){
            if (select.children[i].selected) e = select.children[i];
        }
        if (e != undefined){
            var ui = mkUI(u, parseInt(e.value));
            var oui = document.getElementById("Coup5ModUI");
            oui.parentNode.replaceChild(ui, oui);
        } else {
            Console.Log("Select `change': Selected option not found.");
        }
    });
    serverMsg.addEventListener("click", function(){
        this.textContent = "";
    });

    form.appendChild(select);
    form.appendChild(submit);
    form.appendChild(br);
    for (i = 0; i < inputs.length; i++){
        if (inputs[i].placeholder == "")
            inputs[i].placeholder = inputs[i].name;
        inputs[i].title = inputs[i].name;
        form.appendChild(inputs[i]);
    }
    form.appendChild(uinput);
    wrap.appendChild(form);
    wrap.appendChild(serverMsg);
    boxD.appendChild(h3);
    boxD.appendChild(wrap);
    boxDi.appendChild(boxD);
    boxDo.appendChild(boxDi);

    return boxDo;
}

// insertUI :: IO ()
function insertUI(){
    if (window.location.pathname.match(/\/Account\/Profile\.aspx/i)){
        Console.Log("insertUI: Adding Profile UI.");
        var uid = "ctl00_mainContent_header_lblUsername";
        var username = document.getElementById(uid).textContent;
        var ui = mkUI(username, 1);

        var pid = "ctl00_mainContent_profilePanel";
        var coup5publish = document.getElementById("CoupDBungie5");
        if (coup5publish)
            coup5publish.parentNode.insertBefore(ui, coup5publish)
        else {
            var profilePanel = document.getElementById(pid);
            profilePanel.appendChild(ui);
        }

    } else if (window.location.pathname.match(/Posts\.aspx/i)) {
        Console.Log("insertUI: Adding Posts UI.");
        var sigs = document.getElementsByClassName("leftside");
        for (i = 0; i < sigs.length; i++){
            var title = sigs[i].parentNode.parentNode.children[0].children[0].children[0].title;
            var text = sigs[i].parentNode.parentNode.children[0].children[0].children[0].textContent;
            var username = title || text;
            var li = document.createElement("li");
            li.innerHTML = "<span>Coup-5-Mod:&nbsp;</span>";
            var a = document.createElement("a");
            a.href = "javascript:;";
            a.className = "coup5modspawn";
            a.textContent = username;
            a.addEventListener("click", spawnMod);
            li.appendChild(a);
            sigs[i].appendChild(li);
        }
    }
    Console.Log("Successfully added UI.");
}

// styleUI :: IO ()
function styleUI(){
    var s = document.createElement("style");
    s.type = "text/css";
    s.innerHTML = style;

    document.head.appendChild(s);
}

// }}}

// main :: IO ()
function main(){
    if (Browser.SupportsCoupDBungie()){
        insertUI();
        styleUI();
    } else {
        var reason = "Your browser does not support Coup-5-Mod.";
        reason += "\nlocalStorage: " + localStorage ? true : false;
        reason += "\nXMLHttpRequest: " + XMLHttpRequest ? true : false;
        reason += "\nJSON: " + JSON ? true : false;
        alert(reason);
    }
}

main();
