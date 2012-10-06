
//* check if we are free to use the ks namespace. Ensure that this is the final library that is loaded to be entirely sure.
if(window.ks===undefined){
    ks={};
}else if(typeof ks != "object"){
    throw new Error("Sorry, the ks namespace seems to be taken, please adjust the source code to cover this issue");
}

//* constructor for an Intercom object. It expects an object that can have the objects as defined in the intercom recepy.
ks.Intercom= function(args){
    //* initialize properties that would otherwise be used in static fashion
    this.currentRequests={};
    this.requestStack=[];
    
    for(var prop in args){
        this[prop]=args[prop];
    }

    this.pollServer();

    var toBeDestroyed=this;
    //ensure the intercom's messages are cancelled when the browser window closes
    if(window.onunload){
        var temp=window.onunload;
        window.onunload=function(){
            temp();
            toBeDestroyed.destroy();
        }
    }else{
        window.onunload=function(){
            toBeDestroyed.destroy();
        }
    }
};

//* recepy for the intercom class. Will be inserted into the prototype of the ks.Intercom function.
var intercomRecipe={
    //* whether or not to log all incoming results
    debug:false,    
    url:"http://server/intercom/talk",
    //* how fast the server is polled when there are no outstanding requests in ms
    pollTimeSlow:2000,
    //* how fast the server is polled when there are outstanding requests in ms
    pollTimeFast:100,

    //* current outstanding requests
    currentRequests:null,

    //* @public
    //* makes a new call on the server with the given message. A new id is generated for this request and this id is returned.
    call:function(request){
        var id=this.newRequestId(request);
        request.rid=id;
        
        this.currentRequests[id]=request;
    
        // keep only relevant properties
        var shortenedRequest={};
        shortenedRequest.rid=request.rid;
        shortenedRequest.name=request.name;
        shortenedRequest.args=request.args;
        
        this.pollServer({open:[shortenedRequest]});
        
        return id;
    },
    
    //* cancels the request with the given id(s) on the server.
    cancel:function(requestId){
        var message={close:[]};
        if(typeof requestId=="object" && requestId.length && requestId.push){
            message.close=requestId;
        }else{
            message.close.push(requestId);
        }
        this.pollServer(message);
        this.complete(requestId);
    },
    
    /**
       marks the request with the given id as complete. Providing a list of ids is also supported.
       NOTE: this function does NOT inform the server that the request should no longer be remembered
    ,*/
    complete:function(requestId){
        if(typeof requestId=="object" && requestId.length && requestId.push){
            for(var i=0, id;id=requestId[i];i++){
                delete this.currentRequests[id];
            }
        }else{
            delete this.currentRequests[requestId];
        }
    },
    talking:false,
    requestStack: null,
    //* asks the server for new messages. args can contain a javascript object with open and close properties. These hold the new requests that are initiated or the requests that got removed
    talk:function(args){
        var message={};
        if(args){
            if(args.open){
                message.open=args.open;
            }
            if(args.close){
                message.close=args.close;
            }
        }
    
        if(this.talking){
            this.requestStack.push(message);
        }else{
            this.talking=true;
            try{
                this.sendRequest(message);
            }catch(error){
                if(console){
                    if(console.error){
                        console.error(error.message?error.message:error);
                    }else if(console.log){
                        console.log(error.message?error.message:error);
                    }
                }
            }
        }
    },
    
    //* merges all current requests in the requeststack into one single message and returns this message. Returns null if no message needs to be sent
    mergeRequestStack:function(){
        var message={open:[], close:[]};
        if(this.requestStack.length<=0){
            return null;
        }
        for(var i=0;i<this.requestStack.length;i++){
            var request=this.requestStack[i];
            if(request.open){
                message.open=message.open.concat(request.open);
            }
            if(request.close){
                message.close=message.close.concat(request.close);
            }       
        }
        return message;
    },
    //* when a reply is received, the intercom responds to any requests that received new information and empties the requestStack if it is present
    handleReply:function(reply){
        this.talking=false;
        
        this.respondToRequests(reply);
        
        var followUpRequest=this.mergeRequestStack();
        if(followUpRequest){
            this.requestStack=[];
            this.talk(followUpRequest);
        }
    },
    halted:false,
    pollTimeout:null,
    //* polls the server for a new message and queues a new request after either pollTimeSlow ms if no requests remain to be handled or pollTimeFast ms if there are requests that should be handled. (args are the arguments to be passed in to the talk function
    pollServer:function(args){
        if(this.halted){
            return;
        }
    
        if(this.pollTimeout){
            clearTimeout(this.pollTimeout);
        }
        
        this.talk(args);
    
        var openRequests=false;
        for(var prop in this.currentRequests){
            openRequests=true;
            break;
        }
    
        var context=this;
        var nextPoll=function(){
            context.pollServer.call(context);
        };
        
        this.pollTimeout=setTimeout(nextPoll,openRequests?this.pollTimeFast:this.pollTimeSlow);
    },
    stopPolling:function(){
        this.halted=true;
    },
    //* handles all new information that the server sends our way.
    respondToRequests:function(responses){
        if(this.debug && console && console.log && responses.length>0){
            console.log("server sent message: "+JSON.stringify(responses));
        }
        for(var i=0;i<responses.length;i++){
            var response=responses[i];
            var requestId=response.rid;
            var request=this.currentRequests[requestId];
            if(request){
                if(request.keys && request.keys.indexOf(response.type)>=0){
                    request.onFinish(response);
                    this.complete(requestId);
                }else{
                    request.onResponse(response);
                }
                
            }else if(console && console.log){
                console.log("received information on event that we were not tracking!");
            }
        }
    },
    //* sends a http request to the server
    sendRequest:function(requestObject){
        var httpRequest;
        if (window.XMLHttpRequest) { // Mozilla, Safari, ...
            httpRequest = new XMLHttpRequest();
        } else if (window.ActiveXObject) { // IE 8 and older
            httpRequest = new ActiveXObject("Microsoft.XMLHTTP");
        }
    
        var context=this;
        httpRequest.onreadystatechange = function(){
            context.handleReadyStateChanged.call(context,httpRequest);
        };
    
        var open=requestObject.open?JSON.stringify(requestObject.open):[];
        var close=requestObject.close?JSON.stringify(requestObject.close):[];
        
        httpRequest.open('GET', this.url+"?"+(open.length>0?"open="+open:"")+(close.length>0?"&close="+close:""));
        httpRequest.setRequestHeader('Content-Type','application/json');
        httpRequest.send(null);    
    },
    //* handles the response of the server
    handleReadyStateChanged:function(request){
        if(request.readyState === 4){
            // request has been handled
            if(request.status === 200){
                var response= JSON.parse(request.responseText);
                this.handleReply(response);
            }else if(request.onError){
                request.onError(request);
            }else if(console && console.log){
                console.log("Sorry, apparently something went horribly wrong! The server responded with a "+request.status+ " error code...");
            }
        }else{
            // no ready yet!
        }
    },
    //* encodes the given javascript object so it can be sent to the server, credit goes to enyojs.
    objectToQuery: function(/*Object*/ map) {
        var enc = encodeURIComponent;
        var pairs = [];
        var backstop = {};
        for (var name in map){
            var value = map[name];
            if (value != backstop[name]) {
                var assign = enc(name) + "=";
                if (value.length!==undefined && typeof value != "string") {
                    for (var i=0; i < value.length; i++) {
                        pairs.push(assign + enc(value[i]));
                    }
                } else {
                    pairs.push(assign + enc(value));
                }
            }
        }
        return pairs.join("&");
    }  ,

    MONTH:2678400000,
    //* builds a new id for the given request
    newRequestId:function(request){
        // assume a request can run max thirty days and that it is improbable that more than 10000 requests are made per ms per session.
        var id=new Date().getTime()%(this.MONTH)*1000000+Math.floor(Math.random()*1000000);
        return id+"";
    },

    //* upon destruction, all open requests are cancelled.
    destroy:function(){
        var rids=[];
        for(var idx in this.currentRequests){
            rids.push(this.currentRequests[idx].rid);
        }
        this.cancel(rids);
        this.stopPolling();
        this.currentRequests={};
    }    
};

//* insert the recepy into the prototype
for(var prop in intercomRecipe){
    ks.Intercom.prototype[prop]=intercomRecipe[prop];
}
