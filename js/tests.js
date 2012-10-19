
intercomLocation="/talk";
intercom=new ks.Intercom({url:intercomLocation,debug:true});

/**
   runs the given function with the (optional) given initialResults and checks the results against the object in expected. The test gets duration ms to complete

   the specs object can have the following values:
   - expected:: expected results object, results may contain more properties than expected! (required),
   - toTest:: the function to test with one argument that corresponds to the results object (required),
   - duration:: the time the test has to run, after this time the results are examined,
   - initialResults:: the initial results object (optional),
   - after:: the function to call after the test has completed (and no error was produced), the arguments of this function are the specs itself (optional);
 */
function asyncTest(specs){
    var temp=specs.initialResults || {};
    var results={};
    for(var prop in temp){
        results[prop]=temp[prop];
    }
    specs.results=results;

    var processTestResults=function(){
        var failed=false;
        for(var prop in specs.expected){
            if(specs.expected[prop]!=results[prop]){
                failed=true;
            }
        }

        if(failed){
            specs.results.intercom.destroy();
            throw new Error("Did not receive the correct test results! \nExpected: "+
                            JSON.stringify(specs.expected)+"\nReceived: "+
                            JSON.stringify(results));
        }

        if(specs.after){
            specs.after.call(this,specs);
        }

        if(!specs.dontLog && console && console.log){console.log("Test succeeded: "+specs.toTest.name);}
    };

    setTimeout(processTestResults,specs.duration);

    specs.toTest.call(this,specs.results);    
}
function testStandard(results){
    results.intercom=intercom;
    var content="foobar";

    results.id=intercom.call(
        {name:"echo",
         args:[{string:content,
                count:3,
                interval:1000}],
         keys:["ready"],
         onResponse:function(response){
             results.count+=1;
             if(response.type!="value"|| response.body!=content){
                 results.error="content did not have expected format";
             }
         },
         minSpeed:100,
         onFinish:function(response){
             if(response.body!=true){
                 results.error="final content did not have expected format";
             }else{
                 results.requestFinished=true;
             }
         }
        });
};
asyncTest({
    expected: {count:3,
               error:null,
               requestFinished:true},
    toTest: testStandard,
    duration: 40000,
    initialResults:{count:0,
                    error:null},
    after:function(specs){
        var results=specs.results;
        if(results.intercom.currentRequests[results.id]!==undefined){
            throw new Error("request not correctly removed from currentRequests");
        }
        startCancelTest();
    }
});
function testCancel(results){
    results.intercom=intercom;
    var content="foobar";
    var interval=1000;

    results.id=intercom.call(
        {name:"echo",
         args:[{string:content,
                count:3,
                interval:interval}],
         keys:["ready"],
         minSpeed:100,
         onResponse:function(response){
             results.count+=1;
             if(response.type!="value"|| response.body!=content){
                 results.error="content did not have expected format";
             }
         },
         onFinish:function(response){
             if(response.body!=true){
                 results.error="final content did not have expected format";
             }
         }
        });

    setTimeout(function(){
        results.intercom.cancel(results.id);
    },interval+interval/2);
};
function startCancelTest(){
    asyncTest({
        expected: {count:1,
                   error:null},
        toTest: testCancel,
        duration: 3000,
        initialResults:{count:0,
                        error:null},
        after:function(specs){
            var results=specs.results;
            if(results.intercom.currentRequests[results.id]!==undefined){
                throw new Error("request not correctly removed from currentRequests");
            }
            startDestroyTest();
        }
    });
}
function testDestroy(results){
    results.intercom=intercom;
    var content="echo1";
    var interval=1000;

    results.firstId=intercom.call(
        {name:"echo",
         minSpeed:100,
         args:[{string:content,
                count:3,
                interval:interval}],
         keys:["ready"],
         onResponse:function(response){
             results.count+=1;
             if(response.type!="value"|| response.body!=content){
                 results.error="content did not have expected format";
             }
         },
         onFinish:function(response){
             if(response.body!=true){
                 results.error="final content did not have expected format";
             }
         }
        });

    var secondContent="echo2";
    results.secondId=intercom.call(
        {name:"echo",
         minSpeed:100,
         args:[{string:secondContent,
                count:3,
                interval:interval}],
         keys:["ready"],
         onResponse:function(response){
             results.count+=1;
             if(response.type!="value"|| response.body!=secondContent){
                 results.error="content did not have expected format";
             }
         },
         onFinish:function(response){
             if(response.body!=true){
                 results.error="final content did not have expected format";
             }
         }
        });

    setTimeout(function(){
        results.intercom.destroy();
    },interval+interval/2);
};

function startDestroyTest(){
    asyncTest({
        expected: {count:2,
                   error:null},
        toTest: testDestroy,
        duration: 3000,
        initialResults:{count:0,
                        error:null},
        after:function(specs){
            var results=specs.results;
            if(results.intercom.currentRequests[results.firstId]!==undefined ||
               results.intercom.currentRequests[results.secondId]!==undefined){
                throw new Error("request not correctly removed from currentRequests");
            }
            intercom=new ks.Intercom({url:intercomLocation,debug:true});
            testFlood();
        }
    });
}
function testFloodSingle(results){
    results.intercom=intercom;
    var content="foobar";
    
    results.id=intercom.call(
        {name:"echo",
         args:[{string:content,
                count:3,
                interval:1000}],
         keys:["ready"],
         minSpeed:100,
         onResponse:function(response){
             results.count+=1;
             if(response.type!="value"|| response.body!=content){
                 results.error="content did not have expected format";
             }
         },
         onFinish:function(response){
             if(response.body!=true){
                 results.error="final content did not have expected format";
             }else{
                 results.requestFinished=true;
             }
         }
        });
};

function testFlood(){
    var testSucceeded=[];
    var testCount=100;
    for(var i=0;i<testCount;i++){
        asyncTest({
            expected: {count:3,
                       error:null,
                       requestFinished:true},
            dontLog:true,
            toTest: testFloodSingle,
            duration: 20000,
            initialResults:{count:0,
                            error:null,
                            idx:i
                           },
            after:function(specs){
                var results=specs.results;
                if(results.intercom.currentRequests[results.id]!==undefined){
                    results.intercom.destroy();
                    throw new Error("request not correctly removed from currentRequests");
                }
                testSucceeded.push(specs);
                if(testSucceeded.length==testCount){
                    console.log("flooding test succeeded");
                    results.intercom.destroy();
                }
            }
        });
    }
}
