window.connections = [];

var _pedro_gutierrez$project$Native_WebRTC = function() {
    
    var $scheduler = _elm_lang$core$Native_Scheduler;
    var $nb = $scheduler.nativeBinding;
    var $succeed = $scheduler.succeed;
    var $fail = $scheduler.fail;
    
    var pc = null;



    function hangup(settings) {
        return $nb(function(cb) {
            if (!pc) {
                cb($fail("No active call"));   
            } else {
                pc.close();
                cb($succeed("someid"))
            }
        });
    }

    function createConnection(onIce, onAddStream, ok, err) {
        if( pc != null ) {
            err("Call busy")
        } else {
            pc = new RTCPeerConnection({
                iceServers: [
                    { urls: 'stun:stun.l.google.com:19305' }
                ]
            });

            pc.onicecandidate = function(e) {
                if (!e || !e.candidate) return;
                console.log("got candidate", e.candidate);
                onIce(e.candidate.candidate);
            }
            
            pc.onaddstream = function(e) {
                console.log("got stream", e);
                onAddStream(e);
            };

            console.log("created peer connection", pc );

            navigator.getUserMedia({
                audio: true,
                video: true
            }, function(stream) {
                console.log("got stream from user media", stream);
                pc.addStream(stream);
                ok(pc, URL.createObjectURL(stream));
            }, function(error) {
                err(error);
            });
        }
    };
        
    function answer(sdp, ice, settings) {
        return $nb(function(cb) {
            createConnection(function (ice) {
                $scheduler.rawSpawn(A2(settings.onIce, ice, "someId")); 
            }, function(stream) {
                console.log("got stream", stream);
            }, function(pc, streamUrl) {
                var desc = new RTCSessionDescription({type: 'offer', sdp: sdp});
                pc.setRemoteDescription(desc)
                    .then(function() {
                        for (var i=0; i<ice.length; i++) {
                            pc.addIceCandidate(new RTCIceCandidate({
                                candidate: ice
                            }));
                        }
                        return pc.createAnswer();
                    })
                    .then(function(ans) {
                        return pc.setLocalDescription(new RTCSessionDescription(ans));
                    })
                    .then(function() {
                        cb($succeed({
                            sdp: pc.localDescription.sdp,
                            url: streamUrl
                        }));
                    })
                    .catch(function(err){
                        cb($fail(err));
                    });
            }, function(err) {
                cb($fail(err));
            });
        }, function(err) {
            cb($fail(err));
        });
    }
    
    function addSdp(sdp) {
        return $nb(function(cb) {
            if (!pc) {
                cb($fail("No active call"));   
            } else {
                pc.setRemoteDescription(new RTCSessionDescription({
                    type: 'answer',
                    sdp: sdp
                }));
                cb($succeed(sdp))
            }
        });
    }

    function addIce(ice) {
        return $nb(function(cb) {
            if (!pc) {
                cb($fail("No active call"));   
            } else {
                pc.addIceCandidate(new RTCIceCandidate({
                    candidate: ice
                }));
                cb($succeed(sdp))
            }
        });

    }
    
    function call(settings) {
        return $nb(function(cb) {
            createConnection(function (ice) {
               $scheduler.rawSpawn(A2(settings.onIce, ice, "someId")); 
            }, function(stream) {
                console.log("got stream", stream);
            }, function(pc, streamUrl) {
                pc.createOffer(function(off) {
                    console.log("creating offer", off);
                    var offer = new RTCSessionDescription(off);
                    pc.setLocalDescription(offer, function() {
                        cb($succeed({
                            sdp: off.sdp,
                            url: streamUrl 
                        }));
                    }, function(error) {
                        console.log("got error while setting local description", error);
                        cb($fail(error));
                    });
                }, function(error) {
                    console.log("got error while creating offer", error);
                    cb($fail(error));
                });
            }, function(err) {
                cb($fail(error));
            });
        });
    };
    
    function capabilities(settings) {
        return $nb(function(cb) {
            var mypc = new RTCPeerConnection();
            mypc.createOffer(function(sdp) {
                var desc = new RTCSessionDescription(sdp);
                cb($succeed({
                    sdp: desc.sdp
                }));
            }, function(error) {
                cb($fail(error));   
            },{
                offerToReceiveAudio: 1,
                offerToReceiveVideo: 1
            });
        });
    };

    return {
        capabilities: capabilities,
        call: call,
        hangup: hangup,
        answer: F3(answer),
        addSdp: addSdp,
        addIce: addIce
    }
}();
