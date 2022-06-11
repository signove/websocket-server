/**
* @class MicroServiceHUB
* @classdesc Class to access the realtime channel
*
*
* @example
*  //Real time arrive as events
*
* document.addEventListener('microservicehub.message', function(e) {
*      var sender = e.detail.sender;
*      var message = e.detail.message;
* });
*/
class MicroServiceHUB {

    /**
    * @constructs MicroServiceHUB
    */
    constructor() {
        this.mSessionKey = undefined;
        this.mClientKey = undefined;
        this.mWebSocketRTObject = undefined;
    }

    /**
    * @method MicroServiceHUB#connect
    * @desc It will establish a connection to both the Real Time channel.
    *
    * @param webSocketURL The Web Socket URL to access the realtime channel
    * @param sessionKey The session identifier (Must be unique in the server)
    * @param clientKey The client identifier (Must be unique in the session)
    *
    * @return {Promise} A promise of the connection.
    */
    connect(webSocketURL, sessionKey, clientKey) {
        if(this.mWebSocketRTObject) {
            this.disconnect().then(() => {
                return this.connect(webSocketURL, sessionKey, clientKey);
            });
        } else {
            this.mWebSocketRTObject = new WebSocket(webSocketURL + "?session=" + encodeURIComponent(sessionKey) + "&client=" + encodeURIComponent(clientKey));
            this.mWebSocketRTObject.binaryType = "arraybuffer";
        }

        this.secretClientKey = undefined;

        return new Promise((resolve, reject) => {
            this.mWebSocketRTObject.onopen = (evt) => {
                this.mSessionKey = sessionKey;
                this.mClientKey = clientKey;
            };
            this.mWebSocketRTObject.onmessage = (evt) => {
                if ( evt.data instanceof ArrayBuffer ) {
                    var { version, sender, payload } = Utils.readMicroServiceHUBMessage(evt.data);
                    this.secretClientKey = Utils.arrayBufferToString(payload);
                } else {
                    this.secretClientKey = evt.data;
                }

                this.mWebSocketRTObject.onmessage = (evt) => {
                    var data = "";
                    if ( evt.data instanceof ArrayBuffer ) {
                        var { version, sender, payload } = Utils.readMicroServiceHUBMessage(evt.data);
                        data = payload;
                    } else {
                        data = evt.data;
                    }
                    document.dispatchEvent(new CustomEvent('microservicehub.message', { 'detail': { 'version' : version, 'sender' : sender, 'message' : payload }  }));
                };
            };
            this.mWebSocketRTObject.onerror = (evt) => {
                this.mSessionKey = undefined;
                this.mClientKey = undefined;
                reject(evt);
            };
        });
    }

    /**
    * @method MicroServiceHUB#disconnect
    * @desc Disconnect from both the Real Time channel.
    */
    disconnect() {
        if(this.mWebSocketRTObject) {
            this.mWebSocketRTObject.close();
        }

        var promiseCloseRT = new Promise((resolve, reject) => {
            if(!this.mWebSocketRTObject) {
                resolve("RT session not opened");
            } else {
                this.mWebSocketRTObject.onclose = (evt) => {
                    this.mWebSocketRTObject = undefined;
                    resolve("RT session closed");
                };
            }
        });

        return Promise.all([promiseCloseRT]);
    }

    /**
    * @method MicroServiceHUB#send
    * @desc Send a message through the Real Time channel.
    *
    * @param message - The message as string.
    */
    send(message, receiver = "") {
        if(this.mWebSocketRTObject !== undefined) {
            this.mWebSocketRTObject.send(Utils.createMicroServiceHUBMessageV2(0, message, receiver));
        }
        return new Promise((resolve, reject) => {
            if(this.mWebSocketRTObject === undefined) {
                reject(
                    {
                        message : "It is not connected!"
                    }
                );
            } else {
                resolve();
            }
        });
    }
}
