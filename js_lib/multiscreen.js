/**
* @class MultiScreen
* @classdesc Class to access the realtime channel
*
*
* @example
*  //Real time and configuration messages arrive as events
* document.addEventListener('multiscreen.config.message'), function(e) {
*      var message = e.detail.message
* });
*
* document.addEventListener('multiscreen.message', function(e) {
*      var sender = e.detail.sender;
*      var message = e.detail.message;
* });
*/
class MultiScreen {

    /**
    * @constructs MultiScreen
    */
    constructor() {
        this.mSessionKey = undefined;
        this.mClientKey = undefined;
        this.mWebSocketRTObject = undefined;
        this.mWebSocketConfigObject = undefined;
    }

    /**
    * @method MultiScreen#connect
    * @desc It will establish a connection to both the Real Time channel and the Configuration channel.
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

        this.secretConfigKey = undefined;

        return new Promise((resolve, reject) => {
            this.mWebSocketRTObject.onopen = (evt) => {
                this.mSessionKey = sessionKey;
                this.mClientKey = clientKey;
            };
            this.mWebSocketRTObject.onmessage = (evt) => {
                if ( evt.data instanceof ArrayBuffer ) {
                    this.secretConfigKey = Utils.arrayBufferToString(evt.data);
                } else {
                    this.secretConfigKey = evt.data;
                }
                this.mWebSocketConfigObject = new WebSocket(webSocketURL + "/config?secret_config_key=" +encodeURIComponent(this.secretConfigKey)+ "&session=" + encodeURIComponent(this.mSessionKey) + "&client=" + encodeURIComponent(this.mClientKey));
                this.mWebSocketConfigObject.binaryType = "arraybuffer";

                this.mWebSocketConfigObject.onopen = (evt) => {
                    resolve(
                        {
                            sessionKey : this.mSessionKey,
                            myKey : this.mClientKey
                        }
                    );
                };

                this.mWebSocketConfigObject.onerror = (evt) => {
                    this.mWebSocketConfigObject = undefined;
                };

                this.mWebSocketConfigObject.onmessage = (evt) => {
                    var data = "";
                    if ( evt.data instanceof ArrayBuffer ) {
                        data = Utils.arrayBufferToString(evt.data);
                    } else {
                        data = evt.data;
                    }
                    document.dispatchEvent(new CustomEvent('multiscreen.config.message', { 'detail': { 'message' : data }  }));
                };

                this.mWebSocketRTObject.onmessage = (evt) => {
                    var data = "";
                    if ( evt.data instanceof ArrayBuffer ) {
                        data = Utils.arrayBufferToString(evt.data);
                    } else {
                        data = evt.data;
                    }
                    var sender = data.substring(0, data.indexOf('#'));
                    var message = data.substr(data.indexOf('#') + 1);
                    document.dispatchEvent(new CustomEvent('multiscreen.message', { 'detail': { 'sender' : sender, 'message' : message }  }));
                };
            };
            this.mWebSocketRTObject.onerror = (evt) => {
                this.mSessionKey = undefined;
                this.mClientKey = undefined;
                this.mWebSocketConfigObject = undefined;
                reject(evt);
            };
        });
    }

    /**
    * @method MultiScreen#disconnect
    * @desc Disconnect from both the Real Time channel and the Configuration channel.
    */
    disconnect() {
        if(this.mWebSocketRTObject) {
            this.mWebSocketRTObject.close();
        }
        if(this.mWebSocketConfigObject) {
            this.mWebSocketConfigObject.close();
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

        var promiseCloseConfig = new Promise((resolve, reject) => {
            if(!this.mWebSocketConfigObject) {
                resolve("Config session not opened");
            } else {
                this.mWebSocketConfigObject.onclose = (evt) => {
                    this.mWebSocketConfigObject = undefined;
                    resolve("Config session closed");
                };
            }
        });

        return Promise.all([promiseCloseRT, promiseCloseConfig]);
    }

    /**
    * @method MultiScreen#send
    * @desc Send a message through the Real Time channel.
    *
    * @param message - The message as string.
    */
    send(message, receiver = "all") {
        if(this.mWebSocketRTObject !== undefined) {
            var msgObject = {
                data : message,
                receiver : receiver
            };              
            this.mWebSocketRTObject.send(Utils.stringToArrayBuffer(JSON.stringify(msgObject)));
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

    /**
    * @method MultiScreen#sendConfigMessage
    * @desc Send a config message through the Configuration channel.
    *
    * @param message The message as string.
    */
    sendConfigMessage(message) {
        if(this.mWebSocketConfigObject !== undefined) {
            this.mWebSocketConfigObject.send(Utils.stringToArrayBuffer(message));
        }
        return new Promise((resolve, reject) => {
            if(this.mWebSocketConfigObject === undefined) {
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
