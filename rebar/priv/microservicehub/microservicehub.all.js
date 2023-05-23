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
                    var { command, sender, payload } = Utils.readMicroServiceHUBMessage(evt.data);
                    this.secretClientKey = Utils.arrayBufferToString(payload);
                } else {
                    this.secretClientKey = evt.data;
                }

                this.mWebSocketRTObject.onmessage = (evt) => {
                    var data = "";
                    if ( evt.data instanceof ArrayBuffer ) {
                        var { command, sender, payload } = Utils.readMicroServiceHUBMessage(evt.data);
                        data = payload;
                    } else {
                        data = evt.data;
                    }
                    document.dispatchEvent(new CustomEvent('microservicehub.message', { 'detail': { 'command' : command, 'sender' : sender, 'message' : payload }  }));
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
    * @param message - The message as byte array.
    */
    send(message) {
        if(this.mWebSocketRTObject !== undefined) {
            this.mWebSocketRTObject.send(message);
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

/**
* @class Utils
* @classdesc Utility class.
*/
class Utils {

    /**
    * @constructs Utils
    */
    constructor() {
    }

    /**
     * Generate a random UUID to be used as session key.
     */
    static uuid() {
        function S4() {
            var size = 4;
            var offset = Math.pow(16, size - 1);
            return (((Math.random() * (Math.pow(16, size) - offset)) + offset)|0).toString(16);
        };
        var uuid = [
            S4() + S4(),
            S4(),
            S4(),
            S4(),
            S4() + S4() + S4(),
        ];
        return uuid.join('-');
    }

    /**
     * Execute the given Javascript code.
     * @param {string} code - Javascript code to be executed.
     */
    static execJavascript(code) {
        eval(code);
    }

    /**
     * Convert ArrayBuffer to string
     * @param {*} buf 
     * @returns The array as string
     */
    static arrayBufferToString(buf) {
        return String.fromCharCode.apply(null, new Uint8Array(buf));
    }

    /**
     * Convert string to ArrayBuffer
     * @param {*} str 
     * @returns The string as array buffer
     */
    static stringToArrayBuffer(str, bufferLen = str.length) {
        var buf = new ArrayBuffer(bufferLen);
        var bufView = new Uint8Array(buf);
        for (var i=0, strLen=str.length; i < strLen; i++) {
          bufView[i] = str.charCodeAt(i);
        }
        return buf;
    }

    /**
     * Read a MicroService HUB message
     * @param {*} buffer 
     * @returns An object with sender and payload of the message
     */
    static readMicroServiceHUBMessage(buffer) {
        const VERSION_LENGTH = 2;
        const RECEIVER_LENGTH = 20;
        const PAYLOAD_POS_OFFSET = 20;
        var senderArray = [];
        var sender = undefined;
        var payload = undefined;
        var senderView = undefined;

        var versionView = new DataView(buffer, 0, VERSION_LENGTH);
        var version = versionView.getUint16(0);

        if(version == 1) {
            senderView = new DataView(buffer, VERSION_LENGTH, RECEIVER_LENGTH);
            for (var i=0; i < senderView.byteLength && senderView.getUint8(i) != 0; i++) {
                senderArray[i] = String.fromCharCode(senderView.getUint8(i));
            }
            sender = senderArray.join('');
            payload = buffer.slice(PAYLOAD_POS_OFFSET + VERSION_LENGTH);
            return { sender, payload };
        } else if(version == 2) {
            var commandView = new DataView(buffer, VERSION_LENGTH, 1);
            senderView = new DataView(buffer, VERSION_LENGTH + 1, RECEIVER_LENGTH);
            var command = commandView.getUint8(0);
            for (var i=0; i < senderView.byteLength && senderView.getUint8(i) != 0; i++) {
                senderArray[i] = String.fromCharCode(senderView.getUint8(i));
            }
            sender = senderArray.join('');
            payload = buffer.slice(PAYLOAD_POS_OFFSET + VERSION_LENGTH + 1);
            return { command, sender, payload };
        } else {
            return { sender, payload };
        }
    }

    /**
     * Creates a version 1 MicroService HUB message
     * @param {*} payload 
     * @param {*} receiver 
     * @returns Returns a message as array buffer
     */
    static createMicroServiceHUBMessageV1(payload, receiver = "") {
        const VERSION = 1;
        const VERSION_LENGTH = 2
        const RECEIVER_LENGTH = 20;
        const RECEIVER_POS_OFFSET = 2;
        
        receiver = receiver.substring(0, RECEIVER_LENGTH);

        var buf = new ArrayBuffer(VERSION_LENGTH + RECEIVER_LENGTH + payload.byteLength);
        var dataview = new DataView(buf);
        dataview.setUint16(0, VERSION);
        var receiverView = new DataView(buf, RECEIVER_POS_OFFSET, RECEIVER_LENGTH);
        for (var i=0; i < (receiver.length); i++) {
            receiverView.setUint8(i, receiver.charCodeAt(i));
        }
  	
  		var sourcePayloadView = new DataView(payload);        
        var targetPayloadView = new DataView(buf, VERSION_LENGTH + RECEIVER_LENGTH);
        for (var i=0; i < (sourcePayloadView.byteLength); i++) {
            targetPayloadView.setUint8(i, sourcePayloadView.getUint8(i));
        }
        return buf;
    }

    /**
     * Creates a version 2 MicroService HUB message
     * @param {*} payload 
     * @param {*} receiver 
     * @returns Returns a message as array buffer
     */
    static createMicroServiceHUBMessageV2(command, payload, receiver = "") {
        const VERSION = 2;
        const VERSION_LENGTH = 2
        const RECEIVER_LENGTH = 20;
        const RECEIVER_POS_OFFSET = 2;
        const COMMAND_LENGTH = 1;

        receiver = receiver.substring(0, RECEIVER_LENGTH);

        var buf = new ArrayBuffer(VERSION_LENGTH + RECEIVER_LENGTH + COMMAND_LENGTH + payload.byteLength);
        var dataview = new DataView(buf);
        dataview.setUint16(0, VERSION);
        var receiverView = new DataView(buf, RECEIVER_POS_OFFSET, RECEIVER_LENGTH);
        for (var i=0; i < (receiver.length); i++) {
            receiverView.setUint8(i, receiver.charCodeAt(i));
        }
        
        var commandview = new DataView(buf, VERSION_LENGTH + RECEIVER_LENGTH, 1);
        commandview.setUint8(0, command);

        var sourcePayloadView = new DataView(payload);        
        var targetPayloadView = new DataView(buf, VERSION_LENGTH + RECEIVER_LENGTH + COMMAND_LENGTH);
        for (var i=0; i < (sourcePayloadView.byteLength); i++) {
            targetPayloadView.setUint8(i, sourcePayloadView.getUint8(i));
        }
        return buf;
    }

}
