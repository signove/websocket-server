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
     * @returns 
     */
    static arrayBufferToString(buf) {
        return String.fromCharCode.apply(null, new Uint8Array(buf));
    }

    /**
     * Convert string to ArrayBuffer
     * @param {*} str 
     * @returns 
     */
    static stringToArrayBuffer(str, bufferLen = str.length) {
        var buf = new ArrayBuffer(bufferLen);
        var bufView = new Uint8Array(buf);
        for (var i=0, strLen=str.length; i < strLen; i++) {
          bufView[i] = str.charCodeAt(i);
        }
        return buf;
    } 

    static readMultiscreenMessage(buffer) {
        const RECEIVER_LENGTH = 20;
        const RECEIVER_POS_OFFSET = 2;
        const PAYLOAD_POS_OFFSET = 22;
        var senderArray = [];
        var version = undefined;

        var dataview = new DataView(buffer);
        version = dataview.getUint16(0);
  		var senderView = new DataView(buffer, RECEIVER_POS_OFFSET, RECEIVER_LENGTH);
        for (var i=0; i < senderView.byteLength && senderView.getUint8(i) != 0; i++) {
            senderArray[i] = String.fromCharCode(senderView.getUint8(i));
        }
  		var sender = senderArray.join('');
        var payload = buffer.slice(PAYLOAD_POS_OFFSET);
        
        return { version, sender, payload };
    }

    static createMultiscreenMessage(payload, receiver = "") {
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

}
