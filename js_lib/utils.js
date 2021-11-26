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
    static stringToArrayBuffer(str) {
        var buf = new ArrayBuffer(str.length); // 2 bytes for each char
        var bufView = new Uint8Array(buf);
        for (var i=0, strLen=str.length; i < strLen; i++) {
          bufView[i] = str.charCodeAt(i);
        }
        return buf;
    }

}
