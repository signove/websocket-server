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
            let size = 4;
            let offset = Math.pow(16, size - 1);
            return (((Math.random() * (Math.pow(16, size) - offset)) + offset)|0).toString(16);
        };
        let uuid = [
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
}
