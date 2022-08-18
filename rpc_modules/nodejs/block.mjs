class Block {
    constructor(inputBuffer) {
        this.inputBuffer = inputBuffer;
        this.buffer = new ArrayBuffer(this.getBlockLength());
        this.dataView = new DataView(this.buffer);

        this.dataView.setUint8(0, this.getType());
        this.fillData();
    }

    getBlockLength() {
        return 1 + 2 + this.inputBuffer.byteLength;
    }

    getType() {
        return Block.TYPE.BYTE_ARRAY;
    }

    fillData() {
        var inDataView = new DataView(this.inputBuffer);
        this.dataView.setUint16(1, this.inputBuffer.byteLength);
        for(var i = 3; i < this.getBlockLength(); i++) {
            var value = inDataView.getUint8(i - 3);
            this.dataView.setUint8(i, value);
        }
    }

    className() {
        return this.constructor.name;
    }

    length() {
        return this.dataView.getUint16(0);
    }

    content() {
        let content = [];
        for(var i = 0; i < this.length(); i++) {
            content.push(this.dataView.getUint8(i + Block.HEADER_SIZE));
        }
        return content;
    }
}

class StringBlock extends Block {
    constructor(inString) {
        var enc = new TextEncoder();
        super(enc.encode(inString).buffer)

    }
}

class Uint8Block extends Block {
    constructor(uint8) {
        var arrayBuffer = new ArrayBuffer(1);
        var dataView = new DataView(arrayBuffer);
        dataView.setUint8(0, uint8);
        super(arrayBuffer);
    }
}

class Uint16Block extends Block {
    constructor(uint16) {
        var arrayBuffer = new ArrayBuffer(2);
        var dataView = new DataView(arrayBuffer);
        dataView.setUint16(0, uint16);
        super(arrayBuffer);
    }
}

class Uint32Block extends Block {
    constructor(uint32) {
        var arrayBuffer = new ArrayBuffer(4);
        var dataView = new DataView(arrayBuffer);
        dataView.setUint32(0, uint32);
        super(arrayBuffer);
    }
}

class Float32Block extends Block {
    constructor(float32) {
        var arrayBuffer = new ArrayBuffer(4);
        var dataView = new DataView(arrayBuffer);
        dataView.setFloat32(0, float32)
        super(arrayBuffer);
    }
}

class Float64Block extends Block {
    constructor(float64) {
        var arrayBuffer = new ArrayBuffer(8);
        var dataView = new DataView(arrayBuffer);
        dataView.setFloat64(0, float64)
        super(arrayBuffer);
    }
}

Block.TYPE = {
    UINT8 : 0,
    UINT16: 1,
    UINT32: 2,
    FLOAT32: 3,
    FLOAT64: 4,
    STRING: 5,
    BYTE_ARRAY: 6
};

export { Block, StringBlock, Uint8Block, Uint16Block, Uint32Block, Float32Block, Float64Block }