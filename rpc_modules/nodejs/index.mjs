import { StringBlock, Uint8Block, Uint16Block, Uint32Block, Float32Block, Float64Block } from './block.mjs';


for( var block of [
                new StringBlock("abcd")] ) {
    console.log(block.className());
    console.log(block.length());
    console.log(block.content());
    console.log("-------");
    
}