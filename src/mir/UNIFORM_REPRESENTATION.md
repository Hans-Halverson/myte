OCaml-like uniform representation of values.

Values are exactly one word each, and can be either an integer or a pointer to a memory block.

# Integer Values

Integers are (word size - 1) bits, and have lowest bit set to 1. Ints, Bools, and Variants with no arguments are all encoded as integers.

- Int -> (word size - 1) bits
- Bool -> 0b0..001 and 0b0..011
- Variants with no arguments -> (word size - 1) bits for different variant tags

# Memory Blocks

Memory blocks start with a single word used as the header, followed by the block value.

Bits of header words are: `[54 bit size][2 bit gc flags][8 bit tag]`. The 54 bit size represents the number of words in the memory block. The two gc flag bits will be used by the garbage collector. The tag describes how to interpret the data in the memory value.

- Long -> `[size = 1][gc][tag = 255]` {word}
- Bytes -> `[size][gc][tag = 255]` {size words of binary data} where `size = (num bytes + (wordsize / 8)) / (wordsize / 8)`
- Closure -> `[size = 2][gc][tag = 255]` {funcptr, vtable}
- TraitObject -> `[size = 2][gc][tag = 254]` {thisptr, vtable}
- Tuple, single variant tuple/record -> `[size = num elements][gc][tag = 0]{size values}`
- Variant with arguments -> `[size = num arguments][gc][tag]{size values}` where tag has 254 possible values

Variants are layed out by separating into variants with and without arguments. There can be `2 ^ (word size - 1)` different variants without arguments, each represented as a single int. There can be 255 different variants with arguments, and each receives an int tag between 0 and 255.

During garbage collection do not check words for memory blocks with tag 255 (binary data), and only check the first word for memory blocks with tag 254 (trait object). For memory blocks with tags 0 through 253 check all words (of which there will be `size`).