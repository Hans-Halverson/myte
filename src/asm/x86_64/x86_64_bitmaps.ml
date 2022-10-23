open Asm
open X86_64_asm

(* A bitmap indicating whether pointer-sized words in the data section are pointers. The data
   section is already arranged so that all data items with a pointer-sized alignment are grouped
   together, so the bitmap only needs to cover this section of the data section. *)
type data_section_bitmap = {
  (* Total number of pointer-sized words in the data section covered by the bitmap *)
  num_words: int;
  bitmap: Bytes.t;
}

let round_up_to_alignment size alignment =
  let overflow = size mod alignment in
  if overflow = 0 then
    size
  else
    size + (alignment - overflow)

module BitmapWriter = struct
  type t = {
    bitmap: Bytes.t;
    mutable next_bit_index: int;
  }

  let mk num_bytes = { bitmap = Bytes.make num_bytes '\x00'; next_bit_index = 0 }

  let finish bw = bw.bitmap

  (* Append a single bit to the bitmap *)
  let append_bit bw is_set =
    (* Calculate byte and bit index for next bit *)
    let byte_index = bw.next_bit_index / 8 in
    let bit_index = bw.next_bit_index mod 8 in
    bw.next_bit_index <- bw.next_bit_index + 1;

    (* Calculate mask to add bit to current byte *)
    let bit =
      if is_set then
        1
      else
        0
    in
    let byte_mask = Int.shift_left bit bit_index in

    (* Apply mask to current byte in bitmap *)
    let current_byte = Char.code (Bytes.get bw.bitmap byte_index) in
    let new_byte = Int.logor byte_mask current_byte in
    Bytes.set bw.bitmap byte_index (Char.chr new_byte)

  (* Append a run of `length` of the same bit to bitmap *)
  let rec append_bit_run bw is_set length =
    if length > 0 then (
      append_bit bw is_set;
      append_bit_run bw is_set (length - 1)
    )
end

let gen_data_section_bitmap (data_section : 'a data_section) =
  (* Find total number of pointer-sized words in portion of data section that may contain pointers *)
  let maybe_pointer_items = data_section.(3) in
  let size = List.fold_left (fun size item -> size + item.size) 0 maybe_pointer_items in
  let num_words = size / 8 in

  (* Bitmap is composed of full bytes, so round up number of words to multiple of 8 *)
  let num_bytes_in_bitmap = round_up_to_alignment num_words 8 / 8 in

  (* Construct bitmap from data section *)
  let writer = BitmapWriter.mk num_bytes_in_bitmap in
  List.iter
    (fun item ->
      let num_words_in_item = item.size / pointer_size in
      BitmapWriter.append_bit_run writer item.is_pointer num_words_in_item)
    maybe_pointer_items;

  { num_words; bitmap = BitmapWriter.finish writer }
