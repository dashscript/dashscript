// This base64 converter is implemented from https://github.com/marshallpierce/rust-base64 crate.

use std::convert::TryInto;
use crate::{TinyString};

const INPUT_CHUNK_LEN: usize = 8;
const DECODED_CHUNK_LEN: usize = 6;
const DECODED_CHUNK_SUFFIX: usize = 2;
const PAD_BYTE: u8 = b'=';
const BLOCKS_PER_FAST_LOOP: usize = 4;
const LOW_SIX_BITS: u64 = 0x3F;
const LOW_SIX_BITS_U8: u8 = 0x3F;
const INVALID_VALUE: u8 = 255;
const CHUNKS_PER_FAST_LOOP_BLOCK: usize = 4;
const INPUT_BLOCK_LEN: usize = CHUNKS_PER_FAST_LOOP_BLOCK * INPUT_CHUNK_LEN;
const DECODED_BLOCK_LEN: usize = CHUNKS_PER_FAST_LOOP_BLOCK * DECODED_CHUNK_LEN + DECODED_CHUNK_SUFFIX;
const CHUNK_DECODE: [u8; 8] = [58, 52, 46, 40, 34, 28, 22, 16];

const ENCODE_TABLE: [u8; 64] = [
    65, 
    66, 
    67, 
    68, 
    69, 
    70, 
    71, 
    72, 
    73, 
    74, 
    75, 
    76, 
    77, 
    78, 
    79, 
    80, 
    81, 
    82, 
    83, 
    84, 
    85, 
    86, 
    87, 
    88, 
    89, 
    90, 
    97, 
    98, 
    99, 
    100, 
    101, 
    102, 
    103, 
    104, 
    105, 
    106, 
    107, 
    108, 
    109, 
    110, 
    111, 
    112, 
    113, 
    114, 
    115, 
    116, 
    117, 
    118, 
    119, 
    120, 
    121, 
    122, 
    48, 
    49, 
    50, 
    51, 
    52, 
    53, 
    54, 
    55, 
    56, 
    57, 
    43, 
    47, 
];

const DECODE_TABLE: [u8; 256] = [
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    62,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    63,
    52,
    53,
    54,
    55,
    56,
    57,
    58,
    59,
    60,
    61,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    0,
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    26,
    27,
    28,
    29,
    30,
    31,
    32,
    33,
    34,
    35,
    36,
    37,
    38,
    39,
    40,
    41,
    42,
    43,
    44,
    45,
    46,
    47,
    48,
    49,
    50,
    51,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
    INVALID_VALUE,
];

pub enum DecoderError {
    InvalidLength,
    InvalidByte(usize, u8),
    InvalidLastSymbol(usize, u8)
}

pub fn encode(bytes: &[u8]) -> Option<TinyString> {
    let mut buf = match get_encoded_size(bytes.len()) {
        Some(size) => vec![0; size],
        None => return None
    };

    encode_base64_buf(bytes, &mut buf);

    Some(TinyString::new(match std::str::from_utf8(&*buf) {
        Ok(str_) => str_.as_bytes(),
        Err(_) => return None
    }))
}

pub fn decode(bytes: &[u8]) -> Result<TinyString, DecoderError> {
    let mut buf = Vec::with_capacity(bytes.len() * 4 / 3);
    match decode_base64_buf(bytes, &mut buf) {
        None => Ok(TinyString::new(&*buf)),
        Some(error) => Err(error)
    }
}

fn read_u64(s: &[u8]) -> u64 {
    u64::from_be_bytes(s[..8].try_into().unwrap())
}

fn encode_base64_buf(bytes: &[u8], output: &mut [u8]) {
    let b_len = bytes.len();
    let last_fast_index = b_len.saturating_sub(BLOCKS_PER_FAST_LOOP * 6 + 2);
    let mut input_index: usize = 0;
    let mut bytes_written = 0;

    if last_fast_index > 0 {
        while input_index <= last_fast_index {
            let input_chunk = &bytes[input_index..(input_index + (BLOCKS_PER_FAST_LOOP * 6 + 2))];
            let output_chunk = &mut output[bytes_written..(bytes_written + BLOCKS_PER_FAST_LOOP * 8)];
            let input_u64 = read_u64(&input_chunk[0..]);

            output_chunk[0] = ENCODE_TABLE[((input_u64 >> 58) & LOW_SIX_BITS) as usize];
            output_chunk[1] = ENCODE_TABLE[((input_u64 >> 52) & LOW_SIX_BITS) as usize];
            output_chunk[2] = ENCODE_TABLE[((input_u64 >> 46) & LOW_SIX_BITS) as usize];
            output_chunk[3] = ENCODE_TABLE[((input_u64 >> 40) & LOW_SIX_BITS) as usize];
            output_chunk[4] = ENCODE_TABLE[((input_u64 >> 34) & LOW_SIX_BITS) as usize];
            output_chunk[5] = ENCODE_TABLE[((input_u64 >> 28) & LOW_SIX_BITS) as usize];
            output_chunk[6] = ENCODE_TABLE[((input_u64 >> 22) & LOW_SIX_BITS) as usize];
            output_chunk[7] = ENCODE_TABLE[((input_u64 >> 16) & LOW_SIX_BITS) as usize];

            let input_u64 = read_u64(&input_chunk[6..]);

            output_chunk[8] = ENCODE_TABLE[((input_u64 >> 58) & LOW_SIX_BITS) as usize];
            output_chunk[9] = ENCODE_TABLE[((input_u64 >> 52) & LOW_SIX_BITS) as usize];
            output_chunk[10] = ENCODE_TABLE[((input_u64 >> 46) & LOW_SIX_BITS) as usize];
            output_chunk[11] = ENCODE_TABLE[((input_u64 >> 40) & LOW_SIX_BITS) as usize];
            output_chunk[12] = ENCODE_TABLE[((input_u64 >> 34) & LOW_SIX_BITS) as usize];
            output_chunk[13] = ENCODE_TABLE[((input_u64 >> 28) & LOW_SIX_BITS) as usize];
            output_chunk[14] = ENCODE_TABLE[((input_u64 >> 22) & LOW_SIX_BITS) as usize];
            output_chunk[15] = ENCODE_TABLE[((input_u64 >> 16) & LOW_SIX_BITS) as usize];

            let input_u64 = read_u64(&input_chunk[12..]);

            output_chunk[16] = ENCODE_TABLE[((input_u64 >> 58) & LOW_SIX_BITS) as usize];
            output_chunk[17] = ENCODE_TABLE[((input_u64 >> 52) & LOW_SIX_BITS) as usize];
            output_chunk[18] = ENCODE_TABLE[((input_u64 >> 46) & LOW_SIX_BITS) as usize];
            output_chunk[19] = ENCODE_TABLE[((input_u64 >> 40) & LOW_SIX_BITS) as usize];
            output_chunk[20] = ENCODE_TABLE[((input_u64 >> 34) & LOW_SIX_BITS) as usize];
            output_chunk[21] = ENCODE_TABLE[((input_u64 >> 28) & LOW_SIX_BITS) as usize];
            output_chunk[22] = ENCODE_TABLE[((input_u64 >> 22) & LOW_SIX_BITS) as usize];
            output_chunk[23] = ENCODE_TABLE[((input_u64 >> 16) & LOW_SIX_BITS) as usize];

            let input_u64 = read_u64(&input_chunk[18..]);

            output_chunk[24] = ENCODE_TABLE[((input_u64 >> 58) & LOW_SIX_BITS) as usize];
            output_chunk[25] = ENCODE_TABLE[((input_u64 >> 52) & LOW_SIX_BITS) as usize];
            output_chunk[26] = ENCODE_TABLE[((input_u64 >> 46) & LOW_SIX_BITS) as usize];
            output_chunk[27] = ENCODE_TABLE[((input_u64 >> 40) & LOW_SIX_BITS) as usize];
            output_chunk[28] = ENCODE_TABLE[((input_u64 >> 34) & LOW_SIX_BITS) as usize];
            output_chunk[29] = ENCODE_TABLE[((input_u64 >> 28) & LOW_SIX_BITS) as usize];
            output_chunk[30] = ENCODE_TABLE[((input_u64 >> 22) & LOW_SIX_BITS) as usize];
            output_chunk[31] = ENCODE_TABLE[((input_u64 >> 16) & LOW_SIX_BITS) as usize];

            bytes_written += BLOCKS_PER_FAST_LOOP * 8;
            input_index += BLOCKS_PER_FAST_LOOP * 6;
        }
    }

    let rem = b_len % 3;
    let start_of_rem = b_len - rem;

    while input_index < start_of_rem {
        let input_chunk = &bytes[input_index..(input_index + 3)];
        let output_chunk = &mut output[bytes_written..(bytes_written + 4)];

        output_chunk[0] = ENCODE_TABLE[(input_chunk[0] >> 2) as usize];
        output_chunk[1] = ENCODE_TABLE[((input_chunk[0] << 4 | input_chunk[1] >> 4) & LOW_SIX_BITS_U8) as usize];
        output_chunk[2] = ENCODE_TABLE[((input_chunk[1] << 2 | input_chunk[2] >> 6) & LOW_SIX_BITS_U8) as usize];
        output_chunk[3] = ENCODE_TABLE[(input_chunk[2] & LOW_SIX_BITS_U8) as usize];

        input_index += 3;
        bytes_written += 4;
    }

    if rem == 2 {
        output[bytes_written] = ENCODE_TABLE[(bytes[start_of_rem] >> 2) as usize];
        output[bytes_written + 1] = ENCODE_TABLE[((bytes[start_of_rem] << 4 | bytes[start_of_rem + 1] >> 4) & LOW_SIX_BITS_U8) as usize];
        output[bytes_written + 2] = ENCODE_TABLE[((bytes[start_of_rem + 1] << 2) & LOW_SIX_BITS_U8) as usize];
        bytes_written += 3;
    } else if rem == 1 {
        output[bytes_written] = ENCODE_TABLE[(bytes[start_of_rem] >> 2) as usize];
        output[bytes_written + 1] = ENCODE_TABLE[((bytes[start_of_rem] << 4) & LOW_SIX_BITS_U8) as usize];
        bytes_written += 2;
    }

    assert_eq!(Some(output.len()), bytes_written.checked_add(add_buf_padding(bytes.len(), &mut output[bytes_written..])));
}

fn get_encoded_size(len: usize) -> Option<usize> {
    let rem = len % 3;
    let input_chunks = len / 3;
    let chunk_output = input_chunks.checked_mul(4);

    if rem > 0 {
        match chunk_output {
            Some(n) => n.checked_add(4),
            None => None
        }
    } else {
        chunk_output
    }
}

fn add_buf_padding(len: usize, output: &mut [u8]) -> usize {
    let rem = len % 3;
    let mut bytes_written = 0;

    for _ in 0..((3 - rem) % 3) {
        output[bytes_written] = PAD_BYTE;
        bytes_written += 1;
    }

    bytes_written
}

fn decode_base64_buf(bytes: &[u8], output: &mut Vec<u8>) -> Option<DecoderError> {
    let start_len = output.len();
    let nof_chunks = bytes.len()
        .checked_add(INPUT_CHUNK_LEN - 1)
        .expect("Failed converting base64 to string.") 
        / INPUT_CHUNK_LEN;

    let decoded_len_estimate = nof_chunks
        .checked_mul(DECODED_CHUNK_LEN)
        .and_then(|n| n.checked_add(start_len))
        .expect("Overflow when calculating output buffer length");

    output.resize(decoded_len_estimate, 0);

    let bytes_written = {
        let output = &mut output.as_mut_slice()[start_len..];
        let rem_len = bytes.len() % INPUT_CHUNK_LEN;
        let trailing_bytes_to_skip = match rem_len {
            0 => INPUT_CHUNK_LEN,
            1 | 5 => {
                if let Some(b) = bytes.last() {
                    if *b != PAD_BYTE && DECODE_TABLE[*b as usize] == INVALID_VALUE {
                        return Some(DecoderError::InvalidByte(bytes.len() - 1, *b));
                    }
                }
    
                return Some(DecoderError::InvalidLength);
            },
            2 | 3 | 4 => INPUT_CHUNK_LEN + rem_len,
            _ => rem_len,
        };

        let mut remaining_chunks = nof_chunks;
        let mut input_index = 0;
        let mut output_index = 0;

        {
            let length_of_fast_decode_chunks = bytes.len().saturating_sub(trailing_bytes_to_skip);
    
            if let Some(max_start_index) = length_of_fast_decode_chunks.checked_sub(INPUT_BLOCK_LEN) {
                while input_index <= max_start_index {
                    let input_slice = &bytes[input_index..(input_index + INPUT_BLOCK_LEN)];
                    let output_slice = &mut output[output_index..(output_index + DECODED_BLOCK_LEN)];

                    macro_rules! de_chunk {
                        ($($index:expr => $index2:expr)+) => {
                            $(if let Some(error) = decode_chunk(&input_slice[$index..], input_index + $index, &mut output_slice[$index2..]) {
                                return Some(error);
                            })+
                        };
                    }

                    de_chunk! {
                        0 => 0
                        8 => 6
                        16 => 12
                        24 => 18
                    };
    
                    input_index += INPUT_BLOCK_LEN;
                    output_index += DECODED_BLOCK_LEN - DECODED_CHUNK_SUFFIX;
                    remaining_chunks -= CHUNKS_PER_FAST_LOOP_BLOCK;
                }
            }
    
            if let Some(max_start_index) = length_of_fast_decode_chunks.checked_sub(INPUT_CHUNK_LEN) {
                while input_index < max_start_index {
                    decode_chunk(
                        &bytes[input_index..(input_index + INPUT_CHUNK_LEN)],
                        input_index,
                        &mut output[output_index..(output_index + DECODED_CHUNK_LEN + DECODED_CHUNK_SUFFIX)],
                    )?;
    
                    output_index += DECODED_CHUNK_LEN;
                    input_index += INPUT_CHUNK_LEN;
                    remaining_chunks -= 1;
                }
            }
        }

        for _ in 1..remaining_chunks {
            if let Some(error) = decode_chunk_precise(
                &bytes[input_index..],
                input_index,
                &mut output[output_index..(output_index + DECODED_CHUNK_LEN)],
            ) {
                return Some(error);
            }
    
            input_index += INPUT_CHUNK_LEN;
            output_index += DECODED_CHUNK_LEN;
        }
        
        debug_assert!(bytes.len() - input_index > 1 || bytes.is_empty());
        debug_assert!(bytes.len() - input_index <= 8);

        println!("{:?}", std::str::from_utf8(output));

        let mut leftover_bits = 0_u64;
        let mut morsels_in_leftover = 0;
        let mut padding_bytes = 0;
        let mut first_padding_index = 0_usize;
        let mut last_symbol = 0_u8;
        let start_of_leftovers = input_index;

        for (i, b) in bytes[start_of_leftovers..].iter().enumerate() {
            if *b == PAD_BYTE {
                if i % 4 < 2 {
                    let bad_padding_index = start_of_leftovers
                        + if padding_bytes > 0 {
                            first_padding_index
                        } else { i };

                    return Some(DecoderError::InvalidByte(bad_padding_index, *b));
                }
    
                if padding_bytes == 0 {
                    first_padding_index = i;
                }
    
                padding_bytes += 1;
                continue;
            }
    
            if padding_bytes > 0 {
                return Some(DecoderError::InvalidByte(start_of_leftovers + first_padding_index, PAD_BYTE));
            }

            last_symbol = *b;
    
            let shift = 64 - (morsels_in_leftover + 1) * 6;
            let morsel = DECODE_TABLE[*b as usize];
            if morsel == INVALID_VALUE {
                return Some(DecoderError::InvalidByte(start_of_leftovers + i, *b));
            }
    
            leftover_bits |= (morsel as u64) << shift;
            morsels_in_leftover += 1;
        }

        let leftover_bits_ready_to_append = match morsels_in_leftover {
            0 => 0,
            2 => 8,
            3 => 16,
            4 => 24,
            6 => 32,
            7 => 40,
            8 => 48,
            _ => unreachable!("Impossible: must only have 0 to 8 input bytes in last chunk, with no invalid lengths"),
        };

        let mask = !0 >> leftover_bits_ready_to_append;
        if (leftover_bits & mask) != 0 {
            return Some(DecoderError::InvalidLastSymbol(start_of_leftovers + morsels_in_leftover - 1, last_symbol))
        }

        let mut leftover_bits_appended_to_buf = 0;
        while leftover_bits_appended_to_buf < leftover_bits_ready_to_append {
            let selected_bits = (leftover_bits >> (56 - leftover_bits_appended_to_buf)) as u8;
            output[output_index] = selected_bits;
            output_index += 1;
            leftover_bits_appended_to_buf += 8;
        }

        output_index
    };

    output.truncate(start_len + bytes_written);
    None
}

fn decode_chunk(bytes: &[u8], start_of_input: usize, output: &mut [u8]) -> Option<DecoderError> {
    let morsel = DECODE_TABLE[bytes[0] as usize];
    if morsel == INVALID_VALUE {
        return Some(DecoderError::InvalidByte(start_of_input, bytes[0]));
    }

    let mut accum = (morsel as u64) << CHUNK_DECODE[0];
    
    for i in 1..8 {
        let morsel = DECODE_TABLE[bytes[i] as usize];
        if morsel == INVALID_VALUE {
            return Some(DecoderError::InvalidByte(start_of_input + i, bytes[i]));
        }

        accum |= (morsel as u64) << CHUNK_DECODE[i];
    }

    output[..8].copy_from_slice(&accum.to_le_bytes());

    None
}

fn decode_chunk_precise(bytes: &[u8], start_of_input: usize, output: &mut [u8]) -> Option<DecoderError> {
    let mut buf = [0_u8; 8];

    if let Some(error) = decode_chunk(bytes, start_of_input, &mut buf[..]) {
        return Some(error);
    }

    output[0..6].copy_from_slice(&buf[0..6]);
    None
}