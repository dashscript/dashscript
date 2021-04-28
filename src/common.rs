pub fn get_line_col_by_body(body: String, start: usize) -> (usize, usize) {
    let lines: Vec<&str> = body.split("\n").collect();
    let mut i = 0;
    for line in 0..lines.len() {
        i += lines[line].len() + 1;
        if i > start {
            return (line+1, i - start);
        }
    }

    (lines.len(), 0)
}

pub fn get_line_col_by_line_data(data: Vec<usize>, start: usize) -> (usize, usize) {
    let mut i = 0;
    for line in 0..data.len() {
        i += data[line] + 1;
        if i > start {
            return (line+1, i - start);
        }
    }

    (data.len(), 0)
}

#[allow(non_camel_case_types)]
#[cfg(target_pointer_width = "64")]
pub type fsize = f64;

#[allow(non_camel_case_types)]
#[cfg(target_pointer_width = "64")]
pub const MAX_BYTES: usize = 8;

#[cfg(not(target_pointer_width = "64"))]
pub type fsize = f32;

#[cfg(not(target_pointer_width = "64"))]
pub const MAX_BYTES: usize = 4;