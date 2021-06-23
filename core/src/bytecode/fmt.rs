#[derive(Debug, Default, Clone)]
pub struct Formatter {
    bytes: Vec<u8>
}

impl Formatter {

    pub fn new(bytes: Vec<u8>) -> Self {
        Self { bytes }
    }

}