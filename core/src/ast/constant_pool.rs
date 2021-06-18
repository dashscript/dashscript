use crate::TinyString;

#[derive(Debug, Clone)]
pub struct ConstantPool {
    pub strings: Vec<TinyString>,
    pub ints: Vec<isize>,
    pub floats: Vec<f64>
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self {
            strings: vec![TinyString::new(b"anonymous"), TinyString::new(b"error"), TinyString::new(b"init")],
            ints: Vec::new(),
            floats: Vec::new()
        }
    }
}

impl ConstantPool {

    pub fn add_string(&mut self, constant: TinyString) -> u32 {
        let bytes = constant.to_bytes();
        match self.strings.iter().position(|i| i.to_bytes() == bytes) {
            Some(id) => id as u32,
            None => {
                self.strings.push(constant);
                self.strings.len() as u32 - 1
            }
        }
    }

    pub fn add_int(&mut self, int: isize) -> u32 {
        match self.ints.iter().position(|i| *i == int) {
            Some(id) => id as u32,
            None => {
                self.ints.push(int);
                self.ints.len() as u32 - 1
            }
        }
    }

    pub fn add_float(&mut self, float: f64) -> u32 {
        match self.floats.iter().position(|i| *i == float) {
            Some(id) => id as u32,
            None => {
                self.floats.push(float);
                self.floats.len() as u32 - 1
            }
        }
    }

    pub fn get_string(&self, id: u32) -> TinyString {
        match self.strings.get(id as usize) {
            // A clone of tiny string gets created to not deallocate
            // the original one.
            Some(string) => string.clone(), 
            // Just a temporary default with no bytes.
            None => TinyString::default()
        }
    }  

}