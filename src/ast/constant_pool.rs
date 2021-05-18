use dashscript_lexer::fsize;

#[derive(Debug, Clone)]
pub struct ConstantPool {
    pub strings: Vec<String>,
    pub ints: Vec<isize>,
    pub floats: Vec<fsize>
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self {
            strings: vec!["anonymous".to_string()],
            ints: Vec::new(),
            floats: Vec::new()
        }
    }
}

impl ConstantPool {

    pub fn add_string(&mut self, constant: &String) -> u32 {
        match self.strings.iter().position(|i| i == constant) {
            Some(id) => id as u32,
            None => {
                self.strings.push(constant.to_owned());
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

    pub fn add_float(&mut self, float: fsize) -> u32 {
        match self.floats.iter().position(|i| *i == float) {
            Some(id) => id as u32,
            None => {
                self.floats.push(float);
                self.floats.len() as u32 - 1
            }
        }
    }

    pub fn get_string(&self, id: u32) -> String {
        match self.strings.get(id as usize) {
            Some(string) => string.clone(),
            None => String::new()
        }
    } 

}