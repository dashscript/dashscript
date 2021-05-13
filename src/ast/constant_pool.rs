use dashscript_lexer::fsize;

#[derive(Debug, Clone)]
pub struct ConstantPool {
    pub strings: Vec<String>,
    pub numbers: Vec<fsize>
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self {
            strings: vec!["anonymous".to_string()],
            numbers: vec![]
        }
    }
}

impl ConstantPool {

    pub fn add_string(&mut self, constant: &String) -> u32 {
        (match self.strings.iter().position(|i| i == constant) {
            Some(id) => id,
            None => {
                self.strings.push(constant.to_owned());
                self.strings.len() - 1
            }
        }) as u32
    }

    pub fn add_number(&mut self, number: fsize) -> u32 {
        (match self.numbers.iter().position(|i| *i == number) {
            Some(id) => id,
            None => {
                self.numbers.push(number);
                self.numbers.len() - 1
            }
        }) as u32
    }

    pub fn get_string(&self, id: u32) -> String {
        match self.strings.get(id as usize) {
            Some(string) => string.clone(),
            None => String::new()
        }
    } 

}