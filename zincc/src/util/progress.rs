use std::fmt::{self, Display};

#[derive(Debug)]
pub struct Progress<M: Display> {
    pub message: M,
    done: usize,
    out_of: usize,
}

impl<M: Display> Display for Progress<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let percent = self.done as f64 / self.out_of as f64 * 100.0;
        write!(
            f,
            "{}{}{} [{}/{}] {:.2}%",
            ansi::CursorLeft,
            ansi::EraseLine,
            self.message,
            self.done,
            self.out_of,
            percent,
        )
    }
}

impl<M: Display> Progress<M> {
    pub fn new(message: M, out_of: usize) -> Self {
        Self {
            message,
            done: 0,
            out_of,
        }
    }

    pub fn inc(&mut self) {
        self.done += 1;
    }

    pub fn inc_out_of(&mut self, by: usize) {
        self.out_of += by;
    }
}

#[derive(Debug)]
pub struct NestedProgressMessage<M: Display> {
    vec: Vec<M>,
}

impl<M: Display> NestedProgressMessage<M> {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    pub fn new_with(vec: Vec<M>) -> Self {
        Self { vec }
    }

    pub fn push(&mut self, message: M) {
        self.vec.push(message);
    }

    pub fn pop(&mut self) {
        self.vec.pop();
    }
}

impl<M: Display> Default for NestedProgressMessage<M> {
    fn default() -> Self {
        Self::new()
    }
}

impl<M: Display> Display for NestedProgressMessage<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.vec.len() {
            0 => {}
            1 => write!(f, "{}", self.vec[0])?,
            _ => {
                write!(f, "{}", self.vec[0])?;
                for item in &self.vec[1..] {
                    write!(f, " / {}", item)?;
                }
            }
        }

        Ok(())
    }
}
