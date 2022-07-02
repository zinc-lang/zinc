use std::io::Write;

pub struct AutoIndentingWriter<'a, T: Write> {
    inner: &'a mut T,

    /// If true, the writer will not write any bytes to the inner writer.
    /// It will however still track the indentation level.
    pub disable_write: bool,

    indent_delta: usize,
    indent_count: usize,
    current_line_empty: bool,

    /// Automatically popped when applied
    indent_one_shot_count: usize,

    /// The most recently applied indent
    applied_indent: usize,

    /// Not used until the next line
    indent_next_line: usize,
}

impl<'a, T: Write> AutoIndentingWriter<'a, T> {
    pub fn new(writer: &'a mut T, indent_delta: usize) -> Self {
        Self {
            inner: writer,
            disable_write: false,
            indent_delta,
            indent_count: 0,
            current_line_empty: true,
            indent_one_shot_count: 0,
            applied_indent: 0,
            indent_next_line: 0,
        }
    }

    pub fn current_line_empty(self) -> bool {
        self.current_line_empty
    }

    pub fn applied_indent(&mut self) -> usize {
        self.applied_indent
    }

    pub fn set_indent_delta(&mut self, new_delta: usize) {
        if self.indent_count == new_delta {
            return;
        } else if self.indent_delta > new_delta {
            assert_eq!(self.indent_delta % new_delta, 0);
            self.indent_count *= self.indent_delta / new_delta;
        } else {
            assert_eq!((self.indent_count * self.indent_delta) % new_delta, 0);
            self.indent_count /= new_delta / self.indent_delta;
        }
        self.indent_delta = new_delta;
    }

    pub fn write_no_indent(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let len = buf.len();
        if len == 0 {
            Ok(0)
        } else {
            if !self.disable_write {
                self.inner.write_all(buf)?;
            }
            if buf[len - 1] == b'\n' {
                self.reset_line();
            }
            Ok(len)
        }
    }

    fn reset_line(&mut self) {
        self.current_line_empty = true;
        self.indent_next_line = 0;
    }

    fn apply_indent(&mut self) -> std::io::Result<()> {
        let current_indent = self.get_current_indent();
        if self.current_line_empty && current_indent > 0 && !self.disable_write {
            for _ in 0..current_indent {
                self.inner.write_all(" ".as_bytes())?;
            }
        }

        self.indent_count -= self.indent_one_shot_count;
        self.indent_one_shot_count = 0;
        self.current_line_empty = false;
        Ok(())
    }

    fn get_current_indent(&mut self) -> usize {
        let mut indent_current = self.indent_count;
        if indent_current > 0 {
            let indent_count = self.indent_count - self.indent_next_line;
            indent_current = indent_count * self.indent_delta;
        }
        indent_current
    }

    pub fn insert_newline(&mut self) -> std::io::Result<()> {
        self.write_no_indent("\n".as_bytes()).map(|_| ())
    }

    /// Insert a newline if line is not empty
    pub fn maybe_insert_newline(&mut self) -> std::io::Result<()> {
        if !self.current_line_empty {
            self.insert_newline()
        } else {
            Ok(())
        }
    }

    /// Increment the level of indent.
    /// Does not write anything itself but tells the writer
    /// what to do when we do write again.
    pub fn push_indent(&mut self) {
        self.indent_count += 1;
    }

    /// Push an indent that is popped after being applied
    pub fn push_indent_one_shot(&mut self) {
        self.indent_one_shot_count += 1;
        self.push_indent();
    }

    /// Turns all one-shot indent into regular indents.
    /// Returns the number of indent that must now be manually popped.
    pub fn lock_one_shot_indent(&mut self) -> usize {
        let locked_count = self.indent_one_shot_count;
        self.indent_one_shot_count = 0;
        locked_count
    }

    /// Push an indent that will only be applied on the next line
    pub fn push_indent_next_line(&mut self) {
        self.indent_next_line += 1;
        self.push_indent();
    }

    pub fn pop_indent(&mut self) {
        assert_ne!(self.indent_count, 0);
        self.indent_count -= 1;

        if self.indent_next_line > 0 {
            self.indent_next_line -= 1;
        }
    }

    /// Checks if the most recent indentation exceeds the currently pushed indents
    pub fn is_line_over_indented(&mut self) -> bool {
        if self.current_line_empty {
            false
        } else {
            self.applied_indent > self.get_current_indent()
        }
    }
}

impl<T: Write> Write for AutoIndentingWriter<'_, T> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if buf.is_empty() {
            Ok(0)
        } else {
            self.apply_indent()?;
            self.write_no_indent(buf)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}
