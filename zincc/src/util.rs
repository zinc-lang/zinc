use std::io::Write;

#[derive(Debug)]
pub struct Stopwatch {
    time: std::time::Instant,
}

impl Stopwatch {
    pub fn start() -> Stopwatch {
        Stopwatch {
            time: std::time::Instant::now(),
        }
    }

    pub fn read(&self) -> std::time::Duration {
        let now = std::time::Instant::now();
        now - self.time
    }

    pub fn reset(&mut self) {
        self.time = std::time::Instant::now();
    }

    pub fn lap(&mut self) -> std::time::Duration {
        let time = self.read();
        self.reset();
        time
    }
}

pub fn read_file_to_string<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<String> {
    use std::io::Read;
    let mut file = std::fs::File::open(path)?;
    let mut str = String::new();
    file.read_to_string(&mut str)?;
    Ok(str)
}

// @TODO: Allow to specify the indentation character
pub struct AutoIndentingWriter<'a, T: Write> {
    inner: &'a mut T,

    /// If true, the writer will not write any bytes to the inner writer.
    /// It will however will still track the indentation level.
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

pub mod index_vec {
    use std::fmt::Debug;
    use std::hash::Hash;
    use std::marker::PhantomData;

    pub trait Idx: 'static + Copy + Eq + Debug + Hash {
        fn new(idx: usize) -> Self;
        fn index(self) -> usize;
    }

    #[derive(Debug, Clone)]
    pub struct IndexVec<T, I: Idx> {
        pub raw: Vec<T>,
        _m: PhantomData<I>,
    }

    impl<T, I: Idx> Default for IndexVec<T, I> {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<T, I: Idx> IndexVec<T, I> {
        #[inline]
        pub fn new() -> Self {
            Self::from_raw(vec![])
        }

        #[inline]
        pub fn from_raw(raw: Vec<T>) -> Self {
            Self {
                raw,
                _m: PhantomData,
            }
        }

        #[inline]
        pub fn push(&mut self, t: T) -> I {
            let idx = I::new(self.raw.len());
            self.raw.push(t);
            idx
        }

        #[inline]
        pub fn get(&self, index: I) -> Option<&T> {
            self.raw.get(index.index())
        }
    }

    #[derive(Debug, Clone)]
    #[non_exhaustive]
    pub struct InterningIndexVec<T: Eq, I: Idx> {
        pub raw: IndexVec<T, I>,
    }

    impl<T: Eq, I: Idx> Default for InterningIndexVec<T, I> {
        fn default() -> Self {
            Self {
                raw: Default::default(),
            }
        }
    }

    impl<T: Eq, I: Idx> InterningIndexVec<T, I> {
        #[inline]
        pub fn new() -> Self {
            Self::default()
        }

        #[inline]
        pub fn from_raw(raw: IndexVec<T, I>) -> Self {
            Self { raw }
        }

        #[inline]
        pub fn get_or_intern(&mut self, t: T) -> I {
            self.raw
                .raw
                .iter()
                .position(|it| it == &t)
                .map(|i| I::new(i))
                .unwrap_or_else(|| self.intern(t))
        }

        #[inline]
        pub fn intern(&mut self, t: T) -> I {
            debug_assert!(self.raw.raw.iter().find(|&it| *it == t).is_none());
            self.raw.push(t)
        }

        #[inline]
        pub fn get(&self, index: I) -> Option<&T> {
            self.raw.get(index)
        }
    }
}
