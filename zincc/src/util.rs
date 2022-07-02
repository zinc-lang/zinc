use std::io::Write;

pub fn read_file_to_string<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<String> {
    use std::io::Read;
    let mut file = std::fs::File::open(path)?;
    let mut str = String::new();
    let len = file.read_to_string(&mut str)?;
    assert_eq!(len, str.len());
    Ok(str)
}

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

pub mod time {
    use std::time::{Duration, Instant};

    #[derive(Debug)]
    pub struct Stopwatch {
        time: Instant,
    }

    impl Default for Stopwatch {
        fn default() -> Self {
            Self::new()
        }
    }

    impl Stopwatch {
        #[inline]
        pub fn new() -> Self {
            Self::start()
        }

        #[inline]
        pub fn start() -> Self {
            Self {
                time: Instant::now(),
            }
        }

        #[inline(always)]
        pub fn read(&self) -> Duration {
            let now = Instant::now();
            now - self.time
        }

        #[inline(always)]
        pub fn reset(&mut self) {
            self.time = Instant::now();
        }

        #[inline(always)]
        pub fn lap(&mut self) -> Duration {
            let time = self.read();
            self.reset();
            time
        }

        #[inline(always)]
        pub fn spanned<T>(&mut self, f: impl FnOnce() -> T) -> (T, Duration) {
            self.reset();
            let result = f();
            (result, self.read())
        }
    }

    #[derive(Debug, Default)]
    pub struct Timer {
        map: Vec<(&'static str, Duration)>,
        stopwatch: Stopwatch,
    }

    impl Timer {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn spanned<T>(&mut self, name: &'static str, f: impl FnOnce() -> T) -> T {
            let (res, duration) = self.stopwatch.spanned(f);
            self.map.push((name, duration));
            res
        }

        pub fn write(
            &self,
            padding: &str,
            writer: &mut impl std::io::Write,
        ) -> std::io::Result<()> {
            let total = self.map.iter().map(|(_, dur)| dur).sum::<Duration>();

            let mut map = self.map.clone();
            map.push(("total", total));

            let align = map.iter().map(|(str, _)| str.len()).max().unwrap();

            for (name, duration) in map.iter() {
                writeln!(writer, "{padding}{name:>align$}: {duration:?}")?;
            }

            Ok(())
        }
    }
}

pub mod index {
    use std::fmt::{self, Debug};
    use std::hash::Hash;
    use std::marker::PhantomData;

    macro_rules! define_idx {
        ($name:ident, $inner_ty:ty, $blk_new:expr, $blk_get:expr) => {
            #[derive(Clone, Copy, PartialEq, Eq, Hash)]
            pub struct $name($inner_ty);

            impl std::fmt::Debug for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(
                        f,
                        "{}({})",
                        stringify!($name),
                        crate::util::index::Idx::index(*self)
                    )
                }
            }

            impl crate::util::index::Idx for $name {
                fn new(idx: usize) -> Self {
                    Self($blk_new(idx))
                }

                fn index(self) -> usize {
                    $blk_get(self)
                }
            }
        };
    }
    pub(crate) use define_idx;

    // macro_rules! define_usize_idx {
    //     ($name:ident) => {
    //         $crate::util::index::define_idx!($name, usize, |x| { x }, |s: $name| { s.0 });
    //     };
    // }
    // pub(crate) use define_usize_idx;

    // macro_rules! define_non_zero_usize_idx {
    //     ($name:ident) => {
    //         $crate::util::index::define_idx!(
    //             $name,
    //             std::num::NonZeroUsize,
    //             |x| { std::num::NonZeroUsize::new(x + 1).unwrap() },
    //             |s: $name| { s.0.get() - 1 }
    //         );
    //     };
    // }
    // pub(crate) use define_non_zero_usize_idx;

    macro_rules! define_u32_idx {
        ($name:ident) => {
            $crate::util::index::define_idx!(
                $name,
                u32,
                |x: usize| { x.try_into().unwrap() },
                |s: $name| { s.0 as usize }
            );
        };
    }
    pub(crate) use define_u32_idx;

    macro_rules! define_non_zero_u32_idx {
        ($name:ident) => {
            $crate::util::index::define_idx!(
                $name,
                std::num::NonZeroU32,
                |x: usize| { std::num::NonZeroU32::new(x as u32 + 1).unwrap() },
                |s: $name| { s.0.get() as usize - 1 }
            );
        };
    }
    pub(crate) use define_non_zero_u32_idx;

    pub trait Idx: 'static + Copy + Eq + Hash {
        fn new(idx: usize) -> Self;
        fn index(self) -> usize;
    }

    #[derive(Clone)]
    pub struct IndexVec<T, I: Idx> {
        pub raw: Vec<T>,
        _m: PhantomData<I>,
    }

    impl<T: Debug, I: Idx> fmt::Debug for IndexVec<T, I> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_map()
                .entries(self.raw.iter().enumerate().map(|(k, v)| (k, v)))
                .finish()
        }
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

        /// # Safety
        /// UB if it is not a valid index.
        #[inline]
        pub unsafe fn get_unchecked(&self, index: I) -> &T {
            self.get(index).unwrap_unchecked()
        }

        #[inline]
        pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
            self.raw.get_mut(index.index())
        }

        #[inline]
        pub fn indices(&self) -> Vec<I> {
            self.raw
                .iter()
                .enumerate()
                .map(|(idx, _)| I::new(idx))
                .collect()
        }
    }

    #[derive(Clone)]
    #[non_exhaustive]
    pub struct InterningIndexVec<T: Eq, I: Idx> {
        raw: IndexVec<T, I>,
    }

    impl<T: Eq + Debug, I: Idx> fmt::Debug for InterningIndexVec<T, I> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(&self.raw, f)
        }
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

        pub fn raw(&self) -> &IndexVec<T, I> {
            &self.raw
        }

        #[inline]
        pub fn from_raw(raw: IndexVec<T, I>) -> Self {
            Self { raw }
        }

        #[inline]
        pub fn is_interned(&mut self, t: &T) -> bool {
            self.raw.raw.iter().any(|it| it == t)
        }

        #[inline]
        pub fn get_from_value(&self, t: &T) -> Option<I> {
            self.raw
                .raw
                .iter()
                .position(|it| it == t)
                .map(|i| I::new(i))
        }

        /// # Safety
        /// UB if it has not been interned.
        #[inline]
        pub unsafe fn get_from_value_unchecked(&self, t: &T) -> I {
            self.get_from_value(t).unwrap_unchecked()
        }

        #[inline]
        pub fn get_or_intern(&mut self, t: T) -> I {
            self.get_from_value(&t).unwrap_or_else(|| self.intern(t))
        }

        #[inline]
        pub fn intern(&mut self, t: T) -> I {
            // Make sure that is not already in the vec
            debug_assert!(!self.raw.raw.iter().any(|it| it == &t));
            self.raw.push(t)
        }

        #[inline]
        pub fn get(&self, index: I) -> Option<&T> {
            self.raw.get(index)
        }
    }

    define_non_zero_u32_idx!(StringSymbol);

    /// Small type specialization to allow checking if a string is interned from just a `&str`
    /// without having to first turn it into a `String`
    pub type StringInterningVec = InterningIndexVec<String, StringSymbol>;

    impl StringInterningVec {
        pub fn is_str_interned(&self, str: &str) -> bool {
            self.raw.raw.iter().any(|it| it == str)
        }

        pub fn get_from_str_value(&self, str: &str) -> Option<StringSymbol> {
            self.raw
                .raw
                .iter()
                .position(|it| it == str)
                .map(StringSymbol::new)
        }

        /// # Safety
        /// Same as [`InterningVec`]'s `get_from_value_unchecked`
        ///
        /// [`InterningVec`]: ./struct.InterningVec.html
        pub unsafe fn get_from_str_value_unchecked(&self, str: &str) -> StringSymbol {
            self.get_from_str_value(str).unwrap_unchecked()
        }
    }
}
