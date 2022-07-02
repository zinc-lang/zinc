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

    pub fn write(&self, padding: &str, writer: &mut impl std::io::Write) -> std::io::Result<()> {
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
