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

    pub fn write(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        let align = self.map.iter().map(|(s, _)| s.len()).max().unwrap();

        let mut total = Duration::ZERO;
        for (name, duration) in self.map.iter() {
            writeln!(writer, "{name:>align$}: {duration:?}")?;
            total += *duration;
        }
        writeln!(writer, "\n{name:>align$}: {total:?}", name = "total")?;

        Ok(())
    }
}
