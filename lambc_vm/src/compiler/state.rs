use crate::gc::LambGc;

pub struct State<'gc> {
    pub gc: &'gc mut LambGc,
    has_errors: bool,
}

impl<'gc> std::fmt::Debug for State<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("State").finish_non_exhaustive()
    }
}

impl<'gc> State<'gc> {
    pub fn new(gc: &'gc mut LambGc) -> Self {
        Self {
            gc,
            has_errors: false,
        }
    }

    pub fn add_error<T>(&mut self, err: T) {
        // TODO: Find a way to add any diagnostics to this state
        self.has_errors = true;
        _ = err;
    }

    pub fn has_errors(&self) -> bool {
        self.has_errors
    }
}
