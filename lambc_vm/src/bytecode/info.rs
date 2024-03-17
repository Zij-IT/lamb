use crate::{
    gc::GcRef,
    value::{Function, Str, UnresolvedUpvalue},
};

#[derive(Debug, Default)]
pub(super) struct Block {
    pub(super) base: usize,
    pub(super) offset: usize,
    pub(super) depth: usize,
}

impl Block {
    fn new_for_func() -> Self {
        Self { base: 0, offset: 1, depth: 0 }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Local {
    pub(super) name: String,
    pub(super) depth: usize,
    pub(super) is_captured: bool,
}

impl Local {
    fn new(name: String, depth: usize) -> Self {
        Self { name, depth, is_captured: false }
    }

    fn mark_captured(&mut self) {
        self.is_captured = true;
    }
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub(super) blocks: Vec<Block>,
    pub(super) locals: Vec<Local>,
    pub(super) func: Function,
    pub(super) enclosing: Option<Box<Self>>,
}

impl FunctionInfo {
    pub(super) fn new(name: GcRef<Str>, module: GcRef<Str>) -> Self {
        Self {
            enclosing: None,
            func: Function::new(name, module),
            blocks: vec![Block::new_for_func()],
            // This local refers to the function that is currently being lowered.
            // By setting its depth to zero, we make sure it is unaccessible to
            // the user
            locals: vec![Local::new("".into(), 0)],
        }
    }

    pub(super) fn block(&self) -> &Block {
        self.blocks.last().unwrap()
    }

    pub(super) fn block_mut(&mut self) -> &mut Block {
        self.blocks.last_mut().unwrap()
    }

    pub(super) fn add_local(&mut self, name: String) {
        self.locals.push(Local::new(name, self.block().depth));
    }

    pub(super) fn local_slot(&self, name: &str) -> Option<usize> {
        let idx = self.local_idx(name)?;
        let depth = self.locals[idx].depth;

        // This looks backwards to find the block that the local with a depth
        // of `depth` is found. This local must be within one of the blocks,
        // and as such we must be able to find the associated block.
        let base = self
            .blocks
            .iter()
            .rev()
            .find(|b| b.depth == depth)
            .map(|b| b.base)
            .expect("The local must exist within a block of the same depth");

        // This finds the amount of locals that wee declared in the block
        // before the local `name`
        let predecessors = &self
            .locals
            .iter()
            .take(idx)
            .rev()
            .take_while(|l| l.depth == depth)
            .count();

        Some(base + predecessors)
    }

    fn local_idx(&self, name: &str) -> Option<usize> {
        self.locals.iter().rposition(|l| l.name == name)
    }

    pub(super) fn upvalue_idx(&mut self, name: &str) -> Option<usize> {
        let parent = self.enclosing.as_deref_mut()?;
        if let Some(idx) = parent.local_idx(name) {
            parent.locals[idx].mark_captured();
            return Some(self.add_upvalue(idx, true));
        }

        parent.upvalue_idx(name).map(|l| self.add_upvalue(l, false))
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        let pos = self
            .func
            .upvalues
            .iter()
            .position(|u| u.index == index && u.is_local == is_local);

        match pos {
            Some(p) => p,
            None => {
                self.func.upvalues.push(UnresolvedUpvalue { index, is_local });
                self.func.upvalues.len() - 1
            }
        }
    }
}
