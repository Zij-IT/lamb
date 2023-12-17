use crate::{
    gc::GcRef,
    value::{FuncUpvalue, LambFunc, LambString},
};

#[derive(Default)]
struct Block {
    pub enclosing: Option<Box<Block>>,
    pub base: usize,
    pub offset: usize,
    pub depth: usize,
}

impl Block {
    fn new(enclosing: Option<Box<Block>>) -> Self {
        Self {
            enclosing,
            base: 0,
            offset: 0,
            depth: 0,
        }
    }
}

struct Compiler {
    block: Block,
    locals: Vec<Local>,
    func: LambFunc,
    type_: CompilerType,
    enclosing: Option<Box<Compiler>>,
}

impl Compiler {
    pub fn for_script(name: GcRef<LambString>) -> Self {
        Self {
            enclosing: None,
            type_: CompilerType::Script,
            func: LambFunc::new(name),
            block: Block::new(None),
            locals: Vec::new(),
        }
    }

    pub fn for_func(name: GcRef<LambString>) -> Self {
        Self {
            enclosing: None,
            type_: CompilerType::Function,
            func: LambFunc::new(name),
            block: Block::new(None),
            locals: Vec::new(),
        }
    }

    pub fn new_enclosed(self, name: GcRef<LambString>) -> Self {
        let mut other = Self::for_func(name);
        other.enclosing = Some(Box::new(self));
        other
    }

    pub fn add_local(&mut self, name: String) {
        self.locals.push(Local::new(name, self.block.depth));
    }

    pub fn local_slot(&self, name: &str) -> Option<usize> {
        let idx = self.local_idx(name)?;
        let depth = self.locals[idx].depth;
        let mut base = None;

        let mut block = Some(&self.block);
        while let Some(b) = block {
            if b.depth == depth {
                base = Some(b.base);
                break;
            }

            block = b.enclosing.as_deref();
        }

        let base = base.expect("Block depths are strictly increasing by 1");
        let predecessors = self
            .locals
            .iter()
            .take(idx)
            .rev()
            .take_while(|l| l.depth == depth)
            .count();

        Some(base + predecessors)
    }

    fn local_idx(&self, name: &str) -> Option<usize> {
        self.locals.iter().rev().position(|l| l.name == name)
    }

    fn upvalue_idx(&mut self, name: &str) -> Option<usize> {
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
                self.func.upvalues.push(FuncUpvalue { index, is_local });
                self.func.upvalues.len() - 1
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CompilerType {
    Script,
    Function,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Local {
    name: String,
    depth: usize,
    is_captured: bool,
}

impl Local {
    pub fn new(name: String, depth: usize) -> Self {
        Self {
            name,
            depth,
            is_captured: false,
        }
    }

    pub fn mark_captured(&mut self) {
        self.is_captured = true;
    }
}
