use cpu::{Mmu, State, Result};
use Address;

pub trait AddressCallBack {
    fn is_mine(&self, address: Address) -> bool;
    fn exec<M: Mmu>(&self, address: Address, state: &State, mmu: &M) -> Result<()>;
}

pub trait CallHook {
    fn handle<M: Mmu>(&self, address: Address, state: &State, mmu: &M) -> Option<Result<()>>;
}

impl<A: AddressCallBack> CallHook for A {
    fn handle<M: Mmu>(&self, address: u16, state: &State, mmu: &M) -> Option<Result<()>> {
        match self.is_mine(address) {
            true => Some(self.exec(address, state, mmu)),
            false => None,
        }
    }
}

impl<A: CallHook, B: CallHook> CallHook for (A, B) {
    fn handle<M: Mmu>(&self, address: Address, state: &State, mmu: &M) -> Option<Result<()>> {
        self.0.handle(address, state, mmu).or_else(|| self.1.handle(address, state, mmu))
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub struct NoneHook;

impl CallHook for NoneHook {
    fn handle<M: Mmu>(&self, _address: u16, _state: &State, _mmu: &M) -> Option<Result<()>> {
        None
    }
}
