use cpu::{Mmu, State, Result};
use Address;
use std::ops::Deref;
use std::rc::Rc;

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

impl <C: CallHook> CallHook for Rc<C> {
    fn handle<M: Mmu>(&self, address: u16, state: &State, mmu: &M) -> Option<Result<()>> {
        self.deref().handle(address, state, mmu)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use cpu::Cpu as GenCpu;
    use cpu::PlainMemory;
    use io_bus::VoidIO;
    use std::cell::RefCell;
    use asm::Instruction;
    use std::rc::Rc;

    type Cpu<H> = GenCpu<PlainMemory, VoidIO, VoidIO, H>;
    #[derive(Default)]
    struct CallHookMock { data: RefCell<(Address, State)> }

    #[derive(Default)]
    struct AddressMock { accept: Address, called: RefCell<bool> }

    impl AddressCallBack for AddressMock {
        fn is_mine(&self, address: Address) -> bool {
            address == self.accept
        }

        fn exec<M: Mmu>(&self, _address: u16, _state: &State, _mmu: &M) -> Result<()> {
            *self.called.borrow_mut() = true;
            Ok(())
        }
    }

    impl CallHook for CallHookMock {
        fn handle<M: Mmu>(&self, address: u16, state: &State, _mmu: &M) -> Option<Result<()>> {
            self.data.borrow_mut().0 = address;
            self.data.borrow_mut().1 = state.clone();
            Some(Ok(()))
        }
    }


    #[test]
    fn call_should_call_hook_handle() {
        let hook = Rc::new(CallHookMock::default());

        let mut cpu = Cpu {hook: hook.clone(), ..GenCpu::default() };

        cpu.exec(Instruction::Call(0x54ae)).unwrap();

        assert_eq!(hook.data.borrow().0, 0x54ae);
        assert_eq!(hook.data.borrow().1, cpu.state);
    }

    #[test]
    fn address_call_back_should_exec_his_address() {
        let address = 0x54ae;

        let hook = Rc::new(AddressMock { accept: address, ..Default::default() } );

        let mut cpu = Cpu {hook: hook.clone(), ..GenCpu::default() };

        cpu.exec(Instruction::Call(address)).unwrap();
        assert!(*hook.called.borrow());
    }

    #[test]
    fn address_call_back_should_not_exec_other_address() {
        let address = 0x54ae;
        let hook = Rc::new(AddressMock { accept: address, ..Default::default() } );

        let mut cpu = Cpu {hook: hook.clone(), ..GenCpu::default() };

        for a in 0x1000..0x2000 {
            cpu.exec(Instruction::Call(a)).unwrap();
        }
        assert!(!*hook.called.borrow());
    }
}
