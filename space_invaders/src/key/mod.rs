use sdl2::keyboard::{Keycode, KeyboardState, Scancode};
use sdl2::event::Event;

#[derive(Default)]
struct FlipFlop {
    pub state: bool
}

impl FlipFlop {
    pub fn change(&mut self) {
        self.state = !self.state;
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum KeyState {
    Pressed,
    Released,
}

impl From<bool> for KeyState {
    fn from(state: bool) -> Self {
        match state {
            true => KeyState::Pressed,
            false => KeyState::Released
        }
    }
}

impl Into<bool> for KeyState {
    fn into(self) -> bool {
        match self {
            KeyState::Pressed => true,
            KeyState::Released => false,
        }
    }
}

pub trait WindowKey {
    fn update(&mut self, event: &Event) -> Option<()>;
    fn active(&self) -> bool;
    fn change_state(&mut self, val: bool) -> bool;
}

pub struct DirectKey {
    key: Keycode,
    last: KeyState,
}

impl From<Keycode> for DirectKey {
    fn from(key: Keycode) -> Self {
        DirectKey {
            key: key.into(),
            last: KeyState::Released,
        }
    }
}

impl WindowKey for DirectKey {
    fn update(&mut self, event: &Event) -> Option<()> {
        let new_state = match event {
            Event::KeyUp {keycode: Some(code), repeat: false, ..} if code == &self.key => Some(KeyState::Released) ,
            Event::KeyDown {keycode: Some(code), repeat: false, ..} if code == &self.key => Some(KeyState::Pressed) ,
            _ => None
        }?;
        let prev = self.last;
        self.last = new_state;
        match prev != self.last {
            true => Some(()),
            false => None
        }
    }

    fn active(&self) -> bool {
        self.last.into()
    }

    fn change_state(&mut self, val: bool) -> bool {
        let old = self.last;
        self.last = val.into();
        old != self.last
    }
}

pub struct FlipFlopKey<K: WindowKey> {
    key: K,
    flip_flop: FlipFlop,
}

impl<K: WindowKey> FlipFlopKey<K> {
    pub fn new(key: K, state: bool) -> Self {
        Self { key, flip_flop: FlipFlop { state } }
    }
}

impl<K: WindowKey> From<K> for FlipFlopKey<K> {
    fn from(key: K) -> Self {
        FlipFlopKey {
            key,
            flip_flop: Default::default(),
        }
    }
}

impl<K: WindowKey> WindowKey for FlipFlopKey<K> {
    fn update(&mut self, event: &Event) -> Option<()> {
        self.key.update(event)
            .filter(|_| !self.key.active())
            .map(|_| self.flip_flop.change())
    }

    fn active(&self) -> bool {
        self.flip_flop.state
    }

    fn change_state(&mut self, val: bool) -> bool {
        if val != self.flip_flop.state {
            self.flip_flop.change();
            true
        } else {
            false
        }
    }
}

pub struct ActiveKey<K: WindowKey, A: Fn(bool)> {
    key: K,
    action: A,
}

impl<K: WindowKey, A: Fn(bool)> ActiveKey<K, A> {
    pub fn new(key: K, action: A) -> Self {
        Self { key, action }
    }
}

pub type DKey<F> = ActiveKey<DirectKey, F>;

impl<K: WindowKey, F: Fn(bool)> WindowKey for ActiveKey<K, F> {
    fn update(&mut self, event: &Event) -> Option<()> {
        self.key
            .update(event)
            .map(|_| self.call_action())
    }

    fn active(&self) -> bool {
        self.key.active()
    }

    fn change_state(&mut self, val: bool) -> bool {
        let changed = self.key.change_state(val);
        if changed {
            self.call_action();
        }
        changed
    }
}

impl<K: WindowKey, F: Fn(bool)> ActiveKey<K, F> {
    fn call_action(&self) {
        self.action.call((self.active(), ));
    }
}
