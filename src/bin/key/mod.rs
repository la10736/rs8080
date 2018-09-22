use minifb::Window;
use minifb::Key;

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
    fn update(&mut self, window: &Window) -> bool;
    fn active(&self) -> bool;
    fn change_state(&mut self, val: bool) -> bool;
}

pub struct DirectKey {
    key: Key,
    last: KeyState,
}

impl From<Key> for DirectKey {
    fn from(key: Key) -> Self {
        DirectKey {
            key,
            last: KeyState::Released,
        }
    }
}

impl WindowKey for DirectKey {
    fn update(&mut self, window: &Window) -> bool {
        let prev = self.last;
        self.last = window.is_key_down(self.key).into();
        prev != self.last
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
    fn update(&mut self, window: &Window) -> bool {
        let changed = self.key.update(window);

        if changed && !self.key.active() {
            self.flip_flop.change();
            true
        } else {
            false
        }
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
    fn update(&mut self, window: &Window) -> bool {
        let changed = self.key.update(window);
        if changed {
            self.call_action();
        }
        changed
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
