mod expr;
mod state_builder;

struct State<'i> {
    input: &'i [u8],
    index: usize,
    result: bool,
}

macro_rules! peek {
    ( $state:expr, $callback:expr ) => {
        match $state.input.get($state.index) {
            Some(&val) => val,
            None => return $callback($state),
        }
    };
}

macro_rules! pop {
    ( $state:expr, $callback:expr ) => {
        match $state.input.get($state.index) {
            Some(&val) => {
                let val = $state.input[$state.index];
                $state.index += 1;
                val
            }
            None => return $callback($state),
        }
    };
}

impl<'i> State<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            input: input.as_bytes(),
            index: 0,
            result: true,
        }
    }

    pub fn json(state: &mut State) {
        const LUT: [fn(&mut State); 256] = [State::error; 256];

        if peek!(state, Self::error) == b'c' {
            println!("hi");
        }

        if pop!(state, Self::error) == b'c' {
            println!("hi");
        }
    }

    pub fn error(state: &mut State) {
        state.result = false;
    }
}
