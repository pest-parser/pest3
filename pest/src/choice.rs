#[macro_export]
macro_rules! choice_type {
    ( $name:ident, $( ( $variant:ident, $type:ident ) ),* $(,)? ) => {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub enum $name<$( $type ),*> {
            $( $variant($type) ),*
        }
    };
}

choice_type! {
    Choice2,
    (Choice0, T0),
    (Choice1, T1)
}

choice_type! {
    Choice3,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2)
}

choice_type! {
    Choice4,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3)
}

choice_type! {
    Choice5,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4)
}

choice_type! {
    Choice6,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5)
}

choice_type! {
    Choice7,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6)
}

choice_type! {
    Choice8,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6),
    (Choice7, T7)
}

choice_type! {
    Choice9,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6),
    (Choice7, T7),
    (Choice8, T8)
}

choice_type! {
    Choice10,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6),
    (Choice7, T7),
    (Choice8, T8),
    (Choice9, T9)
}

choice_type! {
    Choice11,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6),
    (Choice7, T7),
    (Choice8, T8),
    (Choice9, T9),
    (Choice10, T10)
}

choice_type! {
    Choice12,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6),
    (Choice7, T7),
    (Choice8, T8),
    (Choice9, T9),
    (Choice10, T10),
    (Choice11, T11)
}

choice_type! {
    Choice13,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6),
    (Choice7, T7),
    (Choice8, T8),
    (Choice9, T9),
    (Choice10, T10),
    (Choice11, T11),
    (Choice12, T12)
}

choice_type! {
    Choice14,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6),
    (Choice7, T7),
    (Choice8, T8),
    (Choice9, T9),
    (Choice10, T10),
    (Choice11, T11),
    (Choice12, T12),
    (Choice13, T13)
}

choice_type! {
    Choice15,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6),
    (Choice7, T7),
    (Choice8, T8),
    (Choice9, T9),
    (Choice10, T10),
    (Choice11, T11),
    (Choice12, T12),
    (Choice13, T13),
    (Choice14, T14)
}

choice_type! {
    Choice16,
    (Choice0, T0),
    (Choice1, T1),
    (Choice2, T2),
    (Choice3, T3),
    (Choice4, T4),
    (Choice5, T5),
    (Choice6, T6),
    (Choice7, T7),
    (Choice8, T8),
    (Choice9, T9),
    (Choice10, T10),
    (Choice11, T11),
    (Choice12, T12),
    (Choice13, T13),
    (Choice14, T14),
    (Choice15, T15)
}
