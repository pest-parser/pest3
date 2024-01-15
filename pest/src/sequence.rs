#[macro_export]
macro_rules! sequence_type {
    ( $name:ident, $( ( $variant:ident, $type:ident ) ),* $(,)? ) => {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct $name<$( $type ),*> {
            $( $variant: $type ),*
        }
    };
}

sequence_type! {
    Sequence2,
    (field_0, T0),
    (field_1, T1)
}

sequence_type! {
    Sequence3,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2)
}

sequence_type! {
    Sequence4,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3)
}

sequence_type! {
    Sequence5,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4)
}

sequence_type! {
    Sequence6,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5)
}

sequence_type! {
    Sequence7,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6)
}

sequence_type! {
    Sequence8,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6),
    (field_7, T7)
}

sequence_type! {
    Sequence9,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6),
    (field_7, T7),
    (field_8, T8)
}

sequence_type! {
    Sequence10,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6),
    (field_7, T7),
    (field_8, T8),
    (field_9, T9)
}

sequence_type! {
    Sequence11,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6),
    (field_7, T7),
    (field_8, T8),
    (field_9, T9),
    (field_10, T10)
}

sequence_type! {
    Sequence12,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6),
    (field_7, T7),
    (field_8, T8),
    (field_9, T9),
    (field_10, T10),
    (field_11, T11)
}

sequence_type! {
    Sequence13,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6),
    (field_7, T7),
    (field_8, T8),
    (field_9, T9),
    (field_10, T10),
    (field_11, T11),
    (field_12, T12)
}

sequence_type! {
    Sequence14,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6),
    (field_7, T7),
    (field_8, T8),
    (field_9, T9),
    (field_10, T10),
    (field_11, T11),
    (field_12, T12),
    (field_13, T13)
}

sequence_type! {
    Sequence15,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6),
    (field_7, T7),
    (field_8, T8),
    (field_9, T9),
    (field_10, T10),
    (field_11, T11),
    (field_12, T12),
    (field_13, T13),
    (field_14, T14)
}

sequence_type! {
    Sequence16,
    (field_0, T0),
    (field_1, T1),
    (field_2, T2),
    (field_3, T3),
    (field_4, T4),
    (field_5, T5),
    (field_6, T6),
    (field_7, T7),
    (field_8, T8),
    (field_9, T9),
    (field_10, T10),
    (field_11, T11),
    (field_12, T12),
    (field_13, T13),
    (field_14, T14),
    (field_15, T15)
}
