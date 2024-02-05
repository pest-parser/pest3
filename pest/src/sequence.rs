#[macro_export]
macro_rules! sequence_type {
    ( $name:ident, $( ( $variant:ident, $type:ident, $trivia:ident ) ),* $(,)? ) => {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct $name<$( $type, const $trivia: ::core::primitive::bool, )*> {
            $( $variant: $type ),*
        }
        impl<'i, R, $($type, const $trivia: ::core::primitive::bool, )*>
        $crate::typed::TypedNode<'i, R> for $name<$($type, $trivia, )*>
        where
            R: $crate::typed::RuleType,
            $(
                $type: $crate::typed::TypedNode<'i, R>,
            )*
        {
            #[inline]
            fn try_parse_with_partial(
                input: $crate::Position<'i>,
                stack: &mut $crate::Stack<$crate::Span<'i>>,
                tracker: &mut $crate::typed::Tracker<'i, R>,
            ) -> ::core::option::Option<($crate::Position<'i>, Self)> {
                $(
                    let input = if $trivia {
                        R::Trivia::try_parse_with_partial(input, stack, tracker)?.0
                    } else {
                        input
                    };
                    let (input, $variant) = $type::try_parse_with_partial(input, stack, tracker)?;
                )*
                let res = Self {
                    $($variant, )*
                };
                ::core::option::Option::Some((input, res))
            }
        }
    };
}

sequence_type! {
    Sequence2,
    (field_0, T0, TR0),
    (field_1, T1, TR1)
}

sequence_type! {
    Sequence3,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2)
}

sequence_type! {
    Sequence4,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3)
}

sequence_type! {
    Sequence5,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4)
}

sequence_type! {
    Sequence6,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5)
}

sequence_type! {
    Sequence7,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6)
}

sequence_type! {
    Sequence8,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6),
    (field_7, T7, TR7)
}

sequence_type! {
    Sequence9,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6),
    (field_7, T7, TR7),
    (field_8, T8, TR8)
}

sequence_type! {
    Sequence10,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6),
    (field_7, T7, TR7),
    (field_8, T8, TR8),
    (field_9, T9, TR9)
}

sequence_type! {
    Sequence11,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6),
    (field_7, T7, TR7),
    (field_8, T8, TR8),
    (field_9, T9, TR9),
    (field_10, T10, TR10)
}

sequence_type! {
    Sequence12,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6),
    (field_7, T7, TR7),
    (field_8, T8, TR8),
    (field_9, T9, TR9),
    (field_10, T10, TR10),
    (field_11, T11, TR11)
}

sequence_type! {
    Sequence13,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6),
    (field_7, T7, TR7),
    (field_8, T8, TR8),
    (field_9, T9, TR9),
    (field_10, T10, TR10),
    (field_11, T11, TR11),
    (field_12, T12, TR12)
}

sequence_type! {
    Sequence14,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6),
    (field_7, T7, TR7),
    (field_8, T8, TR8),
    (field_9, T9, TR9),
    (field_10, T10, TR10),
    (field_11, T11, TR11),
    (field_12, T12, TR12),
    (field_13, T13, TR13)
}

sequence_type! {
    Sequence15,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6),
    (field_7, T7, TR7),
    (field_8, T8, TR8),
    (field_9, T9, TR9),
    (field_10, T10, TR10),
    (field_11, T11, TR11),
    (field_12, T12, TR12),
    (field_13, T13, TR13),
    (field_14, T14, TR14)
}

sequence_type! {
    Sequence16,
    (field_0, T0, TR0),
    (field_1, T1, TR1),
    (field_2, T2, TR2),
    (field_3, T3, TR3),
    (field_4, T4, TR4),
    (field_5, T5, TR5),
    (field_6, T6, TR6),
    (field_7, T7, TR7),
    (field_8, T8, TR8),
    (field_9, T9, TR9),
    (field_10, T10, TR10),
    (field_11, T11, TR11),
    (field_12, T12, TR12),
    (field_13, T13, TR13),
    (field_14, T14, TR14),
    (field_15, T15, TR15)
}