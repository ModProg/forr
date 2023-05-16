// use leptos_reactive::*;
// use forr::forr;
//
// enum OptMbSigOpt<T: 'static> {
//     Value(Option<MaybeSignal<T>>),
//     Option(Option<MaybeSignal<Option<T>>>),
// }
//
// forr! { $type:ty in [MaybeSignal, Memo, ReadSignal, RwSignal, Signal] $*
//     impl<T> From<$type<T>> for OptMbSigOpt<T> {
//         fn from(value: $type<T>) -> Self {
//             Self::Value(Some(value.into()))
//         }
//     }
//     impl<T> From<Option<$type<T>>> for OptMbSigOpt<T> {
//         fn from(value: Option<$type<T>>) -> Self {
//             Self::Value(value.map(Into::into))
//         }
//     }
//     impl<T> From<$type<Option<T>>> for OptMbSigOpt<T> {
//         fn from(value: $type<Option<T>>) -> Self {
//             Self::Option(Some(value.into()))
//         }
//     }
//     impl<T> From<Option<$type<Option<T>>>> for OptMbSigOpt<T> {
//         fn from(value: Option<$type<Option<T>>>) -> Self {
//             Self::Option(value.map(Into::into))
//         }
//     }
// }
