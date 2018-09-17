namespace Banach

[<Sealed>]
type ResultBuilder () =

    member __.Return (a : 'a) : Result<'a, 'b> =
        Ok a

    member __.ReturnFrom (res : Result<'a, 'b>) : Result<'a, 'b> =
        res

    member __.Bind (a : Result<'a, 'c>, f : 'a -> Result<'b, 'c>) : Result<'b, 'c> =
        Result.bind f a

[<AutoOpen>]
module ResultBuilder =

    let result = ResultBuilder ()
