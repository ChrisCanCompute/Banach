namespace Parsy

[<RequireQualifiedAccess>]
type ResultList<'a, 'b> =
| Cons of 'a * ResultList<'a, 'b>
| Done
| Error of 'b

[<RequireQualifiedAccess>]
module ResultList =

    let rec ofList (xs : 'a List) (err : 'b option) : ResultList<'a, 'b> =
        match xs with
        | [] ->
            match err with
            | None -> ResultList.Done
            | Some b -> ResultList.Error b
        | x::xs ->
            ResultList.Cons (x, ofList xs err)
