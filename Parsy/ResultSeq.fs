namespace Parsy

[<RequireQualifiedAccess>]
type internal ResultSeqInner<'a, 'b> =
| Next of 'a * ResultSeq<'a, 'b>
| Done
| Error of 'b

and ResultSeq<'a, 'b> = internal ResultSeq of (unit -> ResultSeqInner<'a, 'b>)

[<RequireQualifiedAccess>]
module ResultSeq =

    let cons (a : 'a) (rs : ResultSeq<'a, 'b>) =
        ResultSeq (fun () -> ResultSeqInner.Next (a, rs))

    let rec ofSeq (s : 'a seq) : ResultSeq<'a, 'b> =
        fun () ->
            if s |> Seq.isEmpty then
                ResultSeqInner.Done
            else
                ResultSeqInner.Next (s |> Seq.head, s |> Seq.tail |> ofSeq)
        |> ResultSeq

    let rec toResultList (ResultSeq f) =
        match f () with
        | ResultSeqInner.Next (a, rs) -> ResultList.Cons (a, toResultList rs)
        | ResultSeqInner.Done -> ResultList.Done
        | ResultSeqInner.Error b -> ResultList.Error b
