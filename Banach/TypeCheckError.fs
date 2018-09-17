namespace Banach

type TypeCheckErrorDetail =
| NotDefined of QualifiedIdentifier
| NotArrowType
| TypeDidntMatchExpected
| TypeDefKindWrong
| TypeDefConstructorTypeWrong
| CouldNotDeterminePatterns
| MatchNoExpectedReturnType
| MatchPatternNotGiven of QualifiedIdentifier
| MatchPatternNotValid of QualifiedIdentifier

type TypeCheckError = TypeCheckErrorDetail list

type 'a TypeCheckResult = Result<'a, TypeCheckError>

[<RequireQualifiedAccess>]
module TypeCheckError =

    let make (detail : TypeCheckErrorDetail) : TypeCheckError =
        [ detail ]

    let aggregate (infos : TypeCheckError seq) : TypeCheckError =
        infos |> Seq.concat |> List.ofSeq

[<RequireQualifiedAccess>]
module TCRes =

    let allOrNone (ress : 'a TypeCheckResult seq) : 'a seq TypeCheckResult =
        if ress |> Seq.exists (function Ok _ -> false | Error _ -> true) then
            ress |> Seq.choose (function Ok _ -> None | Error info -> Some info) |> TypeCheckError.aggregate |> Error
        else
            ress |> Seq.choose (function Ok a -> Some a | Error _ -> None) |> Ok

    let zip (res1 : 'a TypeCheckResult) (res2 : 'b TypeCheckResult) : ('a * 'b) TypeCheckResult =
        match res1, res2 with
        | Ok a, Ok b -> Ok (a, b)
        | Ok a, Error info -> Error info
        | Error info, Ok b -> Error info
        | Error info1, Error info2 -> Error (TypeCheckError.aggregate [ info1 ; info2 ])

    let error (detail : TypeCheckErrorDetail) : 'a TypeCheckResult =
        //Error (TypeCheckError.make detail)
        failwithf "%A" detail
