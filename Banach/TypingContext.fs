namespace Banach

type TypingContext = TypingContext of Map<QualifiedIdentifier, Expr>
with
    override this.ToString () =
        let (TypingContext map) = this
        map |> Map.toSeq |> Seq.map (fun (i, e) -> sprintf "%O : %O" i e.Body) |> String.concat ", "

[<RequireQualifiedAccess>]
module TypingContext =

    let empty = TypingContext Map.empty

    let add qi (e : Expr) (TypingContext map) =
        if Expr.getType e <> Expr.Type then
            failwithf "The expression %O being added to the typing context did not have type Type" e
        TypingContext (Map.add qi e map)

    let tryFind qi (TypingContext map) = Map.tryFind qi map
