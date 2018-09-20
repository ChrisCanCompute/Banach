namespace Banach

type PatternLookup = PatternLookup of Map<Identifier, (Identifier * Expr) list>

[<RequireQualifiedAccess>]
module PatternLookup =

    let empty = PatternLookup Map.empty

    let add i patterns (PatternLookup map) : PatternLookup =
        map |> Map.add i patterns |> PatternLookup

    /// Returns the list of possible constructors that could match the given type
    let tryFind (t : Expr) (PatternLookup map) : (QualifiedIdentifier * Expr) list option =

        let rec getTypeName (t : Expr) : Identifier =
            match t with
            | Apps (e, _) ->
                match e.Body with
                | EBIdentifier (QIIdentifier i) -> i
                | _ -> failwith "Could not get type name"

        // N.B. we should filter these to remove irrelevant patterns
        let typeName = getTypeName t

        match map |> Map.tryFind typeName with
        | None -> None
        | Some patterns ->
            patterns |> List.map (fun (i, e) -> QIQualified (typeName, i), e) |> Some
