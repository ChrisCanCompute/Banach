namespace Banach

type PatternLookup = PatternLookup of Map<Identifier, (Identifier * Expr) list>

[<RequireQualifiedAccess>]
module PatternLookup =

    let empty = PatternLookup Map.empty

    let add i patterns (PatternLookup map) : PatternLookup =
        map |> Map.add i patterns |> PatternLookup

    /// Returns the list of possible constructors that could match the given type
    let tryFind (t : Expr) (PatternLookup map) : (QualifiedIdentifier * Expr) list option =

        // Can we make this easier with active patterns?
        let rec getTypeName (t : Expr) : Identifier =
            match t.Body with
            | EBIdentifier i ->
                match i with
                | QIIdentifier i -> i
                | QIQualified _ -> failwith "Shouldn't be a qualified identifier"
            | EBApplication (f, x) -> getTypeName f
            | EBAbstraction (var, varType, body) -> failwith "Shouldn't be an abstraction"
            | EBMatch (e, ps) -> failwith "Shouldn't be a match type"
            | EBType -> failwith "Shouldn't be Type"
            | EBPi (fromVar, fromType, toType) -> failwith "Shouldn't be a Pi type"
            | EBArrow (fromType, toType) -> failwith "Shouldn't be an arrow type"

        // N.B. we should filter these to remove irrelevant patterns
        let typeName = getTypeName t

        match map |> Map.tryFind typeName with
        | None -> None
        | Some patterns ->
            patterns |> List.map (fun (i, e) -> QIQualified (typeName, i), e) |> Some
