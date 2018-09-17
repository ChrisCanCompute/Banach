namespace Banach

type DefinitionLookup = DefinitionLookup of Map<QualifiedIdentifier, Expr>

[<RequireQualifiedAccess>]
module DefinitionLookup =

    let empty = DefinitionLookup Map.empty

    let add qi e (DefinitionLookup map) = DefinitionLookup (Map.add qi e map)

    let tryFind qi (DefinitionLookup map) = Map.tryFind qi map
