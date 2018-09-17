namespace Banach

type DefinitionLookup

[<RequireQualifiedAccess>]
module DefinitionLookup =

    val empty : DefinitionLookup

    val add : QualifiedIdentifier -> Expr -> DefinitionLookup -> DefinitionLookup

    val tryFind : QualifiedIdentifier -> DefinitionLookup -> Expr option
