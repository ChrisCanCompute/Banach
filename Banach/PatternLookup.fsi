namespace Banach

type PatternLookup

[<RequireQualifiedAccess>]
module PatternLookup =

    val empty : PatternLookup

    val add : Identifier -> (Identifier * Expr) list -> PatternLookup -> PatternLookup

    /// Returns the list of possible constructors that could match the given type
    val tryFind : Expr -> PatternLookup -> (QualifiedIdentifier * Expr) list option
