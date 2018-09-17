namespace Banach

type TypingContext

[<RequireQualifiedAccess>]
module TypingContext =

    val empty : TypingContext

    val add : QualifiedIdentifier -> Expr -> TypingContext -> TypingContext

    val tryFind : QualifiedIdentifier -> TypingContext -> Expr option
