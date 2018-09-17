namespace Banach

type TypingContext = TypingContext of Map<QualifiedIdentifier, Expr>

[<RequireQualifiedAccess>]
module TypingContext =

    let empty = TypingContext Map.empty

    let add qi e (TypingContext map) = TypingContext (Map.add qi e map)

    let tryFind qi (TypingContext map) = Map.tryFind qi map
