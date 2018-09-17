namespace Banach

type TypeCheckerContext =
    {
        DefinitionLookup : DefinitionLookup
        PatternLookup : PatternLookup
        TypingContext : TypingContext
    }

[<RequireQualifiedAccess>]
module TypeChecker =

    val typeCheckTypeDefinition : TypeCheckerContext -> Untyped.TypeDefinition -> TypeDefinition TypeCheckResult

    val typeCheckValueDefinition : TypeCheckerContext -> Untyped.ValueDefinition -> ValueDefinition TypeCheckResult
