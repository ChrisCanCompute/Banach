namespace Banach

[<RequireQualifiedAccess>]
module DefinitionsChecker =

    val typeCheckDefinitions : Untyped.Definition list -> Definition TypeCheckResult list
