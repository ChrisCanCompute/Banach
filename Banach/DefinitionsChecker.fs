namespace Banach

[<RequireQualifiedAccess>]
module DefinitionsChecker =

    let typeCheckDefinitions (definitions : Untyped.Definition list) : Definition TypeCheckResult list =

        let rec inner (context : TypeCheckerContext) (definitions : Untyped.Definition list) : Definition TypeCheckResult list =
            match definitions with
            | [] -> []
            | d::ds ->

                match d with
                | Untyped.Definition.DType typeDef ->
                    let res = TypeChecker.typeCheckTypeDefinition context typeDef
                    match res with
                    | Ok typeDef ->
                        let typingContext =
                            let addConstructor c (i, e) = TypingContext.add (QIQualified (typeDef.Name, i)) e c
                            context.TypingContext
                            |> TypingContext.add (QIIdentifier typeDef.Name) typeDef.Type
                            |> (fun typingContext -> Seq.fold addConstructor typingContext typeDef.Constructors)
                        let patterns = PatternLookup.add typeDef.Name typeDef.Constructors context.PatternLookup
                        let context = { context with TypingContext = typingContext ; PatternLookup = patterns }
                        (typeDef |> DType |> Ok) :: inner context ds
                    | Error info ->
                        Error info :: inner context ds

                | Untyped.Definition.DValue valueDef ->
                    let res = TypeChecker.typeCheckValueDefinition context valueDef
                    match res with
                    | Ok valueDef ->
                        let name = QIIdentifier valueDef.Name
                        let typingContext = TypingContext.add name (Expr.getType valueDef.Expr) context.TypingContext
                        let definitions = DefinitionLookup.add name valueDef.Expr context.DefinitionLookup
                        let context = { context with TypingContext = typingContext ; DefinitionLookup = definitions }
                        (valueDef |> DValue |> Ok) :: inner context ds
                    | Error info ->
                        Error info :: inner context ds

        let typeIdentifier = QIIdentifier (IIdentifier "Type")

        let initialContext =
            {
                DefinitionLookup =
                    DefinitionLookup.empty
                    |> DefinitionLookup.add typeIdentifier Expr.Type
                PatternLookup =
                    PatternLookup.empty
                TypingContext =
                    TypingContext.empty
                    |> TypingContext.add typeIdentifier Expr.Type
            }

        inner initialContext definitions
