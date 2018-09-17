namespace Banach

type TypeCheckerContext =
    {
        DefinitionLookup : DefinitionLookup
        PatternLookup : PatternLookup
        TypingContext : TypingContext
    }

[<RequireQualifiedAccess>]
module TypeChecker =

    // Implicitly, we expect the type of the expression to be Type
    let rec typeCheckType (context : TypeCheckerContext) (t : Untyped.Type) : Expr TypeCheckResult =

        let check = typeCheckType context

        match t with
        | Untyped.Type.TExpression e ->
            typeCheckExpression context (Some Expr.Type) e
        | Untyped.Type.TParameter (t1, t2) ->
            result {
                let! t1, t2 = TCRes.zip (check t1) (check t2)
                return Expr.make (EBArrow (t1, t2)) EBType
            }
        | Untyped.Type.TNamedParameter (i, t1, t2) ->
            result {
                let! t1, t2 = TCRes.zip (check t1) (check t2)
                return Expr.make (EBPi (i, t1, t2)) EBType
            }

    and typeCheckExpression (context : TypeCheckerContext) (expectedType : Expr option) (e : Untyped.Expression) : Expr TypeCheckResult =

        let matchPatterns
            (given : (Untyped.Pattern * Untyped.Expression) list)
            (valid : (QualifiedIdentifier * Expr) list)
            : (QualifiedIdentifier * Identifier list * Untyped.Expression * Expr) list TypeCheckResult =

            let givenKeys = given |> Seq.map (fun (Untyped.Pattern (i, _), _) -> i) |> Set.ofSeq
            let validKeys = valid |> Seq.map fst |> Set.ofSeq
            let missingFromGiven = Set.difference validKeys givenKeys
            let notInValid = Set.difference givenKeys validKeys

            if not (Set.isEmpty missingFromGiven) || not (Set.isEmpty notInValid) then
                Seq.append
                    (missingFromGiven |> Seq.map MatchPatternNotGiven)
                    (notInValid |> Seq.map MatchPatternNotValid)
                |> Seq.map TypeCheckError.make
                |> TypeCheckError.aggregate |> Error
            else
                // The sets of keys are identical
                let valid = valid |> Map.ofSeq
                given |> Seq.map (fun (Untyped.Pattern (i, is), e) -> i, is, e, Map.find i valid) |> List.ofSeq |> Ok

        let typeCheckCase
            (matchExpr : Expr) // Should this be passed down this far? Why not just change lookup instead?
            (expectedType : Expr)
            (i : QualifiedIdentifier, is : Identifier list, e : Untyped.Expression, t : Expr)
            : (QualifiedIdentifier * Identifier list * Expr) TypeCheckResult =

            match is with
            | [] ->

                let expectedType =
                    let matchIdenfitier =
                        match matchExpr.Body with
                        | EBIdentifier i -> i
                        | _ -> failwith "Pattern match was not on a single identifier"
                    let lookup = context.DefinitionLookup |> DefinitionLookup.add matchIdenfitier (Expr.make (EBIdentifier i) matchExpr.Type)
                    expectedType |> Normaliser.normalise lookup

                result {
                    let! e = typeCheckExpression context (Some expectedType) e
                    return i, is, e
                }
            | _ -> failwith "Parameters in patterns not yet supported"

        let e =
            match e with
            | Untyped.Expression.EIdentifier i ->
                match TypingContext.tryFind i context.TypingContext with
                | Some t -> Ok (Expr.make (EBIdentifier i) (t |> Expr.getBody))
                | None -> TCRes.error (NotDefined i)

            | Untyped.Expression.EApplication (e1, e2) ->

                let getArrowElements (e : Expr) : (Expr * Expr) TypeCheckResult =
                    match e.Body with
                    | EBPi (fromVar, fromType, toType) -> Ok (fromType, toType)
                    | EBArrow (fromType, toType) -> Ok (fromType, toType)
                    | _ -> TCRes.error NotArrowType

                result {
                    let! e1 = typeCheckExpression context None e1
                    // Ensure that e1 has type 'a -> 'b and e2 has type 'b
                    let! tFrom, tTo = e1 |> Expr.getType |> getArrowElements
                    // Ensure that e2 has type 'b
                    let! e2 = typeCheckExpression context (Some tFrom) e2
                    return Expr.make (EBApplication (e1, e2)) (tTo |> Expr.getBody)
                }

            | Untyped.Expression.EMatch (e, ps) ->
                result {
                    // 0) Ensure that we have been told what the expected return type is -
                    //      we can't figure it out for ourselves in this case.
                    match expectedType with
                    | None ->
                        return! TCRes.error MatchNoExpectedReturnType
                    | Some expectedType ->
                        // 1) Type check the expression that we are matching against
                        //      - we don't know what the type of this expression should be
                        let! e = typeCheckExpression context None e
                        let t = e |> Expr.getType
                        // 2) Look up the valid patterns to match on for this particular type
                        let patterns = context.PatternLookup |> PatternLookup.tryFind t
                        match patterns with
                        | None ->
                            return! TCRes.error CouldNotDeterminePatterns
                        | Some patterns ->
                            // 3) Try to match the valid patterns against those given
                            let! matched = matchPatterns ps patterns
                            let! cases = matched |> Seq.map (typeCheckCase e expectedType) |> TCRes.allOrNone
                            return Expr.make (EBMatch (e, cases |> List.ofSeq)) (expectedType |> Expr.getBody)
                }

        // Check against expected type
        match expectedType with
        | Some expectedType ->
            result {
                let! e = e
                let t = e |> Expr.getType
                if t = expectedType then
                    return e
                else
                    return! TCRes.error TypeDidntMatchExpected
            }
        | None -> e

    let typeCheckTypeDefinition (context : TypeCheckerContext) (typeDef : Untyped.TypeDefinition) : TypeDefinition TypeCheckResult =

        // Note this could be done much more efficiently if we interleave it with the type checking
        // N.B. we can probably write this better with active patterns
        let rec validateKind (kind : Expr) : unit TypeCheckResult =
            match kind.Body with
            | EBType -> Ok ()
            | EBPi (_, _, toType) -> validateKind toType
            | EBArrow (_, toType) -> validateKind toType
            | _ -> TCRes.error TypeDefKindWrong

        // Note this could be done much more efficiently if we interleave it with the type checking
        // N.B. we can probably write this better with active patterns
        let rec validateConstructorType (constructorType : Expr) : unit TypeCheckResult =
            match constructorType.Body with
            | EBApplication _ -> failwith "Complicated case"
            | EBIdentifier i when i = QIIdentifier typeDef.Name -> Ok ()
            | EBPi (_, _, toType) -> validateConstructorType toType
            | EBArrow (_, toType) -> validateConstructorType toType
            | _ -> TCRes.error TypeDefConstructorTypeWrong

        let typeCheckConstructor (context : TypeCheckerContext) (c : Untyped.ConstructorDefinition) : (Identifier * Expr) TypeCheckResult =
            result {
                let! t = typeCheckType context c.Type
                do! validateConstructorType t
                return c.Name, t
            }

        result {
            let! tType = typeCheckType context typeDef.Type
            let tType = tType |> Normaliser.normalise context.DefinitionLookup
            // 1) Ensure that the type of the type definition is value (i.e. is a series of parameters ending in Type)
            do! validateKind tType
            let typingContext = context.TypingContext |> TypingContext.add (QIIdentifier typeDef.Name) tType
            let context = { context with TypingContext = typingContext }
            // 2) Ensure that the type of each constructor is value (i.e. is a series of parameters ending in the parameterised specific type)
            let! cs = typeDef.Constructors |> List.map (typeCheckConstructor context) |> TCRes.allOrNone
            return
                {
                    Name = typeDef.Name
                    Type = tType
                    Constructors = cs |> List.ofSeq
                }
        }

    let typeCheckValueDefinition (context : TypeCheckerContext) (valueDef : Untyped.ValueDefinition) : ValueDefinition TypeCheckResult =

        let rec inner (context : TypeCheckerContext) (parameters : (Identifier * Untyped.Type) list) : Expr TypeCheckResult =
            match parameters with
            | [] ->
                result {
                    let! returnType = typeCheckType context valueDef.Type
                    let returnType = returnType |> Normaliser.normalise context.DefinitionLookup
                    return! typeCheckExpression context (Some returnType) valueDef.Body
                }
            | (i, t)::ps ->
                result {
                    let! paramType = typeCheckType context t
                    let context = { context with TypingContext = context.TypingContext |> TypingContext.add (QIIdentifier i) paramType }
                    let! e = inner context ps
                    let newBody = EBAbstraction (i, paramType, e)
                    let newType = EBPi (i, paramType, e |> Expr.getType)
                    return Expr.make newBody newType
                }

        result {
            let! e = inner context valueDef.Parameters
            return
                {
                    Name = valueDef.Name
                    Expr = e
                }
        }
