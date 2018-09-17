namespace Banach

[<RequireQualifiedAccess>]
module Normaliser =

    let rec substituteInExpression (i : Identifier) (toSubstitute : Expr) (e : Expr) : Expr =
        // Substitution does not change the type of the expression
        Expr.make (substituteInExpressionBody i toSubstitute e.Body) e.Type

    and substituteInExpressionBody (i : Identifier) (toSubstitute : Expr) (body : ExprBody) : ExprBody =
        let sub = substituteInExpression i toSubstitute
        match body with
        | EBIdentifier i2 when i2 = QIIdentifier i -> toSubstitute |> Expr.getBody
        | EBIdentifier _ -> body
        | EBApplication (f, x) -> EBApplication (sub f, sub x)
        | EBAbstraction _ -> body
        | EBMatch (e, ps) -> EBMatch (sub e, ps)
        | EBType -> body
        | EBPi (fromVar, fromType, toType) when fromVar = i -> EBPi (fromVar, sub fromType, toType)
        | EBPi (fromVar, fromType, toType) -> EBPi (fromVar, sub fromType, sub toType)
        | EBArrow (fromType, toType) -> EBArrow (sub fromType, sub toType)

    let rec normalise (lookup : DefinitionLookup) (e : Expr) : Expr =
        match e.Body with
        | EBIdentifier i ->
            match DefinitionLookup.tryFind i lookup with
            | Some e -> normalise lookup e
            | None -> e

        | EBApplication (f, x) ->

            let checkAgainstExpected (expectedTypeOfX : Expr) =
                if x.Type <> (expectedTypeOfX |> Expr.getBody) then
                    failwith "The type of the parameter did not match the function!"

            let f = f |> normalise lookup
            let x = x |> normalise lookup

            match f.Body with
            | EBAbstraction (var, varType, body) ->
                match f.Type with
                | EBPi (fromVar, fromType, toType) ->
                    checkAgainstExpected fromType
                    let body = substituteInExpression var x body |> Expr.getBody
                    let t = substituteInExpression fromVar x toType |> Expr.getBody
                    Expr.make body t
                | EBArrow (fromType, toType) ->
                    checkAgainstExpected fromType
                    let body = substituteInExpression var x body |> Expr.getBody
                    Expr.make body toType.Body
                | _ -> failwith "Cannot apply parameter to function - wrong type"
            | _ -> failwith "Function was not a lambda abstraction"

        | EBAbstraction _ -> e
        | EBMatch (e, ps) ->
            let e = normalise lookup e

            // Has e been reduced to a constructor?
            // N.B. this currently only works for constructors without parameters!!!
            match e.Body with
            | EBIdentifier qi ->
                let matchingCases = ps |> List.filter (fun (i, is, _) -> i = qi && is = [])
                match matchingCases with
                | [] -> Expr.make (EBMatch (e, ps)) e.Type
                | [ (_, _, e) ] -> normalise lookup e
                | _ -> failwith "found multiple matching cases"
            | _ ->
                Expr.make (EBMatch (e, ps)) e.Type
        | EBType -> e
        | EBPi (fromVar, fromType, toType) ->
            Expr.make (EBPi (fromVar, normalise lookup fromType, normalise lookup toType)) e.Type
        | EBArrow (fromType, toType) ->
            Expr.make (EBArrow (normalise lookup fromType, normalise lookup toType)) e.Type
