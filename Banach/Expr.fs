namespace Banach

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Expr =

    let make body t =
        { Body = body ; Type = t }

    let Type : Expr =
        make EBType EBType

    let getType (e : Expr) : Expr =
        make e.Type EBType

    let getBody (e : Expr) : ExprBody =
        e.Body

[<AutoOpen>]
module ExprPatterns =

    let rec (|Arrows|) (e : Expr) : Choice<Expr, Identifier * Expr> list * Expr =
        match e.Body with
        | EBArrow (fromType, toType) ->
            match toType with
            | Arrows (arrows, last) -> Choice1Of2 fromType :: arrows, last
        | EBPi (fromVar, fromType, toType) ->
            match toType with
            | Arrows (arrows, last) -> Choice2Of2 (fromVar, fromType) :: arrows, last
        | _ -> [], e

    let rec (|Apps|) (e : Expr) : Expr * Expr list =
        match e.Body with
        | EBApplication (f, x) ->
            match f with
            | Apps (f, apps) -> f, apps @ [x]
        | _ -> e, []
