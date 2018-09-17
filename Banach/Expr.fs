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
