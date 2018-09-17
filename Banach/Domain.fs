namespace Banach

type Expr =
    {
        Body : ExprBody
        Type : ExprBody
    }
with
    override this.ToString () = sprintf "%O : %O" this.Body this.Type

and ExprBody =
// Standard three lambda calculus constructors
| EBIdentifier of QualifiedIdentifier
| EBApplication of f : Expr * x : Expr
| EBAbstraction of var : Identifier * varType : Expr * body : Expr
// Pattern matching
| EBMatch of Expr * (QualifiedIdentifier * Identifier list * Expr) list
// Type-oriented extras for our language
| EBType
| EBPi of fromVar : Identifier * fromType : Expr * toType : Expr
| EBArrow of fromType : Expr * toType : Expr
with
    override this.ToString () =
        match this with
        | EBIdentifier i -> sprintf "%O" i
        | EBApplication (f, x) -> sprintf "(%O) (%O)" f.Body x.Body
        | EBAbstraction (var, varType, body) ->
            sprintf "fun (%O : %O) -> %O" var varType.Body body.Body
        | EBMatch (e, ps) ->
            let printPattern (qi, is, e : Expr) =
                sprintf "%O %s -> %O" qi (is |> Seq.map (sprintf "%O") |> String.concat " ") e.Body
            sprintf "match %O with %s" e.Body (ps |> Seq.map printPattern |> String.concat " | ")
        | EBType -> "Type"
        | EBPi (fromVar, fromType, toType) ->
            sprintf "(%O : %O) -> (%O)" fromVar fromType.Body toType.Body
        | EBArrow (fromType, toType) ->
            sprintf "(%O) -> (%O)" fromType.Body toType.Body

(*
let foo e =
    match e with
    | EBIdentifier i -> failwith ""
    | EBApplication (f, x) -> failwith ""
    | EBAbstraction (var, varType, body) -> failwith ""
    | EBMatch (e, ps) -> failwith ""
    | EBType -> failwith ""
    | EBPi (fromVar, fromType, toType) -> failwith ""
    | EBArrow (fromType, toType) -> failwith ""
*)

type TypeDefinition =
    {
        Name : Identifier
        Type : Expr
        Constructors : (Identifier * Expr) list
    }

type ValueDefinition =
    {
        Name : Identifier
        Expr : Expr
    }

type Definition =
| DType of TypeDefinition
| DValue of ValueDefinition
