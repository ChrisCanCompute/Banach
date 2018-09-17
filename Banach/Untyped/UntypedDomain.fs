namespace Banach.Untyped

open Banach

type Pattern = Pattern of QualifiedIdentifier * Identifier list

type Expression =
| EIdentifier of QualifiedIdentifier
| EApplication of Expression * Expression
| EMatch of Expression * (Pattern * Expression) list

type Type =
| TExpression of Expression
| TParameter of Type * Type
| TNamedParameter of Identifier * Type * Type

type ConstructorDefinition =
    {
        Name : Identifier
        Type : Type
    }

type TypeDefinition =
    {
        Name : Identifier
        Type : Type
        Constructors : ConstructorDefinition list
    }

type ValueDefinition =
    {
        Recursive : bool
        Name : Identifier
        Parameters : (Identifier * Type) list
        Type : Type
        Body : Expression
    }

type Definition =
| DType of TypeDefinition
| DValue of ValueDefinition


[<RequireQualifiedAccess>]
module Expression =

    let rec prettyPrint (e : Expression) =
        match e with
        | EIdentifier qi -> sprintf "%O" qi
        | EApplication (e1, e2) -> sprintf "(%s) (%s)" (prettyPrint e1) (prettyPrint e2)
        | EMatch (e, cases) -> failwith "Match"
