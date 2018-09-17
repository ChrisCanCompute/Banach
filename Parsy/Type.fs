namespace Parsy

open Microsoft.FSharp.Reflection
open System

[<RequireQualifiedAccess>]
module Type =

    let rec printInner (t : Type) : string =
        if t = typeof<int> then
            "int"
        else if t = typeof<char> then
            "char"
        else if t = typeof<string> then
            "string"
        else if t |> FSharpType.IsFunction then
            let dom, ran = FSharpType.GetFunctionElements t
            sprintf "%s -> %s" (printInner dom) (printInner ran)
        else if t |> FSharpType.IsTuple then
            let ts = FSharpType.GetTupleElements t
            ts |> Seq.map printInner |> String.concat " * "
        else if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<_ list> then
            let elementType = t.GetGenericArguments () |> Seq.exactlyOne
            sprintf "%s list" (printInner elementType)
        else
            failwithf "%A" t

    let print<'t> = printInner (typeof<'t>)
