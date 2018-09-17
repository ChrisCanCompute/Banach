namespace Parsy

type ParserState =
    {
        Row : int
        Col : int
    }

[<RequireQualifiedAccess>]
module ParserState =

    let incrCol state = { state with Col = state.Col + 1 }


type 'a ReferenceParser =
| Running of (char -> 'a ReferenceParser) * (ParserState * 'a) option
| Done of Result<ParserState * 'a, string>

[<RequireQualifiedAccess>]
module ReferenceParser =

    let true' (a : 'a) (state : ParserState) : 'a ReferenceParser = Result.Ok (state, a) |> Done

    let false' (message : string) : 'a ReferenceParser = Result.Error message |> Done

    let constantChar (c : char) (state : ParserState) : char ReferenceParser =
        let f c' = if c = c' then true' c (ParserState.incrCol state) else false' (sprintf "%c did not match %c" c' c)
        Running (f, None)

    let charSet (p : char -> bool) (state : ParserState) : char ReferenceParser =
        let f c = if p c then true' c (ParserState.incrCol state) else false' (sprintf "%c was not in char set" c)
        Running (f, None)

    let rec alternate (ps : 'a ReferenceParser list) : 'a ReferenceParser =

        let toEmit =
            let extractV = function Running (f, v) -> v | Done (Ok v) -> Some v | Done (Error _) -> None
            let vs = ps |> List.choose extractV
            match vs with [] -> None | [v] -> Some v | _ -> failwith "Ambiguous parse detected"

        let running = ps |> List.choose (function Running (f, v) -> Some (f, v) | Done _ -> None)

        match running with
        | [] ->
            match toEmit with
            | Some v -> Done (Result.Ok v)
            | None -> Done (Result.Error "None of the various parsers matched")
        | rs ->
            let p c = rs |> List.map (fst >> (|>) c) |> alternate
            Running (p, toEmit)

    let rec map (f : 'a -> 'b) (p : 'a ReferenceParser) : 'b ReferenceParser =

        let mapF g = fun c -> map f (g c)
        let mapResult = Result.map (fun (state, v) -> state, f v)
        let mapOption = Option.map (fun (state, v) -> state, f v)

        match p with
        | Running (g, v) -> Running (mapF g, mapOption v)
        | Done v -> mapResult v |> Done

    let rec mapState (f : ParserState -> ParserState) (p : 'a ReferenceParser) : 'a ReferenceParser =

        let mapF g = fun c -> mapState f (g c)
        let mapResult = Result.map (fun (state, v) -> f state, v)
        let mapOption = Option.map (fun (state, v) -> f state, v)

        match p with
        | Running (g, v) -> Running (mapF g, mapOption v)
        | Done v -> mapResult v |> Done

    let rec apply (p1 : ('a -> 'b) ReferenceParser) (p2 : ParserState -> 'a ReferenceParser) : 'b ReferenceParser =

        let moveNext (state, f) = p2 state |> map ((<|) f)
        let stay f = Running ((fun c -> apply (f c) p2), None)

        match p1 with
        | Done (Ok v) -> moveNext v
        | Done (Error s) -> Done (Error s)
        | Running (f, v) ->
            match v with
            | None -> stay f
            | Some v ->
                alternate [ stay f ; moveNext v ]

    let sequence (p1 : 'a ReferenceParser) (p2 : ParserState -> 'b ReferenceParser) : ('a * 'b) ReferenceParser =
        apply (map (fun a b -> a, b) p1) p2

    let constant (s : string) (state : ParserState) : string ReferenceParser =
        Seq.foldBack
            (fun c f state -> sequence (constantChar c state) f |> map List.Cons)
            s
            (true' [])
        <| state
        |> map (Array.ofList >> System.String)

    let rec filter (f : 'a -> bool) (p : 'a ReferenceParser) : 'a ReferenceParser =
        match p with
        | Done (Ok (state, a)) when f a -> Done (Ok (state, a))
        | Done (Ok _) -> Done (Error "Did not match the filter")
        | Done (Error s) -> Done (Error s)
        | Running (g, v) ->
            let g c = filter f (g c)
            let v = v |> Option.bind (fun (state, v) -> if f v then Some (state, v) else None)
            Running (g, v)

    let newLine (indent : int) (state : ParserState) : string ReferenceParser =
        let padding = System.String (' ', indent)
        constant (sprintf "\r\n%s" padding) state
        |> mapState (fun state -> { state with Row = state.Row + 1 ; Col = indent + 1 })

    let congReferenceParser (teq : Teq<'a, 'b>) =
        Teq.believeMe<'a ReferenceParser, 'b ReferenceParser>

    let cast teq = Teq.castFrom (congReferenceParser teq)

    let rec makeParser<'a> (p : 'a Parser) (indent : int) (context : string list) (state : ParserState) : 'a ReferenceParser =
        match p with
        | True a -> true' a state
        | False -> false' "Hit false parser"
        | ConstantChar (c, teq) ->
            constantChar c state |> cast teq
        | Constant (s, teq) ->
            constant s state |> cast teq
        | Digit teq -> charSet System.Char.IsDigit state |> cast teq
        | Letter teq -> charSet System.Char.IsLetter state |> cast teq
        | LetterOrDigit teq -> charSet System.Char.IsLetterOrDigit state |> cast teq
        | AlignedBlock p ->
            makeParser p (state.Col - 1) context state
        | NewLine teq -> newLine indent state |> cast teq
        | Alternate ps ->
            ps |> List.map (fun p -> makeParser p indent context state) |> alternate
        | Map crate ->
            crate.Apply
                { new ParserMapCrateEvaluator<_,_> with
                    member __.Eval f p = map f (makeParser p indent context state)
                }
        | Apply crate ->
            crate.Apply
                { new ParserApplyCrateEvaluator<_,_> with
                    member __.Eval p1 p2 = apply (makeParser p1 indent context state) (makeParser p2 indent context)
                }
        | Delay f -> makeParser (f ()) indent context state
        | Filter (f, p) -> filter f (makeParser p indent context state)
        | Named (name, p) ->
            makeParser p indent (name::context) state

    let buildParser (p : 'a Parser) : string -> Result<'a, string> =

        let state = { Row = 1 ; Col = 1 }
        let p = makeParser p 0 [] state

        let rec eat (p : 'a ReferenceParser) (cs : char list) =
            match cs with
            | [] ->
                match p with
                | Done (Ok (_, a)) -> Result.Ok a
                | Done (Error s) -> Result.Error s
                | Running (_, Some (_, a)) -> Result.Ok a
                | Running (_, None) -> Result.Error "Parser was still running and unable to emit when we ran out of characters"
            | c::cs ->
                match p with
                | Done _ -> Result.Error "Parser had finished, despite unread characters remaining"
                | Running (f, _) -> eat (f c) cs

        fun s -> s |> List.ofSeq |> eat p

    let buildSeqParser (p : 'a Parser) : string -> ResultSeq<'a, string> =

        let rec eat p cs =
            match p with
            | Done (Ok sa) -> Result.Ok (sa, cs)
            | Done (Error s) -> Result.Error s
            | Running (_, Some _) -> Result.Error "Potentially ambiguous parse detected"
            | Running (f, None) ->
                match cs with
                | [] -> Result.Error "Parser was still running and unable to emit when we ran out of characters"
                | c::cs -> eat (f c) cs

        let rec inner state cs =
            fun () ->
                match cs with
                | [] -> ResultSeqInner.Done
                | _ ->
                    let p = makeParser p 0 [] state
                    match eat p cs with
                    | Ok ((state, a), cs) -> ResultSeqInner.Next (a, inner state cs)
                    | Error s -> ResultSeqInner.Error s
            |> ResultSeq.ResultSeq

        fun s ->
            let cs = s |> List.ofSeq
            let state = { Row = 1 ; Col = 1 }
            inner state cs
