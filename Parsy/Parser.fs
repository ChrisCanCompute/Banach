namespace Parsy

type 'a Parser =
    internal
    | True of 'a
    | False
    | ConstantChar of char * Teq<'a, char>
    | Constant of string * Teq<'a, string>
    | Digit of Teq<'a, char>
    | Letter of Teq<'a, char>
    | LetterOrDigit of Teq<'a, char>
    | AlignedBlock of 'a Parser
    | NewLine of Teq<'a, string>
    | Alternate of 'a Parser list
    | Map of 'a ParserMapCrate
    | Apply of 'a ParserApplyCrate
    | Delay of (unit -> 'a Parser)
    | Filter of ('a -> bool) * 'a Parser
    | Named of string * 'a Parser

and 'a ParserMapCrate = abstract member Apply : ParserMapCrateEvaluator<'a, 'ret> -> 'ret
and ParserMapCrateEvaluator<'a, 'ret> = abstract member Eval : ('b -> 'a) -> 'b Parser -> 'ret

and 'a ParserApplyCrate = abstract member Apply : ParserApplyCrateEvaluator<'a, 'ret> -> 'ret
and ParserApplyCrateEvaluator<'a, 'ret> = abstract member Eval : ('b -> 'a) Parser -> 'b Parser -> 'ret


module Parser =

    let true' a = True a

    let false' = False

    let constantChar c = ConstantChar (c, Teq.refl)

    let constant s = Constant (s, Teq.refl)

    let digit = Digit Teq.refl

    let letter = Letter Teq.refl

    let letterOrDigit = LetterOrDigit Teq.refl

    let alignedBlock p = AlignedBlock p

    let newLine = NewLine Teq.refl

    let alternate ps = Alternate ps

    let map f p =
        { new ParserMapCrate<_> with
            member __.Apply e = e.Eval f p
        }
        |> Map

    let apply p1 p2 =
        { new ParserApplyCrate<_> with
            member __.Apply e = e.Eval p1 p2
        }
        |> Apply

    let sequence p1 p2 =
        apply (map (fun a b -> a, b) p1) p2

    let rec fix (f : 'a Parser -> 'a Parser) : 'a Parser =
        f (Delay (fun () -> fix f))

    let zeroOrMore p =
        fun zeroOrMore ->
            alternate [ true' [] ; sequence p zeroOrMore |> map List.Cons ]
        |> fix

    let oneOrMore p =
        sequence p (zeroOrMore p) |> map List.Cons

    let filter f p = Filter (f, p)

    let name name p = Named (name, p)

    let rec printInner<'a> (p : 'a Parser) : (int * string) list =

        let line s = [ 0, s ]
        let indent lines = lines |> List.map (fun (i, s) -> i + 1, s)

        match p with
        | True a -> sprintf "True<%s> %+A" (Type.print<'a>) a |> line
        | False -> sprintf "False<%s>" (Type.print<'a>) |> line
        | ConstantChar (c, teq) -> sprintf "ConstantChar '%c'" c |> line
        | Constant (s, teq) -> sprintf "Constant \"%s\"" s |> line
        | Digit teq -> "Digit" |> line
        | Letter teq -> "Letter" |> line
        | LetterOrDigit teq -> "LetterOrDigit" |> line
        | AlignedBlock p -> failwith "AlignedBlock"
        | NewLine teq -> "NewLine" |> line
        | Alternate ps ->
            (0, "Alternate")::(ps |> List.collect printInner |> indent)
        | Map crate ->
            crate.Apply
                { new ParserMapCrateEvaluator<_,_> with
                    member __.Eval (f : 'b -> 'a) p =
                        let caption = sprintf "Map<%s>" (Type.print<'b -> 'a>)
                        (0, caption)::(p |> printInner |> indent)
                }
        | Apply crate ->
            crate.Apply
                { new ParserApplyCrateEvaluator<_,_> with
                    member __.Eval (p1 : ('c -> 'a) Parser) p2 =
                        let caption = sprintf "Apply<%s>" (Type.print<'c -> 'a>)
                        (0, caption)::((printInner p1)@(printInner p2) |> indent)
                }
        | Delay f -> sprintf "Delay<%s>" (Type.print<'a>) |> line
        | Filter (f, p) ->
            let caption = sprintf "Filter<%s>" (Type.print<'a>)
            (0, caption)::(p |> printInner |> indent)
        | Named (name, p) ->
            let caption = sprintf "Named<%s> %s" (Type.print<'a>) name
            (0, caption)::(p |> printInner |> indent)

    let print (p : 'a Parser) : string =
        printInner p
        |> List.map (fun (indent, s) -> sprintf "%s%s" (System.String(' ', indent * 2)) s)
        |> String.concat System.Environment.NewLine

[<AutoOpen>]
module ParserOperators =

    let (.>>) (p1 : 'a Parser) (p2 : 'b Parser) : 'a Parser =
        Parser.sequence p1 p2 |> Parser.map fst

    let (>>.) (p1 : 'a Parser) (p2 : 'b Parser) : 'b Parser =
        Parser.sequence p1 p2 |> Parser.map snd

    let (.>>.) (p1 : 'a Parser) (p2 : 'b Parser) : ('a * 'b) Parser =
        Parser.sequence p1 p2

    let (|||) (p1 : 'a Parser) (p2 : 'a Parser) : 'a Parser =
        Parser.alternate [ p1 ; p2 ]
