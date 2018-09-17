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

    val true' : 'a -> 'a Parser

    val false' : 'a Parser

    val constantChar : char -> char Parser

    val constant : string -> string Parser

    val digit : char Parser

    val letter : char Parser

    val letterOrDigit : char Parser

    val alignedBlock : 'a Parser -> 'a Parser

    val newLine : string Parser

    val alternate : 'a Parser list -> 'a Parser

    val map : ('a -> 'b) -> 'a Parser -> 'b Parser

    val apply : ('a -> 'b) Parser -> 'a Parser -> 'b Parser

    val sequence : 'a Parser -> 'b Parser -> ('a * 'b) Parser

    val fix : ('a Parser -> 'a Parser) -> 'a Parser

    val zeroOrMore : 'a Parser -> 'a list Parser

    val oneOrMore : 'a Parser -> 'a list Parser

    val filter : ('a -> bool) -> 'a Parser -> 'a Parser

    val name : string -> 'a Parser -> 'a Parser

    val print : 'a Parser -> string


[<AutoOpen>]
module ParserOperators =

    val (.>>) : 'a Parser -> 'b Parser -> 'a Parser

    val (>>.) : 'a Parser -> 'b Parser -> 'b Parser

    val (.>>.) : 'a Parser -> 'b Parser -> ('a * 'b) Parser

    val (|||)  : 'a Parser -> 'a Parser -> 'a Parser
