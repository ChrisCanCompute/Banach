namespace Parsy.Test

open NUnit.Framework
open Parsy

[<TestFixture>]
module TestReferenceParser =

    let aParser = Parser.constantChar 'a'
    let bParser = Parser.constantChar 'b'
    let cParser = Parser.constantChar 'c'

    let altParser = Parser.alternate [ aParser ; bParser ]
    let seqParser = Parser.sequence aParser bParser
    let zeroOrMoreAParser = Parser.zeroOrMore aParser
    let oneOrMoreAParser = Parser.oneOrMore aParser

    let identifierParser =
        ['a'..'z'] @ ['A'..'Z']
        |> List.map Parser.constantChar
        |> Parser.alternate
        |> Parser.oneOrMore
        |> Parser.map (Array.ofList >> System.String)

    let abParser () =

        fun abParser ->
            let combineParsed ((a, ab), b) = sprintf "%c%s%c" a ab b
            let inner = aParser .>>. abParser .>>. bParser |> Parser.map combineParsed
            Parser.true' "" ||| inner
        |> Parser.fix

    let alignedBlockParser =
        let oneOrMore = Parser.constantChar >> Parser.oneOrMore >> Parser.map List.length
        let lineOfBs =
            Parser.constantChar 'b' >>. Parser.zeroOrMore (Parser.newLine >>. Parser.constantChar 'b')
            |> Parser.map (fun bs -> List.length bs + 1)
        oneOrMore 'a' .>>. Parser.alignedBlock lineOfBs .>>. oneOrMore 'c'

    let assertEqual (expected : 'a) (actual : 'a) (message : string) =
        Assert.AreEqual(expected, actual, message)

    let assertParse parser input expected =
        assertEqual (Ok expected) (ReferenceParser.buildParser parser input)

    let assertNoParse parser input =
        assertEqual true (ReferenceParser.buildParser parser input |> Result.isError)

    let testCases =
        [
            "constantChar successfully parses a match", assertParse cParser "c" 'c'
            "constantChar does not parse a mismatch", assertNoParse cParser "k"
            "alternate parses using its first given parser", assertParse altParser "a" 'a'
            "alternate parses using its second given parser", assertParse altParser "b" 'b'
            "alternate does not parse something that doesn't match its parsers", assertNoParse altParser "c"
            "sequence parses a sequence of two characters", assertParse seqParser "ab" ('a', 'b')
            "zeroOrMore parses the empty string", assertParse zeroOrMoreAParser "" []
            "zeroOrMore parses one character", assertParse zeroOrMoreAParser "a" ['a']
            "zeroOrMore parses two characters", assertParse zeroOrMoreAParser "aa" ['a' ; 'a']
            "zeroOrMore parses three characters", assertParse zeroOrMoreAParser "aaa" ['a' ; 'a' ; 'a']
            "oneOrMore does not parse the empty string", assertNoParse oneOrMoreAParser ""
            "oneOrMore parses one character", assertParse oneOrMoreAParser "a" ['a']
            "oneOrMore parses two characters", assertParse oneOrMoreAParser "aa" ['a' ; 'a']
            "oneOrMore parses three characters", assertParse oneOrMoreAParser "aaa" ['a' ; 'a' ; 'a']
            "identifierParser parses an identifier", assertParse identifierParser "foodle" "foodle"
            "abParser parses the empty string", assertParse (abParser ()) "" ""
            "abParser parses the ab", assertParse (abParser ()) "ab" "ab"
            "abParser parses the aabb", assertParse (abParser ()) "aabb" "aabb"
            "abParser does not parse abab", assertNoParse (abParser ()) "abab"
            "aligned block parser parses abc", assertParse alignedBlockParser "abc" ((1, 1), 1)
            "aligned block parser parses aligned example", assertParse alignedBlockParser "ab\r\n bc" ((1, 2), 1)
            "aligned block parser parses aligned example", assertParse alignedBlockParser "ab\r\n b\r\n bc" ((1, 3), 1)
        ]

    [<Test>]
    let ``Test ReferenceParser`` ([<Range(0, 21)>] testCase) =
        let name, test = testCases |> List.item testCase
        test name

    let list p = p .>>. Parser.zeroOrMore (Parser.newLine >>. p) |> Parser.map List.Cons

    let v c = list (Parser.constantChar c) |> Parser.map (List.length)
    let h c = Parser.oneOrMore (Parser.constantChar c) |> Parser.map List.length

    let abcParser = h 'a' .>>. Parser.alignedBlock (v 'b' .>>. h 'c') |> Parser.map (fun (a, (b, c)) -> a, b, c)

    let abcInput = "aaab\r\n   b\r\n   bccc"

    let abcdeParser =
        h 'a' .>>. Parser.alignedBlock (v 'b' .>>. h 'c' .>>. Parser.alignedBlock (v 'd' .>>. h 'e'))
        |> Parser.map (fun (a, ((b, c), (d, e))) -> a, b, c, d, e)

    let abcdeInput = "aaab\r\n   b\r\n   bcccd\r\n       d\r\n       deee"

    let testList (p : 'a Parser) (input : string) (parsed : 'a) (n : int) =
        let listParser = list p
        let listInput = Seq.init n (fun _ -> input) |> String.concat "\r\n"
        let expected = List.init n (fun _ -> parsed)
        assertParse listParser listInput expected "testList"

    [<Test>]
    let ``Test abc aligned block`` () =
        assertParse abcParser abcInput (3, 3, 3) "abc parser"

    [<Test>]
    let ``Test repeated abc aligned block`` () =
        [1..10] |> List.iter (testList abcParser abcInput (3, 3, 3))

    [<Test>]
    let ``Test abcde aligned block`` () =
        assertParse abcdeParser abcdeInput (3, 3, 3, 3, 3) "abced parser"

    [<Test>]
    let ``Test repeated abcde aligned block`` () =
        [1..10] |> List.iter (testList abcdeParser abcdeInput (3, 3, 3, 3, 3))

    [<Test>]
    let ``Test seqParser`` () =
        let aParser = Parser.constantChar 'a' |> ReferenceParser.buildSeqParser
        let input = "aaaab"
        let actual = aParser input |> ResultSeq.toResultList
        let expected = ResultList.ofList [ 'a' ; 'a' ; 'a' ; 'a' ] (Some "b did not match a")
        assertEqual expected actual "seqParser"
