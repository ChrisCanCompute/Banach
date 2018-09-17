namespace Banach.Untyped

open Banach
open Parsy

module Parser =

    let s = Parser.constant
    let c = Parser.constantChar

    let keywords =
        [
            "match"
            "with"
        ]
        |> Set.ofSeq

    let identifier : Identifier Parser =
        Parser.letter .>>. Parser.zeroOrMore Parser.letterOrDigit
        |> Parser.map (fun (c, cs) -> sprintf "%c%s" c (cs |> Array.ofList |> System.String))
        |> Parser.filter (fun s -> not <| Set.contains s keywords)
        |> Parser.map IIdentifier

    let qualifiedIdentifier : QualifiedIdentifier Parser =

        let makeQualified =
            function
            | [] -> failwith "Cannot make a qualifiedIdentifier out of zero identifiers"
            | [i] -> QIIdentifier i
            | [i1 ; i2] -> QIQualified (i1, i2)
            | _ -> failwith "Cannot make a qualifiedIdenfitier with more than two idenfitiers"

        identifier .>>. Parser.zeroOrMore (Parser.constantChar '.' >>. identifier)
        |> Parser.map List.Cons
        |> Parser.map makeQualified

    let pattern : Pattern Parser =
        qualifiedIdentifier .>>. Parser.zeroOrMore (c ' ' >>. identifier)
        |> Parser.map Pattern

    let expression : Expression Parser =

        fun expression ->

            let single =
                Parser.alternate
                    [
                        qualifiedIdentifier |> Parser.map EIdentifier
                        c '(' >>. expression .>> c ')'
                    ]

            let buildExpression (e, es) = List.fold (fun f a -> EApplication (f, a)) e es

            let applications =
                single
                .>>. Parser.zeroOrMore (c ' ' >>. single)
                |> Parser.map buildExpression

            let matchExpression =
                let matchLine = s "match " >>. expression .>> s " with"
                let caseLine = Parser.newLine >>. s "| " >>. pattern .>> s " -> " .>>. expression
                matchLine .>>. Parser.zeroOrMore caseLine |> Parser.map EMatch

            Parser.alternate
                [
                    applications
                    matchExpression
                ]

        |> Parser.fix

    let typeParser : Type Parser =

        fun typeParser ->

            let rec buildType =
                function
                | [], e -> TExpression e
                | (Some i, p)::ps, e -> TNamedParameter (i, p, buildType (ps, e))
                | (None, p)::ps, e -> TParameter (p, buildType (ps, e))

            let parameter =
                Parser.alternate
                    [
                        c '(' >>. identifier .>> s " : " .>>. typeParser .>> c ')' |> Parser.map (fun (i, e) -> Some i, e)
                        c '(' >>. typeParser .>> c ')' |> Parser.map (fun t -> None, t)
                        expression |> Parser.map (fun e -> None, TExpression e)
                    ]

            let parameters = Parser.zeroOrMore (parameter .>> s " -> ")

            parameters .>>. expression |> Parser.map buildType

        |> Parser.fix

    let typeDefinition : TypeDefinition Parser =

        let typeDeclaration =
            s "type " >>. identifier .>> s " : " .>>. typeParser .>> s " ="

        let constructorDefinition =
            s "| " >>. identifier .>> s " : " .>>. typeParser
            |> Parser.map (fun (i, t) -> { Name = i ; Type = t })

        typeDeclaration .>>. Parser.zeroOrMore (Parser.newLine >>. constructorDefinition)
        |> Parser.map (fun ((i, t), cs) -> { Name = i ; Type = t ; Constructors = cs })

    let valueDefinition : ValueDefinition Parser =

        let isRecursive =
            Parser.alternate
                [
                    Parser.true' false
                    s "rec " |> Parser.map (fun _ -> true)
                ]

        let parameter = c '(' >>. identifier .>> s " : " .>>. typeParser .>> c ')'
        let parameters = Parser.zeroOrMore (c ' ' >>. parameter)

        let body =
            Parser.alternate
                [
                    c ' ' >>. expression
                    Parser.newLine >>. s "    " >>. Parser.alignedBlock expression
                ]

        s "let "
        >>. isRecursive
        .>>. identifier
        .>>. parameters
        .>> s " : "
        .>>. typeParser
        .>> s " ="
        .>>. body
        |> Parser.map (fun ((((r, i), ps), t), e) -> { Recursive = r; Name = i ; Parameters = ps ; Type = t ; Body = e })

    let definition : Definition Parser =
        Parser.alternate
            [
                typeDefinition |> Parser.map DType
                valueDefinition |> Parser.map DValue
            ]

    let interDefinitionWhitespace : unit Parser =
        Parser.newLine
        |> Parser.oneOrMore
        |> Parser.map ignore

    let make : Definition list Parser =
        let maybe p = Parser.alternate [ Parser.true' None ; p |> Parser.map Some ]
        let firstDef = maybe definition |> Parser.map (function None -> [] | Some d -> [d])
        let defs = (interDefinitionWhitespace >>. definition) |> Parser.zeroOrMore
        let finalWs = maybe interDefinitionWhitespace
        firstDef .>>. defs .>> finalWs |> Parser.map (fun (ds1, ds2) -> ds1 @ ds2)
