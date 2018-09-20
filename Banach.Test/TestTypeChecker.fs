namespace Banach.Test

open Banach
open NUnit.Framework
open Parsy

[<TestFixture>]
module TestTypeChecker =

    let getParsed = TestUtils.getSource >> ReferenceParser.buildParser Untyped.Parser.make >> Result.getOk

    let tryTypeCheck (fileName : string) : unit =
        let definitions = getParsed fileName
        let typed = DefinitionsChecker.typeCheckDefinitions definitions
        typed |> List.choose (function Ok _ -> None | Error info -> Some info) |> List.iter (printfn "%A")
        typed |> List.forall Result.isOk |> Assert.IsTrue

    let expectFail f expectedMessage =
        let ex = Assert.Throws(fun () -> f ())
        Assert.AreEqual(expectedMessage, ex.Message)

    [<Test>]
    [<Explicit>]
    let ``TypeChecker can type check first examples`` () =
        tryTypeCheck "Test.ban"

    [<Test>]
    let ``TypeChecker can type check silly example`` () =
        tryTypeCheck "Silly.ban"

    [<Test>]
    let ``TypeChecker can type check the definition of List`` () =
        tryTypeCheck "List.ban"

    [<Test>]
    let ``TypeChecker can type check the definition of Void`` () =
        tryTypeCheck "Void.ban"

    [<Test>]
    [<Explicit>]
    let ``TypeChecker rejects a bad example`` () =

        let definitions = getParsed "Bad1.ban"
        let typeCheck () = DefinitionsChecker.typeCheckDefinitions definitions |> ignore
        let expectedMessage = "Unexpected return type in definition of foo. Excepted expression Bool.True to have type Nat, but it had type Bool."

        expectFail typeCheck expectedMessage

    [<Test>]
    [<Explicit>]
    let ``TypeChecker rejects a second bad example`` () =

        let definitions = getParsed "Bad2.ban"
        let typeCheck () = DefinitionsChecker.typeCheckDefinitions definitions |> ignore
        let expectedMessage = "Tried to apply expression Bool.True of type Bool to Nat.S of type (Nat) -> (Nat). Was expecting a Nat, but got a Bool."

        expectFail typeCheck expectedMessage
