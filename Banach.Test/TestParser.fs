namespace Banach.Test

open Banach
open Banach.Untyped
open NUnit.Framework
open Parsy

[<TestFixture>]
module TestParser =

    [<Test>]
    let ``Parser can parse first examples`` () =
        let source = TestUtils.getSource "Test.ban"
        let parsed = ReferenceParser.buildParser Parser.make source
        Assert.IsTrue (parsed |> Result.isOk)
