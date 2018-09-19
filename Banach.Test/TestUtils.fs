namespace Banach.Test

open NUnit.Framework
open System.IO

[<RequireQualifiedAccess>]
module TestUtils =

    let getSource fileName =
        let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly().Location |> FileInfo
        let dir = thisAssembly.Directory.GetDirectories "TestData" |> Seq.exactlyOne
        match dir.GetFiles fileName |> Seq.tryHead with
        | Some file -> File.ReadAllText file.FullName
        | None -> failwithf "Could not find file %s in the TestData directory." fileName

    let assertEqual (expected : 'a) (actual : 'a) =
        Assert.AreEqual(expected, actual)
