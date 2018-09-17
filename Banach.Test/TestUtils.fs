namespace Banach.Test

open NUnit.Framework
open System.IO

[<RequireQualifiedAccess>]
module TestUtils =

    let getSource fileName =
        let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly().Location |> FileInfo
        let dir = thisAssembly.Directory
        let file = dir.GetFiles fileName |> Seq.exactlyOne
        File.ReadAllText file.FullName

    let assertEqual (expected : 'a) (actual : 'a) =
        Assert.AreEqual(expected, actual)
