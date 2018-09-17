namespace Parsy

[<RequireQualifiedAccess>]
module ReferenceParser =

    val buildParser : 'a Parser -> (string -> Result<'a, string>)

    val buildSeqParser : 'a Parser -> (string -> ResultSeq<'a, string>)
