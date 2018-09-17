namespace Parsy

[<RequireQualifiedAccess>]
module Result =

    let isOk = function Ok _ -> true | Error _ -> false

    let isError = function Ok _ -> false | Error _ -> true

    let getOk = function Ok v -> v | Error _ -> failwith "Could not get value for Error Result"
