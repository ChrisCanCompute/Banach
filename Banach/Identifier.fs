namespace Banach

type Identifier = IIdentifier of string
with
    override this.ToString () =
        match this with (IIdentifier name) -> name


type QualifiedIdentifier =
| QIIdentifier of Identifier
| QIQualified of Identifier * Identifier
with
    override this.ToString () =
        match this with
        | QIIdentifier i -> sprintf "%O" i
        | QIQualified (i1, i2) -> sprintf "%O.%O" i1 i2
