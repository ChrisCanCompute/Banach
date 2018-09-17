namespace Banach.Untyped

open Parsy

module Parser =

    val definition : Definition Parser

    val make : Definition list Parser
