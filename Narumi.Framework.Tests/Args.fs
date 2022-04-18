namespace Tests

open NUnit.Framework
open FsUnit
open Narumi.Framework

[<TestFixture>]
type Args() =

  let listCompare (a : 'a list) (b : 'a list) =
    a
    |> List.zip b
    |> List.map (fun (x, y) -> x |> should equal y)
    |> ignore

  [<SetUp>]
  member this.Setup() = ()

  [<Test>]
  member this.``Parsing simple positional arguments``() =
    let args = Arguments "simple three words"
    let argLst = Seq.unfold Args.next args |> Seq.toList
    argLst
    |> listCompare ([ "simple"; "three"; "words" ] |> List.map Positional)

  [<Test>]
  member this.``Parsing arguments with symbols``() =
    let args = Arguments "!sim@ple $three \\^&words"
    let argLst = Seq.unfold Args.next args |> Seq.toList
    argLst
    |> listCompare
         ([ "!sim@ple"; "$three"; "\\^&words" ] |> List.map Positional)

  [<Test>]
  member this.``Parsing string-ed arguments``() =
    let args =
      Arguments "\"catch all of this, mofos UWU\" and \"something else\""
    let argLst = Seq.unfold Args.next args |> Seq.toList
    argLst
    |> listCompare
         ([ "catch all of this, mofos UWU"; "and"; "something else" ]
          |> List.map Positional)

  [<Test>]
  member this.``Parsing full key arguments``() =
    let args =
      Arguments "--simple-key value1 --simple-key-2 \"string-ed values\""
    let argLst = Seq.unfold Args.next args |> Seq.toList
    argLst
    |> listCompare ([ ("simple-key", "value1")
                      ("simple-key-2", "string-ed values") ]
                    |> List.map Keyed)

  [<Test>]
  member this.``Mixed arguments with emojis``() =
    let args = Arguments "⌛ --⌛⌛ ⌛⌛⌛ -⌛\"⌛aa⌛\" -⌛sim⌛ple ⌛⌛⌛"
    let argLst = Seq.unfold Args.next args |> Seq.toList
    argLst
    |> listCompare ([ Positional "⌛"
                      Keyed("⌛⌛", "⌛⌛⌛")
                      Keyed("⌛", "⌛aa⌛")
                      Keyed("⌛", "sim⌛ple")
                      Positional "⌛⌛⌛" ])

  [<Test>]
  member this.``Take some arguments and have rest``() =
    let args =
      Arguments
        "oneArg! --some-keyed keyed-value \"another arg\"\t\tand the rest of the args"
    let (argLst, args) = Args.take 3 args
    let args = Args.rest args
    argLst
    |> listCompare ([ Positional "oneArg!"
                      Keyed("some-keyed", "keyed-value")
                      Positional "another arg" ])
    args |> should equal "and the rest of the args"
