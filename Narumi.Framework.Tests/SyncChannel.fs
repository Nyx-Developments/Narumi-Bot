namespace Tests

open NUnit.Framework
open FsUnit
open Narumi.Framework.Concurrency

[<TestFixture>]
type SyncChannel() =
  let double = (*) 2
  let asyncify fn x = async { return fn x }

  /// Create a stateless channel
  let statelessChannel fn =
    Channel((), fun () x ->
              async { let! result = fn x
                      return ((), result) })

  [<Test>]
  member this.``Should work well with stateless objects``() =
    let original = [ 1..100 ]

    let asyn =
      async {
        let doubleChannel =
          double
          |> asyncify
          |> statelessChannel
        let! resultList = original
                          |> List.map doubleChannel.Post
                          |> Async.Parallel
        return Array.toList resultList
      }
    Async.RunSynchronously asyn
    |> List.zip (original |> List.map double)
    |> List.iter (fun (a, b) -> a |> should equal b)

  [<Test>]
  member this.``Should work well with stateful objects``() =
    let handler sum x = async { return (sum + x, sum + x) }
    let original = [ 1..10 ]

    let asyn =
      async {
        let channel = Channel(0, handler)
        let! resultList = original
                          |> List.map channel.Post
                          |> Async.Parallel
        return Array.toList resultList
      }
    Async.RunSynchronously asyn
    |> List.zip (original
                 |> List.scan (+) 0
                 |> List.tail)
    |> List.iter (fun (a, b) -> a |> should equal b)
