namespace Tests

open NUnit.Framework
open FsUnit
open Narumi.Bot.Commands.Quiz

[<TestFixture>]
type Quiz() =

  let sampleQuiz host : Service.Content =
    { ID = 1
      value = 100
      question = "A question"
      answer = "D"
      host = host }

  let staticProvider returnSome host : Async<Service.Content option> =
    async {
      if returnSome then return Some(sampleQuiz host)
      else return None
    }

  let create() = Service.create 1 (staticProvider true)

  let createQueries channel host announceFn =
    let queries =
      [ Service.New(host, announceFn)
        Service.Answer(1UL, "A")
        Service.Answer(2UL, "A")
        Service.New(host, announceFn)
        Service.Answer(3UL, "d")
        Service.Answer(2UL, "B") ]
      |> List.map (fun x -> (channel, x))

    let q = sampleQuiz host

    let answers =
      [ Service.NewQuiz(Some q)
        Service.WrongAnswer
        Service.WrongAnswer
        Service.QuizAlreadyExists q
        Service.CorrectAnswer
        Service.Ignore ]
    (queries, answers)

  let verify (channel : Service.ChannelType) queries answers =
    let sequential lst =
      let rec fold sum lst =
        async {
          match lst with
          | [] -> return (List.rev sum)
          | x :: rest ->
            let! res = x
            return! fold (res :: sum) rest
        }
      fold [] lst
    async {
      let! responses = queries
                       |> List.map channel.Post
                       |> sequential
      responses
      |> List.zip answers
      |> List.iter (fun (a, b) -> a |> should equal b)
    }

  [<Test>]
  member this.``Should handle a simple game``() =
    let host = "simple host"
    let q = sampleQuiz host

    let announceFn quiz winner =
      eprintfn "%A %A" quiz winner
      quiz |> should equal q
      winner |> should equal (Some 3UL)
      async { return () }

    let (queries, answers) = createQueries 1UL host announceFn
    Async.RunSynchronously <| verify (create()) queries answers
    Async.RunSynchronously <| Async.Sleep 2000

  [<Test>]
  member this.``Should handle multiple games``() =
    let host = "simple host"
    let q = sampleQuiz host

    let announceFn quiz winner =
      eprintfn "%A %A" quiz winner
      quiz |> should equal q
      winner |> should equal (Some 3UL)
      async { return () }

    let transposePairs lst = (lst |> List.map fst, lst |> List.map snd)

    let rec intertwine lst =
      let head lst = lst |> List.map List.head
      let tail lst = lst |> List.map List.tail
      match lst with
      | w when List.forall (not << List.isEmpty) w ->
        List.append (head w) (intertwine (tail lst))
      | w when List.forall List.isEmpty w -> []
      | _ -> failwith "Lists must have the same size"

    let (queries, answers) =
      [ 1UL..20UL ]
      |> List.map (fun x -> createQueries x host announceFn)
      |> transposePairs
      |> fun (a, b) -> (intertwine a, intertwine b)

    Async.RunSynchronously <| verify (create()) queries answers
    Async.RunSynchronously <| Async.Sleep 2000

  [<Test>]
  member this.``Should gracefully handle failed providers``() =
    let service = Service.create 5 (staticProvider false)
    let ign _ _ = async { return () }

    let queries =
      [ Service.New("host", ign)
        Service.Answer(0UL, "")
        Service.New("host", ign) ]
      |> List.map (fun x -> (0UL, x))

    let answers =
      [ Service.NewQuiz None
        Service.Ignore
        Service.NewQuiz None ]

    Async.RunSynchronously <| verify service queries answers

  [<Test>]
  member this.``Should properly timeout on no correct answers``() =
    let service = create()
    let q = sampleQuiz "new host"

    let announceFn quiz winner =
      eprintfn "%A %A" quiz winner
      quiz |> should equal q
      winner |> should equal None
      async { return () }

    let queries = [ (1UL, (Service.New("new host", announceFn))) ]
    let answers = [ Service.NewQuiz(Some q) ]
    Async.RunSynchronously <| verify service queries answers
    Async.RunSynchronously <| Async.Sleep 2000
