namespace Narumi.Bot.Commands.Quiz

open Narumi.Framework
open Narumi.Framework.Concurrency
open Discord.WebSocket
open Discord
open FSharp.Data

/// The quiz service, implemented independently from Discord code
module Service =
  /// The key used in the map
  type Key = uint64

  /// The question content
  type Content =
    { ID : int
      value : int
      question : string
      answer : string
      host : (* We only need a mention string*) string }

  /// The function used to announce the winner (Some winnerID) or it timed out (None).
  type AnnounceFn = Content -> uint64 option -> Async<unit>

  /// The data used is a map of `Key` and `Content`
  type Data = Map<Key, Content * AnnounceFn>

  /// The query question
  type QueryQuestion =
    | New of string * AnnounceFn // A new quiz request, with winner announcing function
    | Answer of uint64 * string // An answer, consists of playerID * answer
    | Timeout of int // Timeout notification, with content ID

  /// Format of a query
  type Query = Key * QueryQuestion

  /// Format of an answer
  type Answer =
    | QuizAlreadyExists of Content
    | NewQuiz of Content option // Might be an error and not have a quiz at all
    | WrongAnswer
    | CorrectAnswer
    | Ignore // Should not reply anything

  type ChannelType = Channel<Data, Query, Answer>

  /// **Description**
  ///
  /// Create a new Quiz module
  ///
  /// **Parameters**
  ///   * `timeout` - parameter of type `int`
  ///   * `generator` - parameter of type `string -> Async<Content option>`
  ///
  /// **Output Type**
  ///   * `Channel<Data,Query,Answer>`
  ///
  /// **Exceptions**
  ///
  let create timeout generator =
    /// Handler handles each request, serves as the input for Channel
    let handler (inbox : ChannelType) map (channel, question) =
      let handleAnswer user (answer : string) =
        async {
          match map |> Map.tryFind channel with
          | Some(q, announceFn) ->
            if answer.ToLower() = q.answer.ToLower() then // correct answer
              do! announceFn q (Some user) // Announce the winner
              return (map |> Map.remove channel, CorrectAnswer)
            else return (map, WrongAnswer)
          | None -> return (map, Ignore)
        }

      let handleNew host announceFn =
        let watchTimeout contentID =
          async {
            if timeout < 0 then return ()
            do! Async.Sleep(timeout * 1000)
            inbox.Send(channel, Timeout contentID)
          }
        async {
          match map |> Map.tryFind channel with
          | Some(q, _) -> return (map, QuizAlreadyExists q)
          | None ->
            match! generator host with
            | Some q ->
              Async.Start <| watchTimeout q.ID
              return (map |> Map.add channel (q, announceFn), NewQuiz(Some q))
            | None -> return (map, NewQuiz None)
        }

      let handleTimeout contentID =
        async {
          match map |> Map.tryFind channel with
          | Some(q, announceFn) when q.ID = contentID ->
            do! announceFn q None
            return (map |> Map.remove channel, Ignore)
          | _ -> return (map, Ignore)
        }

      match question with
      | New(host, announceFn) -> handleNew host announceFn
      | Answer(user, answer) -> handleAnswer user answer
      | Timeout(id) -> handleTimeout id
    ChannelType(Map.empty, handler)

/// The Discord-related part
module Route =
  open Service

  let private quizEmbed (quiz : Content) reveal =
    let answerPlaceholder =
      if reveal then sprintf "The answer was **%s**" quiz.answer
      else
        quiz.answer.ToCharArray()
        |> Array.map ((fun x ->
                      if System.Char.IsWhiteSpace(x) then "⬛"
                      else "⬜")
                      >> string)
        |> String.concat ""
    EmbedBuilder().WithAuthor("Narumi's trivia!")
      .WithTitle(sprintf "A $%d question!" quiz.value)
      .WithDescription(answerPlaceholder + "\n\n" + quiz.question)
      .WithFooter(sprintf "Requested by %s" (quiz.host.ToString()))
      .WithThumbnailUrl("https://i.kym-cdn.com/photos/images/newsfeed/001/303/252/9c2.gif")
      .Build()

  let private announceWinner (client : DiscordSocketClient)
      (channel : ISocketMessageChannel) quiz winner =
    async {
      let winnerAnnouncement =
        match winner with
        | Some(userID) ->
          let user = client.GetUser(userID)
          sprintf "%s was the winner!" user.Mention
        | None -> "no one found the answer..."
      do! channel.SendMessageAsync
            ("The quiz ended, and " + winnerAnnouncement,
             embed = quizEmbed quiz true)
          |> Async.AwaitTask
          |> Async.Ignore
    }

  let private respond (message : Message) answer =
    async {
      let channel = message.Channel
      let mention = message.Author.Mention
      let doTask = Async.AwaitTask >> Async.Ignore
      match answer with
      | Ignore -> return ()
      | CorrectAnswer -> return ()
      | WrongAnswer ->
        do! channel.SendMessageAsync(sprintf "%s, wrong answer!" mention)
            |> doTask
      | NewQuiz(Some q) ->
        do! channel.SendMessageAsync
              (sprintf "%s, new quiz for you!" mention,
               embed = quizEmbed q false) |> doTask
      | NewQuiz(None) ->
        do! channel.SendMessageAsync
              (sprintf
                 "%s, an error has occured while looking for quizzes, please try again..."
                 mention) |> doTask
      | QuizAlreadyExists(q) ->
        do! channel.SendMessageAsync
              (sprintf "%s, There is an ongoing quiz!" mention,
               embed = quizEmbed q false) |> doTask
    }

  /// **Description**
  ///
  /// Create a new Router that holds the quiz.
  ///
  /// **Parameters**
  ///   * `client` - parameter of type `DiscordSocketClient`
  ///   * `timeout` - parameter of type `int`
  ///   * `provider` - parameter of type `string -> Async<Content option>`
  ///
  /// **Output Type**
  ///   * `Node`
  ///
  /// **Exceptions**
  ///
  let create client timeout provider =
    let inbox = Service.create timeout provider
    let query (message : Message) question =
      async { let! answer = inbox.Post(message.Channel.Id, question)
              do! respond message answer }

    let commandNew =
      let handler : Handler =
        fun (msg, _) ->
          query msg
            (New(msg.Author.ToString(), announceWinner client msg.Channel))
      Command.create handler "Create a new quiz." "Create a new quiz."

    let commandAnswer =
      let handler : Handler =
        fun (msg, args) -> query msg (Answer(msg.Author.Id, Args.rest args))
      Command.create handler "Answer the current quiz."
        "Answer the current quiz."

    Router <| { name = "Quiz Module"
                description = "Have fun with Trivia quizzes!"
                routes =
                  [ Router.newRoute commandNew [ "new"; "n"; "+" ]
                    Router.newRoute commandAnswer [ "answer"; "a"; "!"; ">" ] ]
                defaultAction = Router.defaultAction
                helpFn = Router.defaultHelpFn }

module Provider =
  [<Literal>]
  let private URL = "http://jservice.io/api/random?count=25"

  type private QuizParser = JsonProvider<"""
[{"id":123416,"answer":"Oscar","question":"I'd like to thank the Motion Picture Academy for this letter","value":200,"airdate":"2014-06-11T12:00:00.000Z","created_at":"2015-01-18T18:10:10.966Z","updated_at":"2015-01-18T18:10:10.966Z","category_id":16931,"game_id":null,"invalid_count":null,"category":{"id":16931,"title":"the nato phonetic alphabet","created_at":"2015-01-18T18:10:10.749Z","updated_at":"2015-01-18T18:10:10.749Z","clues_count":10}},
{"id":123416,"answer":"Oscar","question":"I'd like to thank the Motion Picture Academy for this letter","value":200,"airdate":"2014-06-11T12:00:00.000Z","created_at":"2015-01-18T18:10:10.966Z","updated_at":"2015-01-18T18:10:10.966Z","category_id":16931,"game_id":null,"invalid_count":10,"category":{"id":16931,"title":"the nato phonetic alphabet","created_at":"2015-01-18T18:10:10.749Z","updated_at":"2015-01-18T18:10:10.749Z","clues_count":10}}]
""">

  /// **Description**
  ///
  /// Try to fetch a quiz from JService.
  ///
  /// **Parameters**
  ///   * `host` - parameter of type `string`
  ///
  /// **Output Type**
  ///   * `Async<Service.Content option>`
  ///
  /// **Exceptions**
  ///
  let fromJService (host : string) : Async<Service.Content option> =
    async {
      let! resp = Http.AsyncRequestString URL
      return resp
             |> QuizParser.Parse
             |> Array.filter (fun x -> x.InvalidCount.IsNone)
             |> Array.tryPick (fun x ->
                  try
                    Some { ID = x.Id
                           value = x.Value
                           question = x.Question
                           answer = x.Answer
                           host = host }
                  with _ -> None)
    }
