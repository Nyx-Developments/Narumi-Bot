// Learn more about F# at http://fsharp.org
open Narumi
open System
open Discord.WebSocket
open Discord
open Narumi.Framework
open Narumi.Bot.Commands

let asyncCatchLog asyn =
  let catcher =
    async {
      match! Async.Catch asyn with
      | Choice1Of2 _ -> return ()
      | Choice2Of2 exn -> eprintfn "%A" exn
    }
  Async.Start catcher

let rootRouter : Node =
  Router { name = "Narumi Bot"
           description =
             "An experimental bot in F#.\n\nhttps://gitlab.com/natsukagami/narumi-bot"
           routes =
             [ Router.newRoute (Command.create (fun (msg : Message, _) ->
                                  async {
                                    do! msg.Channel.SendMessageAsync "pong!"
                                        |> Async.AwaitTask
                                        |> Async.Ignore
                                  }) "Ping" "You ping, I pong...") [ "ping" ] ]
           helpFn = Router.defaultHelpFn
           defaultAction = Router.defaultAction }

let mainAsync =
  async {
    let client = new DiscordSocketClient()
    let events = Framework.Client.events client
    // Ready event
    events.Ready
    |> Observable.map (fun () -> System.Console.WriteLine("Ready"))
    |> Observable.subscribe id
    |> ignore
    // Message event
    events.MessageReceived
    |> Observable.map (rootRouter |> Node.handleMessage "n?")
    |> Observable.subscribe asyncCatchLog
    |> ignore
    events.MessageReceived
    |> Observable.map
         (Quiz.Route.create client 60 Quiz.Provider.fromJService
          |> Node.handleMessage "n!")
    |> Observable.subscribe asyncCatchLog
    |> ignore
    do! Async.AwaitTask
          (client.LoginAsync
             (TokenType.Bot, Environment.GetEnvironmentVariable("TOKEN")))
    do! Async.AwaitTask(client.StartAsync())
    do! Async.Sleep(-1)
  }

[<EntryPoint>]
let main argv =
  Async.RunSynchronously mainAsync
  0 // return an integer exit code
