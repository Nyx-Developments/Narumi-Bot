namespace Narumi.Framework

open Discord.WebSocket

[<AutoOpen>]
module Types =
  type Message = SocketUserMessage

  /// Arguments to the command.
  /// They are either positional (comes in the correct order)
  /// or keyed (comes in the order of evaluation).
  ///
  /// Either way, it is very hard to pin down thing like mentions
  /// and everything. I will need to provide an Utils library for that,
  /// I guess.
  type Argument =
    | Positional of string
    | Keyed of string * string

  /// Arguments is a one-pass string parser that emits `Argument`.
  type Arguments = Arguments of string // Remaining

  /// Handler is a function that takes the original message,
  /// the argument stream and do something with it.
  type Handler = Message * Arguments -> Async<unit>

  /// Filter takes a command and checks whether the user can execute that command.
  /// If `None` is returned, the command can proceed (like returning `0`).
  /// If `Some(x)` is returned, then `x()` is called if not in a checking-only environment
  /// (e.g. collecting help commands).
  type Filter = Message * Arguments -> Async<Option<unit -> Async<unit>>>

  /// A command type. I hope I will have a better way to specify this.
  /// It has a description and some examples.
  ///
  /// The handler only has the arguments starting from the command's point of call.
  type NodeCommand =
    { /// Some description of the command.
      /// There are two values: the short and the long version.
      ///
      /// The short version is used in the context of help functions.
      /// It should be no more than one sentence.
      ///
      /// The long version is displayed upon calling `--help`.
      /// Keep it pretty short is the idea, but it can go as long as a paragraph if you want.
      description : string * string
      /// The command's handler function.
      handler : Handler }

  /// A route in the Router.
  type NodeRouterRoute =
    { /// Plug a node in here to get the pipes running.
      /// Filters should go in here as well, so that the router can decide if it is worth continuing.
      ///
      /// All *keyed* parameters before the command word is thrown away.
      node : Node
      /// A list of possible command names for the route.
      /// Note that this is matched **exact** (no prefixing), **case insensitive** and as a *positional*
      /// parameter.
      command : string list }

  /// The node that acts like a router.
  and NodeRouter =
    { /// The name of the router. Will be used as the "short" description on the help page
      /// of its parent.
      name : string
      /// A longer description that will be used in the full help.
      description : string
      /// The help function, takes the list of routes and returns a handler.
      helpFn : NodeRouter -> Handler
      /// The action that will be performed on a call.
      /// The default behaviour is to try to route (with `Router.route`) and upon failure,
      /// display `help`.
      defaultAction : NodeRouter -> Handler
      /// The list of routes.
      /// Routes are matched on the list's order, and are not "smartly" matched in any way.
      routes : NodeRouterRoute list }

  /// A Node represents a component in the Framework's flow.
  and Node =
    | Command of NodeCommand
    | Router of NodeRouter
    | Filter of Filter * Node
