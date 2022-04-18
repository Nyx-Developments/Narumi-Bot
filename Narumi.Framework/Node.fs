namespace Narumi.Framework

open Discord.WebSocket

module Node =
  /// **Description**
  ///
  /// Execute the given `Node`.
  ///
  /// **Parameters**
  ///   * `msg` - parameter of type `Message`
  ///   * `args` - parameter of type `Arguments`
  ///   * `node` - parameter of type `Node`
  ///
  /// **Output Type**
  ///   * `Async<unit>`
  ///
  /// **Exceptions**
  ///
  let rec execute v node =
    async {
      match node with
      | Filter(f, next) ->
        match! f v with
        | None -> return! execute v next
        | Some(h) -> return! h()
      | Command(c) -> return! c.handler v
      | Router(r) -> return! Router.route execute v r
    }

  /// **Description**
  ///
  /// Handles a `messageReceived` action.
  ///
  /// This filters away all messages from bot, and requires the message to have a certain prefix.
  ///
  /// **Parameters**
  ///   * `node` - parameter of type `Node`
  ///   * `msg` - parameter of type `SocketMessage`
  ///
  /// **Output Type**
  ///   * `Async<unit>`
  ///
  /// **Exceptions**
  ///
  let handleMessage prefix node (msg : SocketMessage) =
    match msg with
    | :? SocketUserMessage as msg ->
      async {
        if (not msg.Author.IsBot) && msg.Content.StartsWith prefix then
          let args = Arguments(msg.Content.Substring prefix.Length)
          return! execute (msg, args) node
      }
    | _ -> async { return () } // Do nothing
