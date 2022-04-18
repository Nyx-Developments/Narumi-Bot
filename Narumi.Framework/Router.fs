namespace Narumi.Framework

open Discord

module Router =
  /// Resolves the node to a Command or Router node, if the user
  /// is allowed to access them.
  let rec private resolveNode v (route : Node) =
    async {
      match route with
      | Command(_)
      | Router(_) -> return Some(route)
      | Filter(p, next) ->
        let! p = p v
        match p with
        | None -> return! resolveNode v next
        | _ -> return None
    }

  /// Create the list of available commands for a single Router.
  let private availableCommands v (router : NodeRouter) =
    async {
      let! routes = router.routes
                    |> List.map
                         (fun r ->
                         async
                           { let! node = resolveNode v r.node
                             return node |> Option.map (fun route -> (r, route)) })
                    |> Async.Parallel
      return routes |> Array.Parallel.choose id
    }

  /// **Description**
  ///
  /// Generates the default help embed for the Router.
  ///
  /// **Parameters**
  ///   * `router` - parameter of type `NodeRouter`
  ///   * `msg` - parameter of type `Message`
  ///   * `args` - parameter of type `Arguments`
  ///
  /// **Output Type**
  ///   * `Async<EmbedBuilder>`
  ///
  /// **Exceptions**
  ///
  let defaultHelpEmbed router v =
    async {
      let embed = EmbedBuilder()
      embed.Title <- sprintf "%s - Help" router.name
      embed.Description <- router.description
      // Perform a check to fetch all possible commands
      let! routes = availableCommands v router
      // Format the help commands
      let! routes = routes
                    |> Array.map (fun (route, node) ->
                         async {
                           let embedField = EmbedFieldBuilder()
                           match node with
                           | Command(c) ->
                             return embedField.WithName("Command "
                                                        + (route.command
                                                           |> List.map
                                                                (fun x ->
                                                                sprintf "`%s`" x)
                                                           |> String.concat
                                                                " / "))
                                              .WithValue(c.description |> fst)
                           | Router(r) ->
                             let! commands = availableCommands v r
                             let commands =
                               commands
                               |> Array.toList
                               |> List.collect (fun (c, _) -> c.command)
                               |> List.sort
                               |> List.map (sprintf "`%s`")
                               |> String.concat " / "
                             return embedField.WithName("Module "
                                                        + (route.command
                                                           |> List.map
                                                                (fun x ->
                                                                sprintf "`%s`" x)
                                                           |> String.concat
                                                                " / "))
                                              .WithValue(sprintf
                                                           "Enter submodule `%s`\n\nAvailable commands: %s"
                                                           r.name commands)
                           | _ -> return failwith "Expecting command or router"
                         })
                    |> Async.Parallel
      embed.Fields <- System.Collections.Generic.List routes
      embed.ThumbnailUrl <- "https://assets.gitlab-static.net/uploads/-/system/project/avatar/11469265/narumi-bot-min.jpg"
      return embed
    }

  /// **Description**
  ///
  /// The default `--help` option reply. Sends the default help embed.
  ///
  /// **Parameters**
  ///   * `router` - parameter of type `NodeRouter`
  ///   * `msg` - parameter of type `Message`
  ///   * `args` - parameter of type `Arguments`
  ///
  /// **Output Type**
  ///   * `Async<unit>`
  ///
  /// **Exceptions**
  ///
  let defaultHelpFn router (msg, args) =
    async { let! embed = defaultHelpEmbed router (msg, args)
            let! _ = Async.AwaitTask
                       (msg.Channel.SendMessageAsync(embed = embed.Build()))
            return () }

  /// The default action of the router.
  /// For this pre-built version, we simply call help if there is no arguments provided.
  let defaultAction = defaultHelpFn

  /// **Description**
  ///
  /// Do the routing.
  ///
  /// **Parameters**
  ///   * `execute` - parameter of type `Message -> Arguments -> Node -> Async<unit>`:
  ///   Needed for the recursive executing of children `Node`s.
  ///   * `msg` - parameter of type `Message`
  ///   * `args` - parameter of type `Arguments`
  ///   * `router` - parameter of type `NodeRouter`
  ///
  /// **Output Type**
  ///   * `Async<unit>`
  ///
  /// **Exceptions**
  ///
  let internal route execute (msg, args) router =
    async {
      // We try to collect the first positional, and throw away all the Keyed.
      match args
            |> Args.tryFind (function
                 | Positional _ -> true
                 | _ -> false) with
      | Some(Positional "help", rest) ->
        return! router.helpFn router (msg, rest)
      | Some(Positional v, rest) ->
        match router.routes
              |> List.tryFind (fun r -> r.command |> List.exists ((=) v)) with
        | Some(node) -> return! execute (msg, rest) node.node
        | None -> return ()
      // No args, perform default action
      | _ -> return! router.defaultAction router (msg, args)
    }

  /// **Description**
  ///
  /// Create a new route.
  ///
  /// **Parameters**
  ///   * `node` - parameter of type `Node`
  ///   * `commands` - parameter of type `string list`
  ///
  /// **Output Type**
  ///   * `NodeRouterRoute`
  ///
  /// **Exceptions**
  ///
  let newRoute node commands =
    { node = node
      command = commands }
