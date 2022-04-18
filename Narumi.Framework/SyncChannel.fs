namespace Narumi.Framework.Concurrency

/// **Description**
///
/// Channel implements a Monad-like concurrent queue that uses `MailboxProcessor` under the hood.
///
/// **Constructor**
///  * initData - parameter of type `'Data`: The initial data.
///  * fn - parameter of type `'Data -> 'Query -> Async<'Data * 'Output>`: the "fold"
///  function for the channel.
type Channel<'Data, 'Query, 'Output>(initData : 'Data, fn : Channel<'Data, 'Query, 'Output> -> 'Data -> 'Query -> Async<'Data * 'Output>) as this =

  let callFn self data (query, replyFn) =
    async {
      let! (newData, output) = fn self data query
      replyFn output
      return newData
    }

  let rec handler self data
          (mailbox : MailboxProcessor<'Query * ('Output -> unit)>) =
    async { let! query = mailbox.Receive()
            let! data = callFn self data query
            return! handler self data mailbox }
  let mailbox = MailboxProcessor.Start(handler this initData)

  /// Posts a new message and awaits for reply.
  member this.Post query =
    mailbox.PostAndAsyncReply(fun chan -> (query, chan.Reply))

  /// Sends the message without waiting for a reply.
  member this.Send query = mailbox.Post(query, ignore)

  /// Overloads the constructor for the pure function version
  new(initData : 'Data, pureFn : 'Data -> 'Query -> Async<'Data * 'Output>) =
    Channel(initData, fun _ -> pureFn)
