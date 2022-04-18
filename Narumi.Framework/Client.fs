namespace Narumi.Framework

open System
open System.Threading.Tasks
open Discord.WebSocket

module Client =
  /// **Description**
  ///
  /// Turn an event into an observable.
  ///
  /// **Parameters**
  ///   * `eventAdd` - parameter of type `Func<'event,Task> -> unit`
  ///   * `eventRemove` - parameter of type `Func<'event,Task> -> unit`
  ///
  /// **Output Type**
  ///   * `IObservable<'event>`
  ///
  /// **Exceptions**
  ///
  let private observeEvent eventAdd eventRemove =
    { new System.IObservable<'event> with
        member this.Subscribe obs =
          let fn =
            fun event ->
              obs.OnNext event
              Task.CompletedTask

          let fn = Func<'event, Task>(fn)
          eventAdd (fn)
          { new System.IDisposable with
              member this.Dispose() = eventRemove fn } }

  /// **Description**
  ///
  /// Same as `observeEvent`, but for unit events.
  ///
  /// **Parameters**
  ///   * `eventAdd` - parameter of type `Func<Task> -> unit`
  ///   * `eventRemove` - parameter of type `Func<Task> -> unit`
  ///
  /// **Output Type**
  ///   * `IObservable<unit>`
  ///
  /// **Exceptions**
  ///
  let private observeUnitEvent eventAdd eventRemove =
    observeEvent (fun fn -> eventAdd (Func<Task>(fn.Invoke)))
      (fun fn -> eventRemove (Func<Task>(fn.Invoke)))

  /// Event observers.
  type Events =
    { ChannelCreated : IObservable<SocketChannel>
      ChannelDestroyed : IObservable<SocketChannel>
      Connected : IObservable<unit>
      Disconnected : IObservable<exn>
      MessageReceived : IObservable<SocketMessage>
      Ready : IObservable<unit> }

  /// **Description**
  ///
  /// Generate event observers for a Client.
  ///
  /// **Parameters**
  ///   * `client` - parameter of type `DiscordSocketClient`
  ///
  /// **Output Type**
  ///   * `Events`
  ///
  /// **Exceptions**
  ///
  let events (client : DiscordSocketClient) =
    { // DiscordSocketClient events
      Connected = observeUnitEvent client.add_Connected client.remove_Connected
      Disconnected =
        observeEvent client.add_Disconnected client.remove_Disconnected
      Ready = observeUnitEvent client.add_Ready client.remove_Ready
      // BaseSocketClient events
      ChannelCreated =
        observeEvent client.add_ChannelCreated client.remove_ChannelCreated
      ChannelDestroyed =
        observeEvent client.add_ChannelDestroyed client.remove_ChannelDestroyed
      // blah blah
      // I'll add when I need any
      MessageReceived =
        observeEvent client.add_MessageReceived client.remove_MessageReceived }
