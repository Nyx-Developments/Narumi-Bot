namespace Narumi.Framework

open System
open System.Text

module Args =
  /// **Description**
  ///
  /// Generate a new Arguments parser from a message.
  ///
  /// **Parameters**
  ///   * `msg` - parameter of type `Message`
  ///
  /// **Output Type**
  ///   * `Arguments`
  ///
  /// **Exceptions**
  ///
  let fromMessage (msg : Message) = Arguments msg.Content

  let rec private parseNext (str : char list) =
    let rec trimWhitespaces str =
      match str with
      | [] -> []
      | x :: rest ->
        if Char.IsWhiteSpace(x) then trimWhitespaces rest
        else str

    let parseLiteral (quote : char) str =
      let rec parse (current : StringBuilder) str =
        match str with
        | [] -> None
        | q :: rest when q = quote -> Some((current.ToString(), rest))
        | x :: rest -> parse (current.Append(x)) rest
      parse (StringBuilder("")) str

    /// Splits at the predicate
    let splitAtPred pred lst =
      let rec accum cur =
        function
        | [] -> (List.rev cur, [])
        | (x :: _) as all when pred x -> (List.rev cur, all)
        | x :: rest -> accum (x :: cur) rest
      accum [] lst

    let rec parseWord (str : char list) =
      let (a, b) = splitAtPred Char.IsWhiteSpace str

      let a =
        a
        |> List.map string
        |> String.concat ""
      (a, b) |> Some

    let parseWordOrLiteral str =
      match str with
      | ('\'' as t) :: rest
      | ('"' as t) :: rest -> parseLiteral t rest
      | x -> parseWord x

    let parseKeyed str =
      match str with
      | '-' :: '-' :: rest ->
        parseWord rest
        |> Option.bind
             (fun (key, rest) ->
             parseWordOrLiteral (trimWhitespaces rest)
             |> Option.map (fun (value, rest) -> (key, value, rest)))
        |> Option.map (fun (key, value, rest) -> (Keyed(key, value), rest))
      | '-' :: x :: rest ->
        parseWordOrLiteral (trimWhitespaces rest)
        |> Option.map (fun (value, rest) -> (Keyed(x.ToString(), value), rest))
      | _ -> None

    let literalPair (a, b) = (Positional a, b)
    let str = trimWhitespaces str
    match str with
    | [] -> None
    /// A keyed argument
    | '-' :: _ -> parseKeyed str
    /// A literal argument
    | ('"' as t) :: rest
    | ('\'' as t) :: rest -> parseLiteral t rest |> Option.map literalPair
    /// Anything else is a word
    | _ -> parseWord str |> Option.map literalPair

  /// **Description**
  ///
  /// Try to parse a new Argument. Will return a `option` with an Argument and the rest.
  ///
  /// **Parameters**
  ///  * `Arguments`
  ///
  /// **Output Type**
  ///   * `(Argument * Arguments) option`
  ///
  /// **Exceptions**
  ///
  let next (Arguments remain) =
    parseNext (remain.ToCharArray() |> List.ofArray)
    |> Option.map (fun (a, b) ->
         (a,
          Arguments(b
                    |> List.map string
                    |> String.concat "")))

  /// **Description**
  ///
  /// Returns the remaining argument set.
  ///
  /// **Parameters**
  ///   * `Arguments`
  ///
  /// **Output Type**
  ///   * `string`
  ///
  /// **Exceptions**
  ///
  let rest (Arguments remain) = remain.TrimStart()

  /// **Description**
  ///
  /// Take at most `count` `Argument`s from the `Arguments`.
  ///
  /// **Parameters**
  ///   * `count` - parameter of type `int`
  ///   * `arguments` - parameter of type `Arguments`
  ///
  /// **Output Type**
  ///   * `Argument list * Arguments`
  ///
  /// **Exceptions**
  ///
  let take count arguments =
    let rec accum cur cnt args =
      match next args with
      | _ when cnt = count -> (List.rev cur, args)
      | Some(arg, rest) -> accum (arg :: cur) (cnt + 1) rest
      | None -> (List.rev cur, args)
    accum [] 0 arguments

  /// **Description**
  ///
  /// Try to find the first parameter satisfying `pred`.
  ///
  /// **Parameters**
  ///   * `pred` - parameter of type `Argument -> bool`
  ///   * `arguments` - parameter of type `Arguments`
  ///
  /// **Output Type**
  ///   * `(Argument * Arguments) option`
  ///
  /// **Exceptions**
  ///
  let tryFind pred arguments =
    let rec run arguments =
      next arguments
      |> Option.bind (fun (v, nx) ->
           if pred v then Some(v, nx)
           else run nx)
    run arguments
