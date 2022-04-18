namespace Narumi.Framework

module Command =
  /// **Description**
  ///
  /// Create a new command
  ///
  /// **Arguments**
  ///  * `handler`: Argument of type `Handler`
  ///  * `short description`: Argument of type `string`
  ///  * `long description`: Argument of type `string`
  let create handler ``short description`` ``long description`` =
    Command({ description = (``short description``, ``long description``)
              handler = handler })
