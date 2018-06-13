open LidcoreBsNode
open BsAsyncMonad

val register : string -> (unit -> unit Callback.t) -> unit
val run : unit -> unit

val subscribe : maxMessages:int -> topic:string -> subscription:string -> (Buffer.t -> unit Callback.t) -> unit Callback.t
