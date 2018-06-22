open LidcoreBsNode
open BsAsyncMonad

module Runtime : sig
  val subscribe : maxMessages:int -> topic:string -> subscription:string ->
                  (Buffer.t -> unit Callback.t) -> unit Callback.t

  val register : string -> (unit -> unit Callback.t) -> unit
  val run : unit -> unit
end

module Config : sig
  val initialize : projectId:string -> serviceAccount:string ->
                   zone:string ->
                   instanceTemplate:'a Js.t ->
                   autoscaler:'b Js.t -> string -> unit Callback.t
  val restart : projectId:string -> zone:string -> string -> unit Callback.t
  val destroy : projectId:string -> zone:string -> string -> unit Callback.t
end
