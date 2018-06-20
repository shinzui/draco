open LidcoreBsNode
open BsAsyncMonad

module Runtime : sig
  val subscribe : maxMessages:int -> topic:string -> subscription:string ->
                  (Buffer.t -> unit Callback.t) -> unit Callback.t

  val register : string -> (unit -> unit Callback.t) -> unit
  val run : unit -> unit
end

module Config : sig
  type ('a, 'b) config = {
    name:             string;
    projectId:        string;
    serviceAccount:   string;
    zone:             string;
    instanceTemplate: 'a Js.t;
    autoscale:        'b Js.t
  } [@@bs.deriving abstract]

  val initialize : restart:bool -> ('a, 'b) config -> unit Callback.t
  val destroy : ('a, 'b) config -> unit Callback.t
end
