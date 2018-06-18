open LidcoreBsNode
open BsAsyncMonad

module Runtime : sig
  val subscribe : maxMessages:int -> topic:string -> subscription:string ->
                  (Buffer.t -> unit Callback.t) -> unit Callback.t

  val register : string -> (unit -> unit Callback.t) -> unit
  val run : unit -> unit
end

module Config : sig
  type 'a instanceTemplate = serviceAccount:string -> projectId:string -> 'a Js.t

  type ('a, 'b) config = {
    name:             string;
    projectId:        string;
    image:            string;
    serviceAccount:   string;
    zone:             string;
    instanceTemplate: 'a instanceTemplate;
    autoscale:        'b Js.t
  } [@@bs.deriving abstract]

  val initialize : ?restart:bool -> ('a, 'b) config -> unit Callback.t
end
