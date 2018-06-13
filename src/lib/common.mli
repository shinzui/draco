open BsAsyncMonad

(* Requeue a pubsub message. Might need to go somewhere else. *)
val requeue : msg:'a Js.t -> string -> unit Callback.t
