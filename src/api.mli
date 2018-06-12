open BsAsyncMonad

(* Type for functions, needs to be exported. *)
type fn

val offline : bool

val requeue : msg:'a Js.t -> string -> unit Callback.t

val on_error : (Js.Exn.t -> unit) -> unit

module Http : sig
  type t

  type request
  type response
  type handler = request -> response Callback.t

  val body   : request -> 'a Js.t
  val param  : request -> string -> string option
  val params : request -> 'a Js.t
  val query  : request -> string -> string option

  val response : ?code:int -> ?headers:(string, string) Hashtbl.t -> 'a -> response Callback.t
  val error    :  code:int -> string  -> 'a Callback.t

  val init : unit -> t

  val get  : ?auth:bool -> t -> string -> handler -> unit
  val post : ?auth:bool -> t -> string -> handler -> unit 
  val put  : ?auth:bool -> t -> string -> handler -> unit

  val export : t -> fn
end

module Event : sig
  type handler = unit Callback.t
  val subscribe : string -> ('a Js.t -> handler) -> fn
end
