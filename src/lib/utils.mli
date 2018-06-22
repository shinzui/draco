open LidcoreBsNode

val get_some : 'a option -> 'a
val partition : int -> 'a array -> 'a array array
val escape : string -> string

module Json : sig
  val parse : string -> 'a Js.t
  val parse_buf : Buffer.t -> 'a Js.t
  val stringify : 'a -> string
end
