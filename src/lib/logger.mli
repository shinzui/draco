module type Logger_t = sig
  val info  : 'a -> unit
  val error : 'a -> unit
end

module Make(Logger:Logger_t) : Logger_t

include Logger_t
