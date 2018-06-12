module type Logger_t = sig
  val info  : 'a -> unit
  val error : 'a -> unit
end

module Make(Logger:Logger_t) = struct
  type process
  external process : process = "" [@@bs.val]
  external on : process -> string -> (exn -> unit) -> unit = "" [@@bs.send]

  let () =
    on process "uncaughtException" (fun exn ->
      Logger.error exn;
      raise exn)

  let info s =
    Logger.info s;
    Js.log s

  let error_log : 'a -> unit [@bs] = [%bs.raw{|function (m) {
    console.error(m);
  }|}]

  let error s =
    Logger.error s;
    error_log (Obj.magic s) [@bs]
end

module Dummy = struct
  let info _ = ()
  let error _ = ()
end

include Make(Dummy)
