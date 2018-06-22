val baseDir    : string
val usage      : string -> string
val configPath : string
val config     : unit -> 'a Js.t
val die        : ?msg:string -> ?usage:string -> unit -> 'a
