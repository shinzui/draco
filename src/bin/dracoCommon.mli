val argc       : int
val argv       : string array
val baseDir    : string
val usage      : string -> unit
val configPath : string
val config     : unit -> 'a Js.t
val die        : ?msg:string -> unit -> 'a
val getPath    : string -> string
