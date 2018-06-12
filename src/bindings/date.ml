type date_class
external date_class : date_class = "Date" [@@bs.val]

external now : date_class -> unit -> float = "" [@@bs.send]
let now = now date_class

type t
external init : int -> t =  "" [@@bs.new "Date"]

external getFullYear : t -> unit -> int = "" [@@bs.send]
let getFullYear d = getFullYear d ()

external getDate : t -> unit -> int = "" [@@bs.send]
let getDate d = getDate d ()

external getMonth : t -> unit -> int = "" [@@bs.send]
let getMonth d = getMonth d ()
