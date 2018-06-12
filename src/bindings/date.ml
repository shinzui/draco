type date_class
external date_class : date_class = "Date" [@@bs.val]

external now : date_class -> float = "" [@@bs.send]
let now () =
  now date_class

type t
external init : int -> t =  "" [@@bs.new "Date"]

external getFullYear : t -> int = "" [@@bs.send]
external getDate     : t -> int = "" [@@bs.send]
external getMonth    : t -> int = "" [@@bs.send]
