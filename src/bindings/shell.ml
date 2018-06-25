external cp : string -> string -> string -> unit = "" [@@bs.module "shell"] 
let cp ?(options="") src dst =
  cp options src dst
