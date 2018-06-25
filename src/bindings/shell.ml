external cp : string -> string -> string -> unit = "" [@@bs.module "shelljs"] 
let cp ?(options="") src dst =
  cp options src dst
