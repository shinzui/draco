open BsAsyncMonad.Callback
open LidcoreBsNode

external argv : string array = "" [@@bs.val] [@@bs.scope "process"]

external exit : int -> 'a = "" [@@bs.val] [@@bs.scope "process"]

let usage = "Usage: deploy /path/to/config.json"

let () =
  let configPath =
    try
      argv.(2)
    with _ ->
      Js.log usage;
      exit 1
  in
  let config =
    Obj.magic
      (Utils.Json.parse_buf
        (Fs.readFileSync configPath))
  in
  let name =
    config |. Instances.Config.name
  in
  Js.log {j|Deploying $(name)...|j};
  finish (Instances.Config.initialize config >| fun () ->
    Js.log "Done!")
