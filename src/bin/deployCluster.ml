open BsAsyncMonad.Callback
open LidcoreBsNode

external argv : string array = "" [@@bs.val] [@@bs.scope "process"]

let usage = "Usage: deploy-cluster [--restart] /path/to/config.json"

external exit : int -> 'a = "" [@@bs.val] [@@bs.scope "process"]

let restart = ref false

let configPath = ref ""

let args = [
  "--restart", Arg.Unit (fun () -> restart := true), "Restart existing instances after deploy"
]

let die ?(msg=usage) () =
  Js.log msg;
  exit 1

let () =
  let argc = Array.length argv in
  if argc < 3 then die ();
  begin
   try
     Arg.parse_argv (Array.sub argv 2 (argc-2) )args (fun path -> configPath := path) usage;
   with
     | Arg.Help msg
     | Arg.Bad msg -> die ~msg ();
  end;
  if !configPath = "" then die ();
  let config =
    Obj.magic
      (Utils.Json.parse_buf
        (Fs.readFileSync !configPath))
  in
  let restart = !restart in
  let name =
    config |. Instances.Config.name
  in
  Js.log {j|Deploying $(name)...|j};
  finish (Instances.Config.initialize ~restart config >| fun () ->
    Js.log "Done!")
