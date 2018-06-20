open BsAsyncMonad.Callback
open LidcoreBsNode

type operation = [
  | `Create
  | `Create_and_restart
  | `Delete
]

external argv : string array = "" [@@bs.val] [@@bs.scope "process"]

let usage = "Usage: deploy-cluster [--restart|--delete] /path/to/config.json"

external exit : int -> 'a = "" [@@bs.val] [@@bs.scope "process"]

let operation = ref `Create

let configPath = ref ""

let args = [
  "--restart", Arg.Unit (fun () -> operation := `Create_and_restart), "Restart existing instances after deploy";
  "--delete", Arg.Unit (fun () -> operation := `Delete), "Delete existing cluster"
]

let die ?(msg=usage) () =
  Js.log msg;
  exit 1

let () =
  let argc = Array.length argv in
  if argc < 3 then die ();
  begin
   try
     Arg.parse_argv (Array.sub argv 1 (argc-1)) args (fun path -> configPath := path) usage;
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
  let fn =
    match !operation with
      | `Create ->
          Instances.Config.initialize ~restart:false
      | `Create_and_restart ->
          Instances.Config.initialize ~restart:true
      | `Delete ->
          Instances.Config.destroy
  in
  let name =
    config |. Instances.Config.name
  in
  Js.log {j|Deploying $(name)...|j};
  finish (fn config >| fun () ->
    Js.log "Done!")
