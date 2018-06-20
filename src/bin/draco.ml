open BsAsyncMonad.Callback
open LidcoreBsNode

type operation = [
  | `Create [@bs.as "Creating"]
  | `Restart [@bs.as "Restarting"]
  | `Destroy [@bs.as "Destroying"]
] [@@bs.deriving jsConverter]

external argv : string array = "" [@@bs.val] [@@bs.scope "process"]

let usage = "Usage: draco [-create|-restart|-destroy] /path/to/config.json"

external exit : int -> 'a = "" [@@bs.val] [@@bs.scope "process"]

let operation = ref `Create

let configPath = ref ""

let args = [
  "-create", Arg.Unit (fun () -> operation := `Create), "Create instances (default)";
  "-restart", Arg.Unit (fun () -> operation := `Restart), "Restart existing instances";
  "-destroy", Arg.Unit (fun () -> operation := `Destroy), "Destroy existing cluster"
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
          Instances.Config.initialize
      | `Restart ->
          Instances.Config.restart
      | `Destroy ->
          Instances.Config.destroy
  in
  let name =
    config |. Instances.Config.name
  in
  let operation =
    operationToJs !operation
  in
  Printf.printf "%s %s.." operation name;
  finish (fn config >| fun () ->
    Printf.printf " done!\n")
