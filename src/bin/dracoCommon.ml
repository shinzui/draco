open LidcoreBsNode

let stage = ref "staging"

let () =
  let args = [
    "-stage", Arg.Set_string stage, "Set stage"
  ] in
  try
    Arg.parse_argv Process.argv args (fun _ -> ()) "";
  with
    | Arg.Help _ | Arg.Bad _ -> ()

let usage opts =
  {j|Usage: draco $(opts) [-stage <stage]|j}

let baseDir =
  let cwd = Process.argv.(1) in
  Path.normalize {j|$(cwd)/../../..|j}

let configPath =
  let stage = !stage in
  {j|$(baseDir)/config/$(stage)/draco.yml|j}

let config () = Yaml.parse
  (Buffer.toString
    (Fs.readFileSync configPath))

let die ?msg ?usage () =
  let fn msg =
   match msg with
     | Some msg -> Logger.error msg
     | None -> ()
  in
  fn msg; fn usage;
  Process.exit 1

let () =
  if not (Fs.existsSync configPath) then
    die ~msg:{j|Couldn't find config file $(configPath)|j} ()
