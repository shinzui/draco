open LidcoreBsNode

let stage = ref "staging"

let argc = Array.length Process.argv - 1
let argv = Array.sub Process.argv 1 argc

let () =
  let args = [
    "-stage", Arg.Set_string stage, "Set stage"
  ] in
  try
    Arg.parse_argv Process.argv args (fun _ -> ()) "";
  with
    | Arg.Help _ | Arg.Bad _ -> ()

let usageMsg = ref "Usage: draco [mode] [options] [-stage <stage>]"

let usage opts =
  usageMsg := {j|Usage: draco [mode] $(opts) [-stage <stage>]|j}

let baseDir =
  let cwd =
    Fs.realpathSync Process.argv.(1)
  in
  Path.normalize {j|$(cwd)/../../../../../..|j}

let configPath =
  let stage = !stage in
  {j|$(baseDir)/config/$(stage)/draco.yml|j}

let config () = Yaml.parse
  (Buffer.toString
    (Fs.readFileSync configPath))

let die ?msg () =
  begin
   match msg with
     | Some msg -> Logger.error msg
     | None -> ()
  end;
  Logger.error !usageMsg;
  Process.exit 1

let () =
  if not (Fs.existsSync configPath) then
    die ~msg:{j|Couldn't find config file $(configPath)|j} ()
