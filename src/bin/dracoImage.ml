open LidcoreBsNode
open DracoCommon

type build = [
  | `Base [@bs.as "base"]
  | `App  [@bs.as "application"]
  | `Both [@bs.as "base and application"]
] [@@bs.deriving jsConverter]

let () =
  usage "image build [base|app|both]"

let packer ~args config =
  let baseDir =
    DracoCommon.baseDir
  in
  let config =
    Utils.escape {j|$(baseDir)/packer/$(config)|j}
  in
  let args = List.map (fun (lbl,value) ->
    let opt = Utils.escape {j|$(lbl)=$(value)|j} in
    {j|-var $(opt)|j}) args
  in
  let args = String.concat " " args in
  let stdio = {Child_process.
    stdin  = `Inherit Process.stdin;
    stdout = `Inherit Process.stdout;
    stderr = `Inherit Process.stderr
  } in
  ignore(Child_process.spawn ~shell:true ~stdio {j|packer build -force $(args) $(config)|j}) 

let () =
  if argc <> 4 then die ();
  if argv.(2) <> "build" then die ();
  let mode =
    match argv.(3) with
      | "base" -> `Base
      | "app"  -> `App
      | "both" -> `Both
      | _ -> die ~msg:"Invalid mode" ()
  in
  let config = config () in
  let args = [
    "project", config##projectId;
    "zone", config##zone
  ] in
  match mode with
    | `Base -> packer ~args "base.json"
    | _ -> assert false

