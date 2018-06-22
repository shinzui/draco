open LidcoreBsNode
open DracoCommon

type build = [
  | `Base [@bs.as "base"]
  | `App  [@bs.as "application"]
  | `Both [@bs.as "base and application"]
] [@@bs.deriving jsConverter]

let () =
  usage "image build [base|app|both]"

let getConfig ~tmp ~config mode =
  let path =
    {j|$(baseDir)/packer/$(mode).json|j}
  in
  let packerConfig =
    Utils.Json.parse_buf
      (Fs.readFileSync path)
  in
  (match Js.Nullable.toOption config##image with
    | Some config ->
        (match Js.Dict.get config mode with
          | Some config ->
              (match Js.Nullable.toOption config##provisioners with
                | Some provisioners ->
                    packerConfig##provisioners #= 
                      (Js.Array.concat provisioners packerConfig##provisioners)
                | None -> ())
          | None -> ())
    | None -> ());
  let path =
    Tmp.make ~postfix:".json" tmp
  in
  Fs.writeFileSync path (Utils.Json.stringify packerConfig);
  path

let packer ~args ~config mode =
  let tmp = Tmp.init () in
  let config =
    Utils.escape (getConfig ~config ~tmp mode)
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
  let child =
    Child_process.spawn ~shell:true ~stdio {j|packer build -force $(args) $(config)|j}
  in
  Child_process.on child (`Exit (fun _ ->
    Tmp.cleanup tmp))

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
    | `Base -> packer ~args ~config "base"
    | _ -> assert false

