open LidcoreBsNode
open DracoCommon

type build = [
  | `Base [@bs.as "base"]
  | `App  [@bs.as "application"]
  | `Both [@bs.as "base and application"]
] [@@bs.deriving jsConverter]

let () =
  usage "image build [base|app|both]"

external __dirname : string = "" [@@bs.val]

type provisioner = {
  ptype:  string [@bs.as "type"];
  script: string;
  environment_vars: string array
} [@@bs.deriving abstract]

let provisioner ~projectId ~zone mode =
  let script =
    Fs.realpathSync {j|$(__dirname)/../../packer/$(mode).sh|j}
  in
  provisioner
    ~ptype:"shell" ~script
    ~environment_vars:[|
      {j|PROJECT=$(projectId)|j};
      {j|ZONE=$(zone)|j}
    |]

type builder = {
  btype: string [@bs.as "type"];
  project_id: string;
  source_image_family: string;
  zone: string;
  ssh_username: string;
  image_name: string;
  image_family: string;
  instance_name: string;
  machine_type: string;
  disk_size: string;
  disk_type: string
} [@@bs.deriving abstract]

let builder ~projectId ~zone mode =
  builder ~btype:"googlecompute"
    ~project_id:projectId
    ~source_image_family:"ubuntu-1604-lts"
    ~zone ~ssh_username:"ubuntu"
    ~image_name:"draco-base"
    ~image_family:"draco"
    ~instance_name:"draco-build-base"
    ~machine_type:"n1-standard-1"
    ~disk_size:"50"
    ~disk_type:"pd-ssd"

type packerConfig = {
  provisioners: provisioner array;
  builders: builder array
} [@@bs.deriving abstract]

let getConfig ~tmp ~config mode =
  let projectId = config##projectId in
  let zone = config##zone in
  let provisioner =
    provisioner ~projectId ~zone mode
  in
  let provisioners =
    match Js.Nullable.toOption config##image with
      | Some config ->
          (match Js.Dict.get config mode with
            | Some config ->
                (match Js.Nullable.toOption config##provisioners with
                   | Some provisioners -> provisioners
                   | None -> [||])
            | None -> [||])
      | None -> [||]
  in
  ignore(Js.Array.unshift provisioner provisioners);
  let builder =
    builder ~projectId ~zone mode
  in
  let packerConfig =
    packerConfig ~provisioners ~builders:[|builder|]
  in
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

