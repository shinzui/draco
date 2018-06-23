open LidcoreBsNode
open DracoCommon

type build = [
  | `Base [@bs.as "base"]
  | `App  [@bs.as "app"]
  | `Both [@bs.as "both"]
] [@@bs.deriving jsConverter]

let default_mode = `Both

let () =
  usage "image build [base|app|both]"

external __dirname : string = "" [@@bs.val]

type provisioner = {
  ptype:  string [@bs.as "type"];
  (* Script provisioner *)
  script: string [@bs.optional];
  environment_vars: string array [@bs.optional];
  (* File provisioner *)
  source: string [@bs.optional];
  destination: string [@bs.optional]
} [@@bs.deriving abstract]

let getPath file =
  Fs.realpathSync {j|$(__dirname)/../../$(file)|j}

let buildProvisioner ~projectId ~zone mode =
  let script =
    getPath {j|packer/$(mode).sh|j}
  in
  provisioner
    ~ptype:"shell" ~script
    ~environment_vars:[|
      {j|PROJECT=$(projectId)|j};
      {j|ZONE=$(zone)|j}
    |] ()

let systemdProvisioner =
  let source =
    getPath "packer/draco.system.in"
  in
  provisioner ~ptype:"file"
              ~source ~destination:"/tmp" ()

type builder = {
  btype: string [@bs.as "type"];
  project_id: string;
  source_image_family: string [@bs.optional];
  source_image: string [@bs.optional];
  zone: string;
  ssh_username: string;
  image_name: string;
  image_family: string;
  instance_name: string;
  machine_type: string;
  disk_size: string;
  disk_type: string
} [@@bs.deriving abstract]

let builder ?source_image ?source_image_family
            ~image_name ~instance_name ~projectId ~zone mode =
  builder ~btype:"googlecompute"
    ~project_id:projectId
    ?source_image_family ?source_image
    ~zone ~ssh_username:"ubuntu"
    ~image_name
    ~image_family:"draco"
    ~instance_name
    ~machine_type:"n1-standard-1"
    ~disk_size:"50"
    ~disk_type:"pd-ssd"

type packerConfig = {
  provisioners: provisioner array;
  builders: builder array
} [@@bs.deriving abstract]

let provisioners ~projectId ~zone ~config mode =
  let buildProvisioners =
    let buildProvisioner =
      buildProvisioner ~projectId ~zone mode
    in
    match buildFromJs mode with
      | Some `App | Some `Both ->
          [|buildProvisioner;systemdProvisioner|]
      | _ ->
          [|buildProvisioner|]
  in
  let customProvisioners =
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
  Js.Array.concat buildProvisioners customProvisioners

let buildConfig ~config mode =
  let projectId = config##projectId in
  let zone = config##zone in
  let smode =
    buildToJs mode
  in
  let provisioners =
    match mode with
      | `Both -> 
          let baseProvisioners =
            provisioners ~projectId ~zone ~config "base"
          in
          Js.Array.concat baseProvisioners
            (provisioners ~projectId ~zone ~config "app")
      | _ ->
          provisioners ~projectId ~zone ~config smode
  in
  let instance_name =
    {j|draco-$(smode)|j}
  in
  let image_name =
    let iname =
      match mode with
        | `Both -> "app"
        | _ -> smode
    in
    {j|draco-$(iname)|j}
  in
  let source_image, source_image_family =
    match mode with
      | `Both | `Base ->
          None, Some "ubuntu-1604-lts"
      | `App ->
          Some "draco-base", Some "draco-images"
  in
  let builder =
    builder ?source_image ?source_image_family
            ~projectId ~zone ~instance_name
            ~image_name mode ()
  in
  packerConfig ~provisioners ~builders:[|builder|]

let getConfig ~tmp ~config mode =
  let packerConfig =
    buildConfig ~config mode
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
  if argc < 3 then die ();
  if argv.(2) <> "build" then die ();
  let mode =
    try
      match argv.(3) with
        | "base" -> `Base
        | "app"  -> `App
        | "both" -> `Both
        | x -> die ~msg:{j|Invalid mode: $(x)|j} ()
     with
       | Invalid_argument "index out of bounds" -> default_mode
  in
  let config = config () in
  let args = [
    "project", config##projectId;
    "zone", config##zone
  ] in
  packer ~args ~config mode
