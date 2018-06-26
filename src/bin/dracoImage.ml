open LidcoreBsNode
open DracoCommon

let default_mode = "both"

let () =
  usage "image build [base|app|both]"

type ('a,'b) packerConfig = {
  provisioners: 'a Js.t array;
  builders:  'b Js.t array
} [@@bs.deriving abstract]

let getConfig ~config ~mode name =
  match Js.Nullable.toOption config##image with
    | Some config ->
        (match Js.Dict.get config mode with
          | Some config ->
              (match Js.Dict.get config name with
                 | Some value -> Obj.magic value
                 | None -> raise Not_found)
          | None -> raise Not_found)
    | None -> raise Not_found

let provisioners ~config mode =
  let get mode =
    let ret =
      getConfig ~config ~mode "provisioners"
    in
    Array.sort (fun x y -> - compare x##priority y##priority) ret;
    Array.iter (Utils.delete "priority") ret;
    ret
  in
  match mode with
    | "both" ->
        Js.Array.concat (get "app") (get "base")
    | _ ->
        get mode

let buildConfig ~config mode =
  let provisioners =
    provisioners ~config mode
  in
  let builder =
    let mode =
      if mode = "both" then "base" else mode
    in
    let instance_name =
      let id = Cuid.get () in
      {j|draco-build-$(id)|j}
    in
    let image_name =
      match mode with
        | "app" | "both" -> "draco-app"
        | _ -> "draco-base"
    in
    let builder =
      getConfig ~config ~mode "builder"
    in
    let condSet lbl value =
      match Js.Dict.get builder lbl with
        | Some v -> ()
        | None -> Js.Dict.set builder lbl value
    in
    condSet "project_id" config##projectId;
    condSet "zone" config##zone;
    condSet "instance_name" instance_name;
    condSet "image_name" image_name;
    builder
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
      let mode = argv.(3) in
      if not (List.mem mode ["base";"app";"both"]) then
        die ~msg:{j|Invalid mode: $(mode)|j} ();
      mode
     with
       | Invalid_argument "index out of bounds" -> default_mode
  in
  let config = config () in
  let args = [
    "project", config##projectId;
    "zone", config##zone
  ] in
  packer ~args ~config mode
