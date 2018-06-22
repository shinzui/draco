open BsAsyncMonad.Callback
open LidcoreBsNode
open DracoCommon

type operation = [
  | `Create [@bs.as "Creating"]
  | `Restart [@bs.as "Restarting"]
  | `Destroy [@bs.as "Destroying"]
] [@@bs.deriving jsConverter]

let usage = usage "instances [create|restart|destroy] instance-group-name"
let die = die ~usage

let () =
  let argc = Array.length Process.argv in
  if argc <> 4 then die ();
  let operation = 
    match Process.argv.(2) with
      | "create" -> `Create
      | "restart" -> `Restart
      | "destroy" -> `Destroy
      | _ -> die ~msg:"Invalid mode" ()
  in
  let name = Process.argv.(3) in
  let config = config () in
  let projectId =
    config##projectId
  in
  let serviceAccount =
    config##serviceAccount
  in
  let zone =
    config##zone
  in
  let instances =
    Array.to_list config##instances
  in
  let config =
    try
      List.find (fun config -> config##name = name) instances
    with
      | Not_found ->
          die ~msg:{j|No config for instance group $(name) in $(configPath)|j} ()
  in
  let fn () =
    match operation with
      | `Create ->
          Instances.Config.initialize ~projectId ~serviceAccount ~zone
                                      ~instanceTemplate:config##instanceTemplate
                                      ~autoscaler:config##autoscaler
                                      name
      | `Restart ->
          Instances.Config.restart ~projectId ~zone name
      | `Destroy ->
          Instances.Config.destroy ~projectId ~zone name
  in
  let operation =
    operationToJs operation
  in
  let spinner =
    Spinner.init {j|$(operation) $(name).. %s|j};
  in
  Spinner.start spinner;
  finish (fn () &> fun () ->
    Spinner.stop ~clean:true spinner;
    Printf.printf "%s %s.. done!\n" operation name;
    return ())
