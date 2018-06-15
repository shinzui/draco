open LidcoreBsNode
open BsAsyncMonad
open BsAsyncMonad.Callback

let exceptionHandler exn =
  !Config.error_handler (Obj.magic exn)

let instances = Hashtbl.create 10
let register label handler =
  Hashtbl.add instances label handler

module C = Gcloud.Compute
module VM = Gcloud.Compute.Zone.VM

let run () =
  let zone =
    Env.get ~default:"us-west1-b" "zone"
  in
  let instance =
     C.Zone.vm (C.zone (C.init ()) zone) (Os.hostname ())
  in
  Callback.finish ~exceptionHandler (VM.getMetadata instance >> fun meta ->
    let items =
      Array.to_list (meta|.VM.metadata|.VM.items)
    in
    let label =
      let data =
        List.find (fun el -> (el|.VM.key) = "draco_instance_type") items
      in
      data|.VM.value
    in
    Logger.info {j|Starting instance $(label)|j};
    let handler = Hashtbl.find instances label in
    handler ())

let stopping = ref false
let () =
  Process.on `SIGTERM (fun () ->
    stopping := true)

(* de-duplicate messages goddamn it *)

(* 2 days *)
let msg_check_expire =
  60. *. 24. *. 2.

let msg_check_key id =
  {j|msg_check_$(id)|j}

let redis =
  Redis.createClient
    (Env.get ~default:"redis://localhost:6379" "REDIS_URL")

let is_duplicate id =
  let key = msg_check_key id in
  Redis.setnx redis key "foo" >> fun n ->
    if n = 0 then
      return true
    else
     begin
      Redis.expire redis key msg_check_expire >| fun () ->
        false
     end

 let log msg =
  Logger.info {j|$(msg)|j}

let subscribe ~maxMessages ~topic ~subscription handler =
  let pubsub = Gcloud.PubSub.init () in
  let config =
    let flowControl =
      Gcloud.PubSub.flow_control ~maxMessages ()
    in
    Gcloud.PubSub.subscription_config ~flowControl ()
  in
  let requeue msg =
    let msg =
      Utils.Json.parse_buf (msg|.Gcloud.PubSub.data)
    in
    Common.requeue ~msg topic
  in
  let requeue msg =
    requeue msg ||> fun exn ->
      exceptionHandler exn;
      return ()
  in
  Gcloud.PubSub.subscription ~config ~topic pubsub subscription >| fun s ->
    Gcloud.PubSub.subscribe s (fun msg ->
      if not !stopping then
        begin
          let handler () =
            handler (msg|.Gcloud.PubSub.data) >| fun _ ->
              Gcloud.PubSub.ack msg
          in
          let handler () =
            handler () ||> fun exn ->
              Gcloud.PubSub.ack msg;
              requeue msg >> fun _ ->
                Callback.fail exn
          in
          let handler =
            let id = msg|.Gcloud.PubSub.id in
            is_duplicate id >> fun ret ->
              if ret then
               begin
                log {j|Found duplicate message with id: $(id)|j};
                Gcloud.PubSub.ack msg;
                return ()
               end
              else
                handler ()
          in
          Callback.finish ~exceptionHandler handler
        end
       else
         Gcloud.PubSub.nack msg)
