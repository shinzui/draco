open LidcoreBsNode
open BsAsyncMonad
open BsAsyncMonad.Callback
  
module Runtime = struct
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
end

module Config = struct
  type 'a instanceTemplate = serviceAccount:string -> projectId:string -> 'a Js.t

  type ('a, 'b) config = {
    name:             string;
    projectId:        string;
    image:            string;
    serviceAccount:   string;
    zone:             string;
    instanceTemplate: 'a instanceTemplate;
    autoscale:        'b Js.t
  } [@@bs.deriving abstract]

  let initialize ?(restart=true) config =
    let name =
      name config
    in
    let zone =
      zone config
    in
    let serviceAccount =
      serviceAccount config
    in
    let projectId =
      projectId config
    in
    let instanceTemplateConfig =
      Obj.magic
        (instanceTemplate config ~serviceAccount ~projectId)
    in
    instanceTemplateConfig##properties##metadata #= [%bs.obj{
      items = [|[%bs.obj{
        key  = "mode";
        value = name
      }]|]
    }];
    let autoscaleConfig =
      Obj.magic (autoscale config)
    in
    autoscaleConfig##target #= name;
    let compute =
      Gcloud.Compute.init ~config:(Gcloud.config ~projectId ()) ()
    in
    let instanceTemplate =
      Gcloud.Compute.instanceTemplate compute name
    in
    let zone =
      Gcloud.Compute.zone compute zone
    in
    let instanceGroupManager =
      Gcloud.Compute.Zone.instanceGroupManager zone name
    in
    let autoscaler =
      Gcloud.Compute.Zone.autoscaler zone name
    in
    let createInstanceTemplate () = 
      Gcloud.Compute.InstanceTemplate.get ~autoCreate:instanceTemplateConfig instanceTemplate
    in
    let createGroup () =
      Gcloud.Compute.Zone.InstanceGroupManager.exists instanceGroupManager >> fun exists ->
      if exists then
        return ()
      else
        Gcloud.Compute.Zone.InstanceGroupManager.create ~targetSize:0
                                                        ~instanceTemplate
                                                        instanceGroupManager
    in
    createInstanceTemplate () >> fun () ->
      createGroup () >> fun () ->
        Gcloud.Compute.Zone.Autoscaler.get ~autoCreate:autoscaleConfig autoscaler >> fun () ->
          if restart then
            Gcloud.Compute.Zone.InstanceGroupManager.recreateVMs instanceGroupManager
          else
            return ()
          
 end
