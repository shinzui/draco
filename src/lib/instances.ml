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
    Process.on (`SIGTERM (fun () ->
      stopping := true))
  
  (* de-duplicate messages goddamn it *)
  
  (* 2 days *)
  let msg_check_expire =
    60. *. 24. *. 2.
  
  let msg_check_key id =
    {j|msg_check_$(id)|j}

  let redis_client = ref None

  let get_redis () =
    match !redis_client with
      | Some client -> client
      | None ->
          let client =
            Redis.createClient
              (Env.get ~default:"redis://localhost:6379" "REDIS_URL")
          in
          redis_client := Some client;
          client
  
  let is_duplicate id =
    let redis = get_redis () in
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
  type components = {
    compute: Gcloud.Compute.t;
    zone: Gcloud.Compute.Zone.t;
    instanceTemplate: Gcloud.Compute.InstanceTemplate.t;
    instanceGroupManager: Gcloud.Compute.Zone.InstanceGroupManager.t;
    autoscaler: Gcloud.Compute.Zone.Autoscaler.t
  }

  let components ~projectId ~zone name =
    let compute =
      Gcloud.Compute.init
        ~config:(Gcloud.config ~projectId ()) ()
    in
    Gcloud.Compute.pushInterceptor compute [%bs.obj{
      request = fun reqOps ->
        reqOps##uri #= (Js.String.replace "/v1/" "/beta/" reqOps##uri);
        reqOps
    }];
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
    {compute; zone; instanceTemplate;
     instanceGroupManager; autoscaler}

  let initialize ~projectId ~serviceAccount ~zone
                 ~instanceTemplate ~autoscaler name =
    let instanceTemplateConfig =
      Obj.magic instanceTemplate
    in
    instanceTemplateConfig##properties##metadata #= [%bs.obj{
      items = [|[%bs.obj{
        key  = "mode";
        value = name
      }]|]
    }];
    instanceTemplateConfig##properties##serviceAccounts #= [|[%bs.obj{
      email = serviceAccount;
      scopes = [|"https://www.googleapis.com/auth/cloud-platform"|]
    }]|];
    let autoscalerConfig =
      Obj.magic autoscaler
    in
    autoscalerConfig##target #= name;
    autoscalerConfig##name #= name;
    autoscalerConfig##zone #= zone;
    let {compute; zone; instanceTemplate;
         instanceGroupManager; autoscaler} =
      components ~projectId ~zone name 
    in
    let createInstanceTemplate () = 
      async_unless
        (Gcloud.Compute.InstanceTemplate.exists instanceTemplate)
        (fun () ->
          Gcloud.Compute.createInstanceTemplate compute name instanceTemplateConfig >>
            Gcloud.Compute.InstanceTemplate.get)
    in
    let createGroup () =
      async_unless
        (Gcloud.Compute.Zone.InstanceGroupManager.exists instanceGroupManager)
        (fun () ->
          Gcloud.Compute.Zone.createInstanceGroupManager ~targetSize:0
                                                         ~instanceTemplate
                                                         zone
                                                         name >>
            Gcloud.Compute.Zone.InstanceGroupManager.get)
    in
    let createAutoscaler () =
      async_unless
        (Gcloud.Compute.Zone.Autoscaler.exists autoscaler)
        (fun () ->
          discard(Gcloud.Compute.Zone.createAutoscaler zone name autoscalerConfig))
    in
    createInstanceTemplate () >> createGroup >> createAutoscaler
    
  let restart ~projectId ~zone name =
    let {instanceGroupManager} =
      components ~projectId ~zone name
    in
    Gcloud.Compute.Zone.InstanceGroupManager.recreateVMs instanceGroupManager

  let destroy ~projectId ~zone name =
    let {instanceTemplate;
         instanceGroupManager;
         autoscaler} =
      components ~projectId ~zone name
    in
    let deleteAutoscaler =
      async_if
        (Gcloud.Compute.Zone.Autoscaler.exists autoscaler)
        (fun () ->
          Gcloud.Compute.Zone.Autoscaler.delete autoscaler >> fun () ->
            repeat
              (fun () ->
                Gcloud.Compute.Zone.Autoscaler.exists autoscaler) return)
    in
    let deleteGroup () =
      async_if
        (Gcloud.Compute.Zone.InstanceGroupManager.exists instanceGroupManager)
        (fun () ->
          Gcloud.Compute.Zone.InstanceGroupManager.delete instanceGroupManager >> fun () ->
            repeat
              (fun () ->
                Gcloud.Compute.Zone.InstanceGroupManager.exists instanceGroupManager) return)
    in
    let deleteInstanceTemplate () =
      async_if
        (Gcloud.Compute.InstanceTemplate.exists instanceTemplate)
        (fun () ->
          Gcloud.Compute.InstanceTemplate.delete instanceTemplate >> fun () ->
            repeat
              (fun () ->
                Gcloud.Compute.InstanceTemplate.exists instanceTemplate) return)
    in
    deleteAutoscaler >> deleteGroup >> deleteInstanceTemplate
 end
