open LidcoreBsExpress
open LidcoreBsNode
open BsAsyncMonad
open BsAsyncMonad.Callback

let offline =
  try
    Sys.getenv "OFFLINE_DEPLOY" = "true"
  with Not_found -> false

external make_error : string -> Js.Exn.t = "Error" [@@bs.new]

let error_handler = ref (fun _ -> ())

let on_error h = error_handler := h

let maxRetries = 10

let pubsub = Gcloud.PubSub.init ()

let requeue ~msg topic =
  let msg =
    Obj.magic msg
  in
  let retry =
    match Js.toOption msg##retry with
      | Some r -> r + 1
      | None -> 1
  in
  msg##retry #= retry;
  let msg =
    Utils.Json.stringify msg
  in
  if retry >= maxRetries then
    Callback.fail
      (Obj.magic
        (make_error {j|[$(topic)] Message reached max retries: $(msg)|j}))
  else
   begin
    Logger.info {j|Retrying message: $(msg)|j};
    Gcloud.PubSub.publish pubsub topic msg
   end
  
module Functions =  struct
  type fn = Firebase.Functions.t
  
  let wrap ~error cb = fun [@bs] err ret ->
    match Js.toOption err with
      | Some (Js.Exn.Error e) ->
          !error_handler e;
          error cb
      | Some exn ->
          let e =
            try
              make_error (Printexc.to_string exn)
            with _ -> Obj.magic exn
          in
          !error_handler e;
          error cb
      | None ->
          cb err ret [@bs]
  
  module Http = struct
    type t = Express.t
  
    type request = Express.request
  
    let init = Express.init ~useCors:true
  
    let param req lbl =
      Js.Dict.get (Express.params req) lbl
    let params req = Obj.magic (Express.params req)
    let body req = Obj.magic (Express.body req)
    let query req = Js.Dict.get (Express.query req)
  
    type response = <
      statusCode: int;
      headers: string Js.Dict.t [@bs.get nullable];
      body: string
    > Js.t
  
    type handler = request -> response Callback.t
  
    exception Error of response
  
    external make_response : statusCode:int -> headers:(string Js.Dict.t Js.Null_undefined.t) -> body:string -> unit -> response = "" [@@bs.obj]
  
    let make_error ~statusCode body =
      Error (make_response ~statusCode ~headers:Js.Null_undefined.null ~body ())
  
    let error ~code msg =
      Callback.fail (make_error ~statusCode:code msg)
  
    let response ?(code=200) ?headers msg =
      let headers =
        match headers with  
          | None -> Js.Null_undefined.null
          | Some h ->
              let headers = Js.Dict.empty () in
              Hashtbl.iter (Js.Dict.set headers) h;
              Js.Null_undefined.return headers
      in
      match Js.Json.stringifyAny msg with
        | Some body ->
            return (make_response ~statusCode:code ~headers ~body ())
        | None ->
            Callback.fail (make_error ~statusCode:500 "Invalid message!")
  
    let wrap handler cb =
      let error =
        return
          (make_response ~statusCode:500 ~headers:Js.Null_undefined.null ~body:"Internal server error" ())
      in
      let cb = wrap ~error cb in
      let cb = fun [@bs] err ret ->
        match Js.toOption err with
          | Some (Error e) ->
              cb Js.Nullable.null e [@bs]
          | _ ->
              cb err ret [@bs]
      in
      handler cb
  
    let authenticate req =
      let token =
        if offline then "blabla" else
          Env.get "AUTHENTICATION_TOKEN"
      in
      let error =
        error ~code:401 "Unauthorized"
      in
      match Js.Dict.get (Express.headers req) "authorization" with
        | None -> error
        | Some authorization ->
            let re =
              [%re "/Token token=(.+)/"]
            in
            begin
             match re |> Js.Re.exec authorization with
               | Some t when (Js.Re.captures t).(1) = Js.Nullable.return token ->
                   return ()
               | _ -> error
            end
  
    let send_response : int -> string Js.Dict.t Js.null_undefined -> string -> Express.response -> unit = [%bs.raw{|function (code, headers, body, resp) {
      var key;
      headers = headers || {};
      for (key in headers)
        resp.append(key, headers[key]);
      resp.status(code).send(body);
    }|}]
  
    let add_route meth ?(auth=false) app route handler =
      let meth, fn =
        match meth with
          | `Get  -> "GET", Express.get
          | `Post -> "POST", Express.post
          | `Put  -> "PUT", Express.put
      in
      let handler req =
        (if auth then
           authenticate req
         else
           return ()) >> fun _ ->
           handler req
      in  
      let handler req =
        wrap (handler req)
      in
      fn app route (fun req resp ->
        let url = Express.originalUrl req in
        Logger.info {j|Serving $(meth) $(url)|j};
        let cb = fun [@bs] err ret ->
          match Js.toOption err with
            | Some _ -> assert false
            | None -> send_response ret##statusCode ret##headers ret##body resp
        in
        handler req cb)
  
    let get = add_route `Get
    let post = add_route `Post
    let put = add_route `Put
  
    let export app =
      Firebase.Functions.Https.from_express app
  end
  
  module Event = struct
    type handler = unit Callback.t
  
    let subscribe topic fn =
      Firebase.Functions.PubSub.on_publish topic (fun [@bs] message _ ->
        let json = Firebase.Functions.PubSub.json message in
        let fn =
          fn json ||> fun exn ->
            requeue ~msg:json topic >> fun () ->
              Callback.fail exn
        in
        Callback.to_promise fn)
  end
end

module Instances = struct
  let exceptionHandler exn =
    !error_handler (Obj.magic exn)

  let instances = Hashtbl.create 10
  let register label handler =
    Hashtbl.add instances label handler

  module C = Gcloud.Compute

  let run () =
    let zone =
      Env.get ~default:"us-west1-b" "zone"
    in
    let instance =
       C.Zone.vm (C.zone (C.init ()) zone) (Os.hostname ())
    in
    Callback.finish ~exceptionHandler (C.VM.getMetadata instance >> fun meta ->
      let items =
        Array.to_list meta##metadata##items
      in
      let label =
        let data =
          List.find (fun el -> el##key = "draco_instance_type") items
        in
        data##value
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
        Utils.Json.parse_buf msg##data
      in
      requeue ~msg topic
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
              handler msg##data >| fun _ ->
                Gcloud.PubSub.ack msg
            in
            let handler () =
              handler () ||> fun exn ->
                Gcloud.PubSub.ack msg;
                requeue msg >> fun _ ->
                  Callback.fail exn
            in
            let handler =
              let id = msg##id in
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
