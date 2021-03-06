open BsAsyncMonad

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
  if retry >= !Config.maxMessageRetries then
    Callback.fail
      (Obj.magic
        (JsError.make {j|[$(topic)] Message reached max retries: $(msg)|j}))
  else
   begin
    Logger.info {j|Retrying message: $(msg)|j};
    Gcloud.PubSub.publish pubsub topic msg
   end
