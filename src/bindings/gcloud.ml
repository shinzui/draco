open LidcoreBsNode
open BsAsyncMonad
open BsAsyncMonad.Callback

let project =
  Env.get ~default:"draco-dev" "PROJECT"

type config = {
  projectId : string [@bs.optional]
} [@@bs.deriving abstract]

let default_config =
  config ~projectId:project ()

module PubSub = struct
  type t
  external pubsub : config -> t = "@google-cloud/pubsub" [@@bs.module] [@@bs.new]

  let init ?(config=default_config) () =
    pubsub config

  type topic
  external topic : t -> string -> topic = "" [@@bs.send]

  let get_topic pubsub name =
    return (topic pubsub name)

  type subscription

  type flow_control = {
    maxBytes:    int [@bs.optional];
    maxMessages: int [@bs.optional]
  } [@@bs.deriving abstract]

  type subscription_config = {
    flowControl: flow_control [@bs.optional];
    maxConnections: int [@bs.optional]
  } [@@bs.deriving abstract]

  external subscription : topic -> string -> subscription_config -> subscription = "" [@@bs.send]
  external exists : subscription -> bool Callback.callback -> unit = "" [@@bs.send]
  external create : subscription -> unit Callback.callback -> unit = "" [@@bs.send] 

  let subscription ~config ~topic client name =
    get_topic client topic >> fun topic ->
      let subscription = subscription topic name config in
      exists subscription >> fun ret ->
      (if ret then return () else
        create subscription) >> fun _ ->
          return subscription

  type msg = {
    id:        string;
    ackId:     string;
    timestamp: string;
    data:      Buffer.t
  } [@@bs.deriving abstract]
  external ack : msg -> unit = "" [@@bs.send]
  external nack : msg -> unit = "" [@@bs.send]
  external on : subscription -> string -> (msg -> unit) -> unit = "" [@@bs.send]

  let subscribe subscription handler =
    on subscription "message" handler

  type publisher
  external publisher : topic -> publisher = "" [@@bs.send]
  external publish : publisher -> Buffer.t -> unit callback -> unit = "" [@@bs.send]

  let publish client name data =
    let data = Buffer.from data in
    get_topic client name >> fun topic ->
      let publisher = publisher topic in
      publish publisher data 


  type publisher_client
  external publisher_client : unit -> publisher_client = "PublisherClient" [@@bs.new] [@@bs.scope "v1"] [@@bs.module "@google-cloud/pubsub"]

  external topicPath : publisher_client -> string -> string -> string = "" [@@bs.send]

  type message_element = {
    element_data: Buffer.t [@bs.as "data"];
  } [@@bs.deriving abstract]

  type publish_request = {
    topic: string;
    messages: message_element array
  } [@@bs.deriving abstract]

  external publishBatch : publisher_client -> publish_request -> unit callback -> unit = "publish" [@@bs.send] 
  let publishBatch ~projectId ~topic messages =
    let client = publisher_client () in
    let topic =
      topicPath client projectId topic
    in
    let messages =
      Array.map (fun msg ->
                   let element_data = Buffer.from msg in
                   message_element ~element_data)
                 messages
    in
    let request =
      publish_request ~topic ~messages
    in
    publishBatch client request
end

module Compute = struct
  type t

  external init : config -> t = "@google-cloud/compute" [@@bs.module] [@@bs.new]
  let init ?(config=default_config) () = init config

  external interceptors : t -> 'a Js.t array = "" [@@bs.get]

  let pushInterceptor c x =
    ignore(Js.Array.push x (interceptors c))

  module InstanceTemplate = struct
    type t

    external exists : t -> bool Callback.callback -> unit = "" [@@bs.send]
    external get    : t -> unit Callback.callback -> unit = "" [@@bs.send]
    external delete : t -> unit Callback.callback -> unit = "" [@@bs.send]
  end
  external instanceTemplate : t -> string -> InstanceTemplate.t = "" [@@bs.send]
  external createInstanceTemplate : t -> string -> 'a Js.t -> InstanceTemplate.t Callback.callback -> unit = "" [@@bs.send]

  module Zone = struct
    type t

    module Autoscaler = struct
      type t
      external exists : t -> bool Callback.callback -> unit = "" [@@bs.send]
      external delete : t -> unit Callback.callback -> unit = "" [@@bs.send]
    end
    external autoscaler : t -> string -> Autoscaler.t = "" [@@bs.send]
    external createAutoscaler : t -> string -> 'a Js.t -> Autoscaler.t Callback.callback -> unit = "" [@@bs.send]

    module InstanceGroupManager = struct
      type t
      external exists : t -> bool Callback.callback -> unit = "" [@@bs.send]
      external get    : t -> unit Callback.callback -> unit = "" [@@bs.send]
      external delete : t -> unit Callback.callback -> unit = "" [@@bs.send]
      external recreateVMs : t -> unit Callback.callback -> unit = "" [@@bs.send]
    end
    external instanceGroupManager : t -> string -> InstanceGroupManager.t = "" [@@bs.send]
    external createInstanceGroupManager : t -> string -> InstanceTemplate.t -> int -> 'a Js.t Js.Nullable.t -> InstanceGroupManager.t Callback.callback -> unit = "" [@@bs.send]
    let createInstanceGroupManager ?options ~targetSize ~instanceTemplate t name =
      let options =
        Js.Nullable.fromOption options
      in
      createInstanceGroupManager t name instanceTemplate targetSize options

    module VM = struct
      type t

      type item = {
        key:   string;
        value: string
      } [@@bs.deriving abstract]

      type items = {
        items: item array
      } [@@bs.deriving abstract]

      type metadata = {
        metadata: items
      } [@@bs.deriving abstract]

      external getMetadata : t -> metadata Callback.callback -> unit = "" [@@bs.send]
    end
    external vm : t -> string -> VM.t = "" [@@bs.send]
  end
  external zone : t -> string -> Zone.t = "" [@@bs.send]
end
