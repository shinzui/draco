open BsAsyncMonad
open LidcoreBsNode

val project : string

type config = {
  projectId : string [@bs.optional]
} [@@bs.deriving abstract]

module PubSub : sig
  type t

  val init : ?config:config -> unit -> t

  val publish : t -> string -> string -> unit Callback.t

  val publishBatch : projectId:string -> topic:string -> string array -> unit Callback.t

  type subscription

  type flow_control = {
    maxBytes:    int [@bs.optional];
    maxMessages: int [@bs.optional]
  } [@@bs.deriving abstract]

  type subscription_config = {
    flowControl: flow_control [@bs.optional];
    maxConnections: int [@bs.optional]
  } [@@bs.deriving abstract]

  val subscription : config:subscription_config -> topic:string -> t -> string -> subscription Callback.t 

  type msg = {
    id:        string;
    ackId:     string;
    timestamp: string;
    data:      Buffer.t
  } [@@bs.deriving abstract]

  val ack  : msg -> unit
  val nack : msg -> unit
  val subscribe : subscription -> (msg -> unit) -> unit 
end

module Compute : sig
  type t

  val init : ?config:config -> unit -> t

  module VM : sig
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

    val getMetadata : t -> metadata Callback.t
  end

  module Zone : sig
    type t
    val vm : t -> string -> VM.t
  end

  val zone : t -> string -> Zone.t
end
