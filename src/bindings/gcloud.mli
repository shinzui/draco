open BsAsyncMonad
open LidcoreBsNode

val project : string

module PubSub : sig
  type config = <
    projectId: string [@bs.get nullable]
  > Js.t

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

  type msg = <
    id:        string;
    ackId:     string;
    timestamp: string;
    data:      Buffer.t
  > Js.t

  val ack  : msg -> unit
  val nack : msg -> unit
  val subscribe : subscription -> (msg -> unit) -> unit 
end

module Storage : sig
  type config = <
    projectId: string [@bs.get nullable]
  > Js.t

  type t
  type bucket
  type file

  type url_config = <
    action:  string;
    expires: int;
  > Js.t

  val init : ?config:config -> unit -> t
  val bucket : t -> string -> bucket
  val file : bucket -> string -> file
  val createReadStream : file -> Stream.readable
  val createWriteStream : file -> Stream.writable
  val getSignedUrl : config:url_config -> file -> string Callback.t 
end

module Firestore : sig
  type config = <
    projectId: string [@bs.get nullable]
  > Js.t

  type t

  val init : ?config:config -> unit -> t

  module Document : sig
    type 'a t
    exception Not_saved
    val is_saved : [`Saved|`Unsaved] t -> bool
    val saved    : [`Saved|`Unsaved] t -> [`Saved] t
    val save     : [`Saved|`Unsaved] t -> 'a Js.t -> [`Saved] t Callback.t
    val get      : [`Saved] t -> string -> 'a
    val path     : [`Saved|`Unsaved] t -> string
    val delete   : [`Saved] t -> [`Unsaved] t Callback.t
  end
  val document : t -> string -> [`Saved|`Unsaved] Document.t Callback.t

  module QuerySnapshot : sig
    type t
    val docs : t -> [`Saved] Document.t array
  end

  module Collection : sig
    type t
    val id : t -> string
    val get : t -> QuerySnapshot.t Callback.t
  end
  val collection : t -> string -> Collection.t
  val collections: t -> Collection.t array Callback.t

  module Transaction : sig
    type 'a t

    val get    : [`Read] t -> string -> [`Saved|`Unsaved] Document.t Callback.t
    val write  : [`Read] t -> [`Write] t
    val save   : [`Write] t -> [`Saved|`Unsaved] Document.t -> 'a Js.t -> [`Saved] Document.t
    val delete : [`Write] t -> [`Saved] Document.t -> [`Unsaved] Document.t
  end

  type transaction_options = <
    maxAttempts: int
  > Js.t

  val runTransaction : ?options:transaction_options -> t -> ([`Read] Transaction.t -> 'a Callback.t) -> 'a Callback.t

  module Counter : sig
    type t
    val incr : t -> int Callback.t
    val delete : t -> unit Callback.t
  end
  val counter : t -> string -> Counter.t

  val latest_cleanup : t -> float option Callback.t
  val cleanup_collection : t -> Collection.t -> unit Callback.t
end

module Compute : sig
  type config = <
    projectId: string [@bs.get nullable]
  > Js.t

  type t

  val init : ?config:config -> unit -> t

  module VM : sig
    type t

    type metadata = <
      metadata: <
        items: <
          key:   string;
          value: string
        > Js.t array
      > Js.t
    > Js.t

    val getMetadata : t -> metadata Callback.t
  end

  module Zone : sig
    type t
    val vm : t -> string -> VM.t
  end

  val zone : t -> string -> Zone.t
end
