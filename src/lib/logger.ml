let handlers = Js.Dict.empty ()

let on_info fn =
  Js.Dict.set handlers "info" (Obj.magic fn)

let on_error fn =
  Js.Dict.set handlers "error" (Obj.magic fn)

let info s =
  Js.log s;
  match Js.Dict.get handlers "info" with
    | Some fn ->
        let fn : 'a -> unit = Obj.magic fn in
        fn s
    | None -> ()

let error_log : 'a -> unit [@bs] = [%bs.raw fun m ->
  "console.error(m);"
]

let error s =
  error_log (Obj.magic s) [@bs];
  match Js.Dict.get handlers "error" with
    | Some fn ->
        let fn : 'a -> unit = Obj.magic fn in
        fn s
    | None -> ()

type process
external process : process = "" [@@bs.val]
external on : process -> string -> (exn -> unit) -> unit = "" [@@bs.send]

let () =
  on process "uncaughtException" (fun exn ->
    error (Obj.magic exn);
    raise exn)
