open LidcoreBsNode

let get_some = function
  | Some v -> v
  | None -> assert false

let partition size a =
  let len = Array.length a in
  let ret = [||] in
  let pos = ref 0 in
  while !pos < len do
    ignore(Js.Array.push
      (Js.Array.slice ~start:!pos ~end_:(!pos+size) a)
      ret);
    pos := !pos + size;
  done;
  ret

let escape s =
  Printf.sprintf "%S" s

let delete = [%bs.raw fun key obj ->
  "delete obj[key];"
]

module Json = struct
  let parse : string -> 'a Js.t = [%bs.raw{|function (x) {
    return JSON.parse(x);
  }|}]

  let parse_buf buf =
    parse (Buffer.toString buf)

  let stringify obj =
    match Js.Json.stringifyAny (Obj.magic obj) with
      | Some v -> v
      | None -> assert false
end
