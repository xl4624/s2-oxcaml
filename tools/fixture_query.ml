open Core

(* Parse a path like "faces[0].edges_raw[1].x" into steps. *)
type step =
  | Key of string
  | Index of int

let parse_path path =
  (* Split on '.' but keep bracket expressions attached to the preceding key. *)
  let buf = Buffer.create 32 in
  let steps = ref [] in
  let flush_buf () =
    let s = Buffer.contents buf in
    Buffer.clear buf;
    if not (String.is_empty s) then steps := Key s :: !steps
  in
  let i = ref 0 in
  let n = String.length path in
  while !i < n do
    let c = path.[!i] in
    match c with
    | '.' ->
      flush_buf ();
      incr i
    | '[' ->
      flush_buf ();
      incr i;
      let j = ref !i in
      while !j < n && Char.( <> ) path.[!j] ']' do
        incr j
      done;
      let idx = Int.of_string (String.sub path ~pos:!i ~len:(!j - !i)) in
      steps := Index idx :: !steps;
      i := !j + 1
    | _ ->
      Buffer.add_char buf c;
      incr i
  done;
  flush_buf ();
  List.rev !steps
;;

let rec navigate json steps =
  match steps with
  | [] -> json
  | Key k :: rest ->
    (match json with
     | `Assoc pairs ->
       (match List.Assoc.find pairs ~equal:String.equal k with
        | Some v -> navigate v rest
        | None ->
          let keys = List.map pairs ~f:fst |> String.concat ~sep:", " in
          eprintf "Key %S not found. Available: %s\n" k keys;
          exit 1)
     | _ ->
       eprintf "Expected object for key %S, got %s\n" k (Yojson.Safe.to_string json);
       exit 1)
  | Index i :: rest ->
    (match json with
     | `List items ->
       (match List.nth items i with
        | Some v -> navigate v rest
        | None ->
          eprintf "Index %d out of range (length %d)\n" i (List.length items);
          exit 1)
     | _ ->
       eprintf "Expected array for index %d, got %s\n" i (Yojson.Safe.to_string json);
       exit 1)
;;

let print_keys json =
  match json with
  | `Assoc pairs ->
    List.iter pairs ~f:(fun (k, v) ->
      let type_hint =
        match v with
        | `Assoc _ -> "object"
        | `List items -> sprintf "array[%d]" (List.length items)
        | `Float f -> sprintf "float(%g)" f
        | `Int i -> sprintf "int(%d)" i
        | `String s -> sprintf "string(%S)" s
        | `Bool b -> sprintf "bool(%b)" b
        | `Null -> "null"
        | `Intlit s -> sprintf "intlit(%s)" s
        | `Tuple _ -> "tuple"
        | `Variant _ -> "variant"
      in
      printf "  %-30s  %s\n" k type_hint)
  | `List items ->
    List.iteri items ~f:(fun i v ->
      let type_hint =
        match v with
        | `Assoc _ -> "object"
        | `List sub -> sprintf "array[%d]" (List.length sub)
        | _ -> Yojson.Safe.to_string v
      in
      printf "  [%d]  %s\n" i type_hint)
  | _ -> printf "%s\n" (Yojson.Safe.to_string json)
;;

let usage () =
  eprintf
    {|Usage:
  fixture_query <fixture.json> <path>        print value at path
  fixture_query <fixture.json> <path> --keys list keys/indices at path
  fixture_query <fixture.json> --keys        list top-level keys

Path syntax:
  faces[0].edges_raw[1]   array index with brackets
  faces[0].uv.x           nested object keys

Examples:
  fixture_query test/fixtures/s2cell.json "faces[0].edges_raw[0]"
  fixture_query test/fixtures/s2cell.json "faces[0]" --keys
  fixture_query test/fixtures/s2cellid.json --keys
|};
  exit 1
;;

let () =
  let args = Sys.get_argv () |> Array.to_list |> List.tl_exn in
  match args with
  | [ file; "--keys" ] ->
    let json = Yojson.Safe.from_file file in
    print_keys json
  | [ file; path ] ->
    let json = Yojson.Safe.from_file file in
    let steps = parse_path path in
    let value = navigate json steps in
    printf "%s\n" (Yojson.Safe.pretty_to_string value)
  | [ file; path; "--keys" ] ->
    let json = Yojson.Safe.from_file file in
    let steps = parse_path path in
    let value = navigate json steps in
    print_keys value
  | _ -> usage ()
;;
