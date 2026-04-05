open Core
open Stdlib_upstream_compatible

let load_fixture filename =
  let path = Filename.concat "fixtures" filename in
  Yojson.Safe.from_file path
;;

let member = Yojson.Safe.Util.member
let to_list = Yojson.Safe.Util.to_list

let float_of_json_exn = function
  | `Float f -> f
  | `Int i -> Float.of_int i
  | j -> failwith (sprintf "expected float, got %s" (Yojson.Safe.to_string j))
;;

let float_u_of_json_exn j = Float_u.of_float (float_of_json_exn j)

let bool_of_json_exn = function
  | `Bool b -> b
  | j -> failwith (sprintf "expected bool, got %s" (Yojson.Safe.to_string j))
;;

let string_of_json_exn = function
  | `String s -> s
  | j -> failwith (sprintf "expected string, got %s" (Yojson.Safe.to_string j))
;;

let int_of_json_exn = function
  | `Int i -> i
  | `Intlit s -> int_of_string s
  | `Float f -> Float.iround_nearest_exn f
  | j -> failwith (sprintf "expected int, got %s" (Yojson.Safe.to_string j))
;;

let int64_of_json_exn = function
  | `String s -> Int64.of_string s
  | `Int i -> Int64.of_int i
  | `Intlit s -> Int64.of_string s
  | j -> failwith (sprintf "expected int64, got %s" (Yojson.Safe.to_string j))
;;

(** Little-endian [float] at [off] in a byte string (for S2 binary fixtures). *)
let get_le_f64_from_string s off =
  let rec loop i acc =
    if Int.( = ) i 8
    then Stdlib.Int64.float_of_bits acc
    else (
      let byte = Stdlib.Char.code (String.unsafe_get s (off + i)) in
      loop
        (i + 1)
        (Stdlib.Int64.logor
           acc
           (Stdlib.Int64.shift_left (Stdlib.Int64.of_int byte) (8 * i))))
  in
  loop 0 0L
;;

let check_float_exact msg ~expected ~actual =
  if not (Float.( = ) expected actual)
  then Alcotest.fail (sprintf "%s: expected %.17g, got %.17g" msg expected actual)
;;

let check_float_u_exact msg ~expected ~actual =
  if not (Float_u.equal expected actual)
  then
    Alcotest.fail
      (sprintf
         "%s: expected %s, got %s"
         msg
         (Float_u.to_string expected)
         (Float_u.to_string actual))
;;

let check_bool msg ~expected ~actual = Alcotest.(check bool) msg expected actual

(** Two-element JSON array of floats: low and high endpoints (R1). *)
let r1_interval_of_json j =
  let lo, hi =
    match to_list j with
    | [ lo; hi ] -> float_of_json_exn lo, float_of_json_exn hi
    | _ -> failwith "expected [lo, hi]"
  in
  S2.R1_interval.create ~lo:(Float_u.of_float lo) ~hi:(Float_u.of_float hi)
;;

(** Two-element JSON array of floats: low and high endpoints (S1). *)
let s1_interval_of_json j =
  match to_list j with
  | [ lo; hi ] ->
    S2.S1_interval.create ~lo:(float_u_of_json_exn lo) ~hi:(float_u_of_json_exn hi)
  | _ ->
    (match failwith "expected [lo, hi]" with
     | (_ : Nothing.t) -> .)
;;

(** Two-element JSON array of floats (x, y) for R2 points. *)
let r2_point_of_json j =
  match to_list j with
  | [ x; y ] -> S2.R2_point.create ~x:(float_u_of_json_exn x) ~y:(float_u_of_json_exn y)
  | _ ->
    (match failwith "expected [x, y]" with
     | (_ : Nothing.t) -> .)
;;

(** JSON object with [x] and [y] fields, each a two-float interval. *)
let r2_rect_of_json j =
  let x_intv = member "x" j in
  let y_intv = member "y" j in
  let x_lo = float_u_of_json_exn (List.nth_exn (to_list x_intv) 0) in
  let x_hi = float_u_of_json_exn (List.nth_exn (to_list x_intv) 1) in
  let y_lo = float_u_of_json_exn (List.nth_exn (to_list y_intv) 0) in
  let y_hi = float_u_of_json_exn (List.nth_exn (to_list y_intv) 1) in
  let x = S2.R1_interval.create ~lo:x_lo ~hi:x_hi in
  let y = S2.R1_interval.create ~lo:y_lo ~hi:y_hi in
  S2.R2_rect.create_intervals_exn ~x ~y
;;

(** Three-element JSON array of floats (x, y, z) for R3 / cell fixtures. *)
let r3_vector_of_json j =
  match to_list j with
  | [ x; y; z ] ->
    S2.R3_vector.create
      ~x:(float_u_of_json_exn x)
      ~y:(float_u_of_json_exn y)
      ~z:(float_u_of_json_exn z)
  | _ ->
    (match failwith "expected [x, y, z]" with
     | (_ : Nothing.t) -> .)
;;

(** Chord angle from squared length. *)
let s1_chord_angle_of_json j = S2.S1_chord_angle.of_length2 (float_of_json_exn j)

(** Hex string cell id from golden data. C++ [std::to_string] on [uint64_t] can exceed
    [max_int] of signed [Int64]. *)
let s2_cell_id_of_json j =
  let s = string_of_json_exn j in
  S2.S2_cell_id.of_int64 (Int64.of_string ("0u" ^ s))
;;

let check_float ?(eps = 1e-15) msg ~expected ~actual =
  match Float.is_nan expected, Float.is_nan actual with
  | true, true -> ()
  | true, false | false, true ->
    Alcotest.fail (sprintf "%s: NaN mismatch (expected %g, got %g)" msg expected actual)
  | false, false ->
    if Float.is_inf expected || Float.is_inf actual
    then check_float_exact msg ~expected ~actual
    else (
      let diff = Float.abs (expected -. actual) in
      if Float.( > ) diff eps
      then
        Alcotest.fail
          (sprintf "%s: expected %.17g, got %.17g (diff %g)" msg expected actual diff))
;;

let check_float_u ?(eps = 1e-15) msg ~expected ~actual =
  check_float
    ~eps
    msg
    ~expected:(Float_u.to_float expected)
    ~actual:(Float_u.to_float actual)
;;

let check_float_u_or_infinity msg expected_json actual =
  match expected_json with
  | `Null -> Alcotest.(check bool) (msg ^ " is inf") true (Float_u.is_infinite actual)
  | j -> check_float_u msg ~expected:(float_u_of_json_exn j) ~actual
;;

let check_r1_interval_exact msg ~expected ~actual =
  check_float_u_exact
    (msg ^ " lo")
    ~expected:(S2.R1_interval.lo expected)
    ~actual:(S2.R1_interval.lo actual);
  check_float_u_exact
    (msg ^ " hi")
    ~expected:(S2.R1_interval.hi expected)
    ~actual:(S2.R1_interval.hi actual)
;;

let check_s1_interval_exact msg ~expected ~actual =
  check_float_u_exact
    (msg ^ " lo")
    ~expected:(S2.S1_interval.lo expected)
    ~actual:(S2.S1_interval.lo actual);
  check_float_u_exact
    (msg ^ " hi")
    ~expected:(S2.S1_interval.hi expected)
    ~actual:(S2.S1_interval.hi actual)
;;

let check_r2_point_exact msg ~(expected : S2.R2_point.t) ~actual =
  check_float_u_exact
    (msg ^ " x")
    ~expected:(S2.R2_point.x expected)
    ~actual:(S2.R2_point.x actual);
  check_float_u_exact
    (msg ^ " y")
    ~expected:(S2.R2_point.y expected)
    ~actual:(S2.R2_point.y actual)
;;

let check_r2_point ?(eps = 1e-15) msg ~(expected : S2.R2_point.t) ~actual =
  check_float_u
    ~eps
    (msg ^ " x")
    ~expected:(S2.R2_point.x expected)
    ~actual:(S2.R2_point.x actual);
  check_float_u
    ~eps
    (msg ^ " y")
    ~expected:(S2.R2_point.y expected)
    ~actual:(S2.R2_point.y actual)
;;

let check_r3_vector_exact msg ~(expected : S2.R3_vector.t) ~actual =
  check_float_u_exact
    (msg ^ " x")
    ~expected:(S2.R3_vector.x expected)
    ~actual:(S2.R3_vector.x actual);
  check_float_u_exact
    (msg ^ " y")
    ~expected:(S2.R3_vector.y expected)
    ~actual:(S2.R3_vector.y actual);
  check_float_u_exact
    (msg ^ " z")
    ~expected:(S2.R3_vector.z expected)
    ~actual:(S2.R3_vector.z actual)
;;

let check_r3_vector msg ~(expected : S2.R3_vector.t) ~actual =
  check_float_u
    (msg ^ " x")
    ~expected:(S2.R3_vector.x expected)
    ~actual:(S2.R3_vector.x actual);
  check_float_u
    (msg ^ " y")
    ~expected:(S2.R3_vector.y expected)
    ~actual:(S2.R3_vector.y actual);
  check_float_u
    (msg ^ " z")
    ~expected:(S2.R3_vector.z expected)
    ~actual:(S2.R3_vector.z actual)
;;
