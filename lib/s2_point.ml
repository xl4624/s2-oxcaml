open Core

type t = R3_vector.t [@@deriving sexp_of, unboxed_option]

let[@zero_alloc ignore] pp ppf t = R3_vector.pp ppf t
let[@zero_alloc ignore] to_string t = R3_vector.to_string t

let origin =
  R3_vector.create
    ~x:(-#0.0099994664350250197)
    ~y:#0.0025924542609324121
    ~z:#0.99994664350250195
;;

let[@inline] [@zero_alloc] of_coords ~x ~y ~z =
  let open Float_u.O in
  if x = #0.0 && y = #0.0 && z = #0.0
  then origin
  else R3_vector.normalize (R3_vector.create ~x ~y ~z)
;;

let[@inline] [@zero_alloc] x t = R3_vector.x t
let[@inline] [@zero_alloc] y t = R3_vector.y t
let[@inline] [@zero_alloc] z t = R3_vector.z t
let[@inline] [@zero_alloc] to_r3 t = t

let[@inline] [@zero_alloc] is_unit_length t =
  Float.( <= )
    (Float.abs (Float_u.to_float (R3_vector.norm2 t) -. 1.0))
    (5.0 *. Float.epsilon_float)
;;

let[@inline] [@zero_alloc] ortho a =
  let k = R3_vector.largest_abs_component a - 1 in
  let k = if k < 0 then 2 else k in
  let temp =
    let tx = #0.012 in
    let ty = #0.0053 in
    let tz = #0.00457 in
    match k with
    | 0 -> R3_vector.create ~x:#1.0 ~y:ty ~z:tz
    | 1 -> R3_vector.create ~x:tx ~y:#1.0 ~z:tz
    | 2 -> R3_vector.create ~x:tx ~y:ty ~z:#1.0
    | _ -> assert false
  in
  R3_vector.(R3_vector.normalize (R3_vector.cross a temp))
;;

let[@inline] [@zero_alloc] compare_lex a b = R3_vector.compare a b

let[@inline] [@zero_alloc] symbolic_cross_prod_sorted a b =
  let open Float_u.O in
  if R3_vector.x b <> #0.0 || R3_vector.y b <> #0.0
  then R3_vector.create ~x:(Float_u.neg (R3_vector.y b)) ~y:(R3_vector.x b) ~z:#0.0
  else if R3_vector.z b <> #0.0
  then R3_vector.create ~x:(R3_vector.z b) ~y:#0.0 ~z:#0.0
  else if R3_vector.x a <> #0.0 || R3_vector.y a <> #0.0
  then R3_vector.create ~x:(R3_vector.y a) ~y:(Float_u.neg (R3_vector.x a)) ~z:#0.0
  else R3_vector.create ~x:#1.0 ~y:#0.0 ~z:#0.0
;;

let[@inline] [@zero_alloc] symbolic_cross_prod a b =
  if compare_lex a b < 0
  then symbolic_cross_prod_sorted a b
  else R3_vector.(R3_vector.neg (symbolic_cross_prod_sorted b a))
;;

let[@zero_alloc] robust_cross_prod a b =
  let sum = R3_vector.add a b in
  let diff = R3_vector.sub b a in
  let cross = R3_vector.cross sum diff in
  if R3_vector.equal cross R3_vector.zero
  then if R3_vector.equal a b then ortho a else symbolic_cross_prod a b
  else cross
;;

let[@inline] [@zero_alloc] distance a b = R3_vector.angle a b

let[@inline] [@zero_alloc] stable_angle a b =
  let diff = R3_vector.sub a b in
  let sum = R3_vector.add a b in
  S1_angle.of_radians
    Float_u.O.(#2.0 * Float_u.atan2 (R3_vector.norm diff) (R3_vector.norm sum))
;;

let[@inline] [@zero_alloc] chord_angle_between a b =
  R3_vector.sub a b |> R3_vector.norm2 |> S1_chord_angle.of_length2
;;

let[@inline] [@zero_alloc] rotate p ~axis ~angle =
  let center = R3_vector.mul axis (R3_vector.dot p axis) in
  let dx = R3_vector.sub p center in
  let dy = R3_vector.cross axis p in
  let cos_a = Float_u.cos (S1_angle.radians angle) in
  let sin_a = Float_u.sin (S1_angle.radians angle) in
  let t1 = R3_vector.mul dx cos_a in
  let t2 = R3_vector.mul dy sin_a in
  let t3 = R3_vector.add t1 t2 in
  R3_vector.(R3_vector.normalize (R3_vector.add t3 center))
;;

let[@inline] [@zero_alloc] add a b = R3_vector.add a b
let[@inline] [@zero_alloc] sub a b = R3_vector.sub a b
let[@inline] [@zero_alloc] mul t k = R3_vector.mul t k
let[@inline] [@zero_alloc] neg t = R3_vector.neg t
let[@inline] [@zero_alloc] mul_components a b = R3_vector.mul_components a b
let[@inline] [@zero_alloc] div_components a b = R3_vector.div_components a b
let[@inline] [@zero_alloc] max a b = R3_vector.max a b
let[@inline] [@zero_alloc] min a b = R3_vector.min a b

let[@inline] [@zero_alloc] sqrt t =
  R3_vector.create
    ~x:(Float_u.sqrt (R3_vector.x t))
    ~y:(Float_u.sqrt (R3_vector.y t))
    ~z:(Float_u.sqrt (R3_vector.z t))
;;

let[@inline] [@zero_alloc] floor t =
  R3_vector.create
    ~x:(Float_u.round_down (R3_vector.x t))
    ~y:(Float_u.round_down (R3_vector.y t))
    ~z:(Float_u.round_down (R3_vector.z t))
;;

let[@inline] [@zero_alloc] ceil t =
  R3_vector.create
    ~x:(Float_u.round_up (R3_vector.x t))
    ~y:(Float_u.round_up (R3_vector.y t))
    ~z:(Float_u.round_up (R3_vector.z t))
;;

(* Matches C++ [Vector3::FRound], which uses [std::rint] (round half-to-even
   under the default IEEE rounding mode). *)
let[@inline] [@zero_alloc] fround t =
  R3_vector.create
    ~x:(Float_u.round_nearest_half_to_even (R3_vector.x t))
    ~y:(Float_u.round_nearest_half_to_even (R3_vector.y t))
    ~z:(Float_u.round_nearest_half_to_even (R3_vector.z t))
;;

let[@inline] [@zero_alloc] nan () =
  R3_vector.create ~x:(Float_u.nan ()) ~y:(Float_u.nan ()) ~z:(Float_u.nan ())
;;

let[@inline] [@zero_alloc] is_nan t =
  Float_u.is_nan (R3_vector.x t)
  || Float_u.is_nan (R3_vector.y t)
  || Float_u.is_nan (R3_vector.z t)
;;

let[@inline] [@zero_alloc] compare a b = R3_vector.compare a b
let[@inline] [@zero_alloc] equal a b = R3_vector.equal a b

let[@inline] [@zero_alloc] approx_equal ~(max_error : Packed_float_option.Unboxed.t) a b =
  let max_error = Packed_float_option.Unboxed.value max_error ~default:#1e-15 in
  let angle = R3_vector.angle a b in
  S1_angle.compare angle (S1_angle.of_radians max_error) <= 0
;;

(* 3x3 matrix stored as three column vectors. *)
type frame =
  #{ col0 : R3_vector.t
   ; col1 : R3_vector.t
   ; col2 : R3_vector.t
   }

let[@inline] [@zero_alloc] get_frame z =
  let col2 = z in
  let col1 = ortho z in
  let col0 = R3_vector.cross col1 z in
  #{ col0; col1; col2 }
;;

let[@inline] [@zero_alloc] to_frame #{ col0; col1; col2 } p =
  R3_vector.create
    ~x:(R3_vector.dot col0 p)
    ~y:(R3_vector.dot col1 p)
    ~z:(R3_vector.dot col2 p)
;;

let[@inline] [@zero_alloc] from_frame #{ col0; col1; col2 } q =
  let t1 = R3_vector.mul col0 (R3_vector.x q) in
  let t2 = R3_vector.mul col1 (R3_vector.y q) in
  let t3 = R3_vector.mul col2 (R3_vector.z q) in
  let t4 = R3_vector.add t1 t2 in
  R3_vector.add t4 t3
;;
