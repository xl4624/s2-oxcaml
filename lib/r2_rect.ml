open Core

type t =
  #{ x : R1_interval.t
   ; y : R1_interval.t
   }
[@@deriving sexp_of]

let[@zero_alloc ignore] pp ppf t =
  Format.fprintf
    ppf
    "{x=%s, y=%s}"
    (R1_interval.to_string t.#x)
    (R1_interval.to_string t.#y)
;;

let[@zero_alloc ignore] to_string t =
  sprintf "{x=%s, y=%s}" (R1_interval.to_string t.#x) (R1_interval.to_string t.#y)
;;

let[@inline] [@zero_alloc] is_valid t =
  Bool.equal (R1_interval.is_empty t.#x) (R1_interval.is_empty t.#y)
;;

module Option = struct
  type value = t
  type nonrec t = t

  let none =
    #{ x = R1_interval.create ~lo:(Float_u.nan ()) ~hi:(Float_u.nan ())
     ; y = R1_interval.create ~lo:(Float_u.nan ()) ~hi:(Float_u.nan ())
     }
  ;;

  let[@inline] [@zero_alloc] is_none t = Float_u.is_nan (R1_interval.lo t.#x)
  let[@inline] [@zero_alloc] is_some t = not (is_none t)
  let[@inline] [@zero_alloc] some v = v
  let[@inline] [@zero_alloc] unchecked_value t = t
  let[@inline] [@zero_alloc] value t ~default = if is_none t then default else t

  let[@inline] [@zero_alloc] value_exn t =
    if is_none t
    then (
      match raise_s [%message "R2_rect.Option.value_exn: none"] with
      | (_ : Nothing.t) -> .)
    else t
  ;;

  module Optional_syntax = struct
    module Optional_syntax = struct
      let[@zero_alloc] is_none t = is_none t
      let[@zero_alloc] unsafe_value t = unchecked_value t
    end
  end

  let%template[@alloc a = (heap, stack)] [@inline] [@zero_alloc ignore] sexp_of_t t
    : Sexp.t
    =
    if is_none t then sexp_of_string "None" else sexp_of_t t [@exclave_if_stack a]
  ;;
end

let[@inline] [@zero_alloc] create ~(lo : R2_point.t) ~(hi : R2_point.t) : Option.t =
  let x = R1_interval.create ~lo:(R2_point.x lo) ~hi:(R2_point.x hi) in
  let y = R1_interval.create ~lo:(R2_point.y lo) ~hi:(R2_point.y hi) in
  let t = #{ x; y } in
  if is_valid t then t else Option.none
;;

let[@inline] [@zero_alloc] create_intervals ~(x : R1_interval.t) ~(y : R1_interval.t)
  : Option.t
  =
  let t = #{ x; y } in
  if is_valid t then t else Option.none
;;

let[@inline] [@zero_alloc] create_exn ~(lo : R2_point.t) ~(hi : R2_point.t) : t =
  let x = R1_interval.create ~lo:(R2_point.x lo) ~hi:(R2_point.x hi) in
  let y = R1_interval.create ~lo:(R2_point.y lo) ~hi:(R2_point.y hi) in
  let t = #{ x; y } in
  if is_valid t
  then t
  else (
    match raise_s [%message "R2Rect: both intervals must be empty or non-empty"] with
    | (_ : Nothing.t) -> .)
;;

let[@inline] [@zero_alloc] create_intervals_exn ~(x : R1_interval.t) ~(y : R1_interval.t)
  : t
  =
  let t = #{ x; y } in
  if is_valid t
  then t
  else (
    match raise_s [%message "R2Rect: both intervals must be empty or non-empty"] with
    | (_ : Nothing.t) -> .)
;;

let empty = #{ x = R1_interval.empty; y = R1_interval.empty }

let full =
  #{ x = R1_interval.create ~lo:(-#1.0) ~hi:#1.0
   ; y = R1_interval.create ~lo:(-#1.0) ~hi:#1.0
   }
;;

let[@inline] [@zero_alloc] from_center_size ~(center : R2_point.t) ~(size : R2_point.t)
  : t
  =
  let open Float_u.O in
  #{ x =
       R1_interval.create
         ~lo:(R2_point.x center - (#0.5 * R2_point.x size))
         ~hi:(R2_point.x center + (#0.5 * R2_point.x size))
   ; y =
       R1_interval.create
         ~lo:(R2_point.y center - (#0.5 * R2_point.y size))
         ~hi:(R2_point.y center + (#0.5 * R2_point.y size))
   }
;;

let[@inline] [@zero_alloc] from_point (p : R2_point.t) : t =
  #{ x = R1_interval.from_point (R2_point.x p)
   ; y = R1_interval.from_point (R2_point.y p)
   }
;;

let[@inline] [@zero_alloc] from_point_pair (p1 : R2_point.t) (p2 : R2_point.t) : t =
  #{ x = R1_interval.from_point_pair (R2_point.x p1) (R2_point.x p2)
   ; y = R1_interval.from_point_pair (R2_point.y p1) (R2_point.y p2)
   }
;;

let[@inline] [@zero_alloc] x t = t.#x
let[@inline] [@zero_alloc] y t = t.#y

let[@inline] [@zero_alloc] lo t =
  R2_point.create ~x:(R1_interval.lo t.#x) ~y:(R1_interval.lo t.#y)
;;

let[@inline] [@zero_alloc] hi t =
  R2_point.create ~x:(R1_interval.hi t.#x) ~y:(R1_interval.hi t.#y)
;;

let[@inline] [@zero_alloc] is_empty t = R1_interval.is_empty t.#x

let[@inline] [@zero_alloc] get_vertex t k =
  let k = k % 4 in
  match k with
  | 0 -> R2_point.create ~x:(R1_interval.lo t.#x) ~y:(R1_interval.lo t.#y)
  | 1 -> R2_point.create ~x:(R1_interval.hi t.#x) ~y:(R1_interval.lo t.#y)
  | 2 -> R2_point.create ~x:(R1_interval.hi t.#x) ~y:(R1_interval.hi t.#y)
  | 3 -> R2_point.create ~x:(R1_interval.lo t.#x) ~y:(R1_interval.hi t.#y)
  | _ -> assert false
;;

let[@inline] [@zero_alloc] get_vertex_ij t i j =
  let px = if i = 0 then R1_interval.lo t.#x else R1_interval.hi t.#x in
  let py = if j = 0 then R1_interval.lo t.#y else R1_interval.hi t.#y in
  R2_point.create ~x:px ~y:py
;;

let[@inline] [@zero_alloc] center t =
  R2_point.create ~x:(R1_interval.center t.#x) ~y:(R1_interval.center t.#y)
;;

let[@inline] [@zero_alloc] size t =
  R2_point.create ~x:(R1_interval.length t.#x) ~y:(R1_interval.length t.#y)
;;

let[@inline] [@zero_alloc] contains_point t (p : R2_point.t) =
  R1_interval.contains t.#x (R2_point.x p) && R1_interval.contains t.#y (R2_point.y p)
;;

let[@inline] [@zero_alloc] interior_contains_point t (p : R2_point.t) =
  R1_interval.interior_contains t.#x (R2_point.x p)
  && R1_interval.interior_contains t.#y (R2_point.y p)
;;

let[@inline] [@zero_alloc] contains_rect t other =
  R1_interval.contains_interval t.#x other.#x
  && R1_interval.contains_interval t.#y other.#y
;;

let[@inline] [@zero_alloc] interior_contains_rect t other =
  R1_interval.interior_contains_interval t.#x other.#x
  && R1_interval.interior_contains_interval t.#y other.#y
;;

let[@inline] [@zero_alloc] intersects t other =
  R1_interval.intersects t.#x other.#x && R1_interval.intersects t.#y other.#y
;;

let[@inline] [@zero_alloc] interior_intersects t other =
  R1_interval.interior_intersects t.#x other.#x
  && R1_interval.interior_intersects t.#y other.#y
;;

let[@inline] [@zero_alloc] add_point t (p : R2_point.t) =
  #{ x = R1_interval.add_point t.#x (R2_point.x p)
   ; y = R1_interval.add_point t.#y (R2_point.y p)
   }
;;

let[@inline] [@zero_alloc] add_rect t other =
  #{ x = R1_interval.union t.#x other.#x; y = R1_interval.union t.#y other.#y }
;;

let[@inline] [@zero_alloc] project t (p : R2_point.t) : R2_point.Option.t =
  if is_empty t
  then R2_point.Option.none
  else
    R2_point.Option.some
      (R2_point.create
         ~x:(R1_interval.project_exn t.#x (R2_point.x p))
         ~y:(R1_interval.project_exn t.#y (R2_point.y p)))
;;

let[@inline] [@zero_alloc] project_exn t (p : R2_point.t) =
  if is_empty t
  then (
    match raise_s [%message "R2_rect.project: rectangle is empty"] with
    | (_ : Nothing.t) -> .)
  else
    R2_point.create
      ~x:(R1_interval.project_exn t.#x (R2_point.x p))
      ~y:(R1_interval.project_exn t.#y (R2_point.y p))
;;

let[@inline] [@zero_alloc] expanded t (margin : R2_point.t) =
  let xx = R1_interval.expanded t.#x (R2_point.x margin) in
  let yy = R1_interval.expanded t.#y (R2_point.y margin) in
  if R1_interval.is_empty xx || R1_interval.is_empty yy
  then empty
  else #{ x = xx; y = yy }
;;

let[@inline] [@zero_alloc] expanded_scalar t margin =
  let marginu = Float_u.of_float margin in
  let xx = R1_interval.expanded t.#x marginu in
  let yy = R1_interval.expanded t.#y marginu in
  if R1_interval.is_empty xx || R1_interval.is_empty yy
  then empty
  else #{ x = xx; y = yy }
;;

let[@inline] [@zero_alloc] union t other =
  #{ x = R1_interval.union t.#x other.#x; y = R1_interval.union t.#y other.#y }
;;

let[@inline] [@zero_alloc] intersection t other =
  let xx = R1_interval.intersection t.#x other.#x in
  let yy = R1_interval.intersection t.#y other.#y in
  if R1_interval.is_empty xx || R1_interval.is_empty yy
  then empty
  else #{ x = xx; y = yy }
;;

let[@zero_alloc ignore] approx_equal ?(max_error = 1e-15) t other =
  let me = Packed_float_option.Unboxed.some (Float_u.of_float max_error) in
  R1_interval.approx_equal ~max_error:me t.#x other.#x
  && R1_interval.approx_equal ~max_error:me t.#y other.#y
;;

let[@inline] [@zero_alloc] equal t other =
  R1_interval.equal t.#x other.#x && R1_interval.equal t.#y other.#y
;;
