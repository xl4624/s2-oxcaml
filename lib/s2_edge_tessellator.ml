open Core

(* The error between a geodesic edge and its projected counterpart is
   bounded by a convex combination of two worst-case shapes:

     E1(x) = 1 - x^2                  (maximal at the midpoint x = 0)
     E2(x) = x (1 - x^2) (scaled)     (zero at the midpoint, off-center)

   We pick the interpolation parameter x0 where E1(x0) = E2(x0) and
   evaluate the actual error at both x0 and -x0. Their max, divided by
   E1(x0), is a conservative upper bound on the error anywhere in [-1, 1].
   Equivalently, we can scale the tolerance by [scale_factor = E1(x0)]
   instead of dividing the error. *)

let interpolation_fraction : float# = #0.31215691082248312
let scale_factor : float# = #0.83829992569888509

(* Less than a micrometer on Earth's surface, but still far larger than
   the projection and interpolation errors we inherit from double
   precision. *)
let min_tolerance = S1_angle.of_radians #1e-13

type t =
  #{ projection : S2_projections.t
   ; scaled_tolerance : S1_chord_angle.t
   }

let[@zero_alloc ignore] sexp_of_t t : Sexp.t =
  Sexp.List
    [ Sexp.List [ Sexp.Atom "projection"; S2_projections.sexp_of_t t.#projection ]
    ; Sexp.List
        [ Sexp.Atom "scaled_tolerance"
        ; Float.sexp_of_t (Float_u.to_float (S1_chord_angle.radians t.#scaled_tolerance))
        ]
    ]
;;

let[@inline] [@zero_alloc] create ~projection ~tolerance =
  let clamped =
    if S1_angle.compare tolerance min_tolerance < 0 then min_tolerance else tolerance
  in
  let scaled_radians = Float_u.O.(scale_factor * S1_angle.radians clamped) in
  let scaled_tolerance = S1_chord_angle.of_radians scaled_radians in
  #{ projection; scaled_tolerance }
;;

(* Parametric error estimate: max distance between the two edges at
   the two sampling points [t1 = x0] and [t2 = 1 - x0]. Edges longer
   than roughly 90 degrees (a . b < -1e-14) are always subdivided
   because the two-sample approximation is not robust at that scale. *)
let[@inline] [@zero_alloc] estimate_max_error t ~pa ~a ~pb ~b =
  if Float_u.O.(R3_vector.dot a b < -#1e-14)
  then S1_chord_angle.infinity
  else (
    let t1 = interpolation_fraction in
    let t2 = Float_u.O.(#1.0 - interpolation_fraction) in
    let mid1 = S2_edge_distances.interpolate a b t1 in
    let mid2 = S2_edge_distances.interpolate a b t2 in
    let pmid1 =
      S2_projections.unproject
        t.#projection
        (S2_projections.interpolate t.#projection ~f:t1 pa pb)
    in
    let pmid2 =
      S2_projections.unproject
        t.#projection
        (S2_projections.interpolate t.#projection ~f:t2 pa pb)
    in
    let d1 = S2_point.chord_angle_between mid1 pmid1 in
    let d2 = S2_point.chord_angle_between mid2 pmid2 in
    if Int.(S1_chord_angle.compare d1 d2 >= 0) then d1 else d2)
;;

module R2_buf = Unboxed_vec.Make [@kind (float64 & float64) mod external_] (struct
    type t = R2_point.t

    let default = R2_point.zero
  end)

module S2_buf =
Unboxed_vec.Make [@kind (float64 & float64 & float64) mod external_] (struct
    type t = S2_point.t

    let default = R3_vector.zero
  end)

(* Given a geodesic edge a->b with projections pa and pb_in (pb_in not yet
   wrapped relative to pa), subdivide and push the projected vertices (all
   except pa) to [out]. *)
let rec append_projected_rec t ~pa ~a ~pb_in ~b ~out =
  let pb = S2_projections.wrap_destination t.#projection ~a:pa ~b:pb_in in
  let err = estimate_max_error t ~pa ~a ~pb ~b in
  if S1_chord_angle.compare err t.#scaled_tolerance <= 0
  then R2_buf.push out pb
  else (
    let mid = R3_vector.normalize (R3_vector.add a b) in
    let pmid_raw = S2_projections.project t.#projection mid in
    let pmid = S2_projections.wrap_destination t.#projection ~a:pa ~b:pmid_raw in
    append_projected_rec t ~pa ~a ~pb_in:pmid ~b:mid ~out;
    append_projected_rec t ~pa:pmid ~a:mid ~pb_in:pb ~b ~out)
;;

let append_projected t ~a ~b ~(out : R2_buf.t) =
  let pa = S2_projections.project t.#projection a in
  let pa =
    if R2_buf.length out = 0
    then (
      R2_buf.push out pa;
      pa)
    else S2_projections.wrap_destination t.#projection ~a:(R2_buf.last out) ~b:pa
  in
  let pb = S2_projections.project t.#projection b in
  append_projected_rec t ~pa ~a ~pb_in:pb ~b ~out
;;

let rec append_unprojected_rec t ~pa ~a ~pb_in ~b ~out =
  let pb = S2_projections.wrap_destination t.#projection ~a:pa ~b:pb_in in
  let err = estimate_max_error t ~pa ~a ~pb ~b in
  if S1_chord_angle.compare err t.#scaled_tolerance <= 0
  then S2_buf.push out b
  else (
    let pmid = S2_projections.interpolate t.#projection ~f:#0.5 pa pb in
    let mid = S2_projections.unproject t.#projection pmid in
    append_unprojected_rec t ~pa ~a ~pb_in:pmid ~b:mid ~out;
    append_unprojected_rec t ~pa:pmid ~a:mid ~pb_in:pb ~b ~out)
;;

let append_unprojected t ~pa ~pb ~(out : S2_buf.t) =
  let a = S2_projections.unproject t.#projection pa in
  let b = S2_projections.unproject t.#projection pb in
  if S2_buf.length out = 0 then S2_buf.push out a;
  append_unprojected_rec t ~pa ~a ~pb_in:pb ~b ~out
;;

let project t points =
  let n = Array.length points in
  if n = 0
  then [||]
  else if n = 1
  then [| S2_projections.project t.#projection points.(0) |]
  else (
    let out = R2_buf.create () in
    for i = 0 to n - 2 do
      append_projected t ~a:points.(i) ~b:points.(i + 1) ~out
    done;
    R2_buf.to_array out)
;;

let unproject t points =
  let n = Array.length points in
  if n = 0
  then [||]
  else if n = 1
  then [| S2_projections.unproject t.#projection points.(0) |]
  else (
    let out = S2_buf.create () in
    for i = 0 to n - 2 do
      append_unprojected t ~pa:points.(i) ~pb:points.(i + 1) ~out
    done;
    S2_buf.to_array out)
;;
