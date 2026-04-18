open Core

let[@inline] [@zero_alloc] origin () = S2_point.origin
let[@inline] [@zero_alloc] is_unit_length p = S2_point.is_unit_length p

let[@inline] [@zero_alloc] approx_equals
  ~(max_error_radians : Packed_float_option.Unboxed.t)
  a
  b
  =
  S2_point.approx_equal ~max_error:max_error_radians a b
;;

let[@inline] [@zero_alloc] ortho a = S2_point.ortho a
let[@inline] [@zero_alloc] ref_dir a = S2_point.ortho a
let[@inline] [@zero_alloc] rotate p ~axis ~angle = S2_point.rotate p ~axis ~angle

type frame = S2_point.frame

let[@inline] [@zero_alloc] get_frame z = S2_point.get_frame z
let[@inline] [@zero_alloc] to_frame m p = S2_point.to_frame m p
let[@inline] [@zero_alloc] from_frame m q = S2_point.from_frame m q
