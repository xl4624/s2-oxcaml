open Core

let%expect_test "radius_constants" =
  printf
    "radius_meters=%s radius_km=%s\n"
    (Float_u.to_string S2.Earth.radius_meters)
    (Float_u.to_string S2.Earth.radius_km);
  [%expect {| radius_meters=6371010. radius_km=6371.01 |}]
;;

let%expect_test "altitude_constants" =
  printf
    "lowest_m=%s highest_m=%s\n"
    (Float_u.to_string S2.Earth.lowest_altitude_meters)
    (Float_u.to_string S2.Earth.highest_altitude_meters);
  [%expect {| lowest_m=-10898. highest_m=8848. |}]
;;

let%expect_test "angle_length_roundtrip_on_radius" =
  (* A length equal to the Earth's radius in meters subtends exactly 1
     radian at the sphere's center. *)
  let angle = S2.Earth.angle_from_length S2.Earth.radius_meters in
  printf "radians=%s\n" (S2.S1_angle.to_string angle);
  printf "back_meters=%s\n" (Float_u.to_string (S2.Earth.length_from_angle angle));
  [%expect {|
    radians=1.
    back_meters=6371010.
    |}]
;;

let%expect_test "steradians_and_area_full_sphere" =
  (* The full sphere subtends 4 * pi steradians; the surface area is
     4 * pi * r^2. *)
  let four_pi = #12.566370614359172 in
  printf "area_m2=%s\n" (Float_u.to_string (S2.Earth.area_from_steradians four_pi));
  printf
    "sr_from_surface_area=%s\n"
    (Float_u.to_string
       (S2.Earth.steradians_from_area (S2.Earth.area_from_steradians four_pi)));
  [%expect
    {|
    area_m2=510066073117988.62
    sr_from_surface_area=12.566370614359174
    |}]
;;

let%expect_test "initial_bearing_due_east" =
  (* Traveling from (0N, 0E) to (0N, 1E) along the equator has an initial
     bearing of 90 degrees (east). *)
  let a = S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#0.0 in
  let b = S2.S2_latlng.of_degrees ~lat:#0.0 ~lng:#1.0 in
  let bearing = S2.Earth.initial_bearing_from_latlngs a b in
  printf "bearing_deg=%s\n" (Float_u.to_string (S2.S1_angle.degrees bearing));
  [%expect {| bearing_deg=90. |}]
;;
