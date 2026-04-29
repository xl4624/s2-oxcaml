(* Quickcheck property tests for S2_wedge_relations. *)
open Core

(* --- Generators ----------------------------------------------------------- *)

module Wedge_pair = struct
  type t =
    { a0 : S2.S2_point.t
    ; ab1 : S2.S2_point.t
    ; a2 : S2.S2_point.t
    ; b0 : S2.S2_point.t
    ; b2 : S2.S2_point.t
    }
  [@@deriving sexp_of]

  let gen_unit_point rnd =
    let open Base_quickcheck.Generator in
    let coord = float_inclusive (-1.0) 1.0 in
    let rec loop () =
      let x = generate coord ~size:30 ~random:rnd in
      let y = generate coord ~size:30 ~random:rnd in
      let z = generate coord ~size:30 ~random:rnd in
      if Float.((x *. x) +. (y *. y) +. (z *. z) < 1e-6)
      then loop ()
      else
        S2.S2_point.of_coords
          ~x:(Float_u.of_float x)
          ~y:(Float_u.of_float y)
          ~z:(Float_u.of_float z)
    in
    loop ()
  ;;

  (* Wedge endpoints must be distinct from the shared vertex; draw until the simple
     equality check passes. *)
  let gen_distinct rnd ~from =
    let rec loop () =
      let p = gen_unit_point rnd in
      if S2.S2_point.equal p from then loop () else p
    in
    loop ()
  ;;

  let quickcheck_generator =
    Base_quickcheck.Generator.create (fun ~size:_ ~random:rnd ->
      let ab1 = gen_unit_point rnd in
      let a0 = gen_distinct rnd ~from:ab1 in
      let a2 = gen_distinct rnd ~from:ab1 in
      let b0 = gen_distinct rnd ~from:ab1 in
      let b2 = gen_distinct rnd ~from:ab1 in
      { a0; ab1; a2; b0; b2 })
  ;;

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let qc_config =
  let module T = Base_quickcheck.Test in
  { T.default_config with test_count = 200; shrink_count = 50 }
;;

(* --- Properties ----------------------------------------------------------- *)

let%test_unit "relation_exhaustive" =
  Base_quickcheck.Test.run_exn
    (module Wedge_pair)
    ~config:qc_config
    ~f:(fun { Wedge_pair.a0; ab1; a2; b0; b2 } ->
      (* Any call must return one of the five relations; pattern matching exhausts the
         tags so the match itself is the proof. *)
      match S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2 with
      | Equals
      | Properly_contains
      | Is_properly_contained
      | Properly_overlaps
      | Is_disjoint -> ())
;;

let%test_unit "relation_reflexive_equals" =
  Base_quickcheck.Test.run_exn
    (module Wedge_pair)
    ~config:qc_config
    ~f:(fun { Wedge_pair.a0; ab1; a2; _ } ->
      let r = S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0:a0 ~b2:a2 in
      match r with
      | Equals -> ()
      | other ->
        Alcotest.failf
          "wedge vs itself: expected Equals, got %s"
          (Sexp.to_string (S2.S2_wedge_relations.Relation.sexp_of_t other)))
;;

let%test_unit "equals_symmetric" =
  Base_quickcheck.Test.run_exn
    (module Wedge_pair)
    ~config:qc_config
    ~f:(fun { Wedge_pair.a0; ab1; a2; b0; b2 } ->
      let ab = S2.S2_wedge_relations.get_wedge_relation ~a0 ~ab1 ~a2 ~b0 ~b2 in
      match ab with
      | Equals ->
        let ba =
          S2.S2_wedge_relations.get_wedge_relation ~a0:b0 ~ab1 ~a2:b2 ~b0:a0 ~b2:a2
        in
        (match ba with
         | Equals -> ()
         | other ->
           Alcotest.failf
             "Equals not symmetric: got %s"
             (Sexp.to_string (S2.S2_wedge_relations.Relation.sexp_of_t other)))
      | _ -> ())
;;
