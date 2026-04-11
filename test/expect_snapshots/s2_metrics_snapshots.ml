open Core

(* No error-path snapshot: [S2_metrics] exposes no [_exn] functions in
   [lib/s2_metrics.mli]. Level lookups clamp their input internally. *)

module M = S2.S2_metrics

let levels = [ 0; 5; 10; 15; 20; 30 ]

let show m level =
  let v = Float_u.to_float (M.get_value m level) in
  sprintf "%.17e" v
;;

let%expect_test "width_metrics" =
  List.iter levels ~f:(fun l ->
    printf
      "level=%2d  min=%s  avg=%s  max=%s\n"
      l
      (show M.min_width l)
      (show M.avg_width l)
      (show M.max_width l));
  [%expect
    {|
    level= 0  min=9.42809041582063467e-01  avg=1.43452367288609950e+00  max=1.70489717919921846e+00
    level= 5  min=2.94627825494394834e-02  avg=4.48288647776906093e-02  max=5.32780368499755769e-02
    level=10  min=9.20711954669983855e-04  avg=1.40090202430283154e-03  max=1.66493865156173678e-03
    level=15  min=2.87722485834369955e-05  avg=4.37781882594634857e-05  max=5.20293328613042743e-05
    level=20  min=8.99132768232406108e-07  avg=1.36806838310823393e-06  max=1.62591665191575857e-06
    level=30  min=8.78059343976959090e-10  avg=1.33600428037913470e-09  max=1.58780923038648298e-09
    |}]
;;

let%expect_test "edge_metrics" =
  List.iter levels ~f:(fun l ->
    printf
      "level=%2d  min=%s  avg=%s  max=%s\n"
      l
      (show M.min_edge l)
      (show M.avg_edge l)
      (show M.max_edge l));
  [%expect
    {|
    level= 0  min=9.42809041582063467e-01  avg=1.45921374638610613e+00  max=1.70489717919921846e+00
    level= 5  min=2.94627825494394834e-02  avg=4.56004295745658164e-02  max=5.32780368499755769e-02
    level=10  min=9.20711954669983855e-04  avg=1.42501342420518176e-03  max=1.66493865156173678e-03
    level=15  min=2.87722485834369955e-05  avg=4.45316695064119301e-05  max=5.20293328613042743e-05
    level=20  min=8.99132768232406108e-07  avg=1.39161467207537282e-06  max=1.62591665191575857e-06
    level=30  min=8.78059343976959090e-10  avg=1.35899870319860627e-09  max=1.58780923038648298e-09
    |}]
;;

let%expect_test "diag_metrics" =
  List.iter levels ~f:(fun l ->
    printf
      "level=%2d  min=%s  avg=%s  max=%s\n"
      l
      (show M.min_diag l)
      (show M.avg_diag l)
      (show M.max_diag l));
  [%expect
    {|
    level= 0  min=1.25707872210941796e+00  avg=2.06042273899847173e+00  max=2.43865459443402122e+00
    level= 5  min=3.92837100659193111e-02  avg=6.43882105937022414e-02  max=7.62079560760631630e-02
    level=10  min=1.22761593955997847e-03  avg=2.01213158105319504e-03  max=2.38149862737697384e-03
    level=15  min=3.83629981112493273e-05  avg=6.28791119079123451e-05  max=7.44218321055304326e-05
    level=20  min=1.19884369097654148e-06  avg=1.96497224712226079e-06  max=2.32568225329782602e-06
    level=30  min=1.17074579196927879e-09  avg=1.91891821008033280e-09  max=2.27117407548615822e-09
    |}]
;;

let%expect_test "area_metrics" =
  List.iter levels ~f:(fun l ->
    printf
      "level=%2d  min=%s  avg=%s  max=%s\n"
      l
      (show M.min_area l)
      (show M.avg_area l)
      (show M.max_area l));
  [%expect
    {|
    level= 0  min=1.25707872210941796e+00  avg=2.09439510239319526e+00  max=2.63579925696316142e+00
    level= 5  min=1.22761593955997847e-03  avg=2.04530771718085475e-03  max=2.57402271187808732e-03
    level=10  min=1.19884369097654148e-06  avg=1.99737081755942847e-06  max=2.51369405456844465e-06
    level=15  min=1.17074579196927879e-09  avg=1.95055743902287936e-09  max=2.45477935016449673e-09
    level=20  min=1.14330643746999882e-12  avg=1.90484124904578063e-12  max=2.39724545914501634e-12
    level=30  min=1.09034198519706613e-18  avg=1.81659817604616225e-18  max=2.28619142450811037e-18
    |}]
;;

let%expect_test "get_closest_level" =
  let angles = [ 1.0; 0.1; 0.01; 0.001; 1e-5; 1e-8 ] in
  List.iter angles ~f:(fun a ->
    let au = Float_u.of_float a in
    printf
      "angle=%.2e  closest(avg_edge)=%2d  closest(avg_diag)=%2d  closest(avg_width)=%2d\n"
      a
      (M.get_closest_level M.avg_edge au)
      (M.get_closest_level M.avg_diag au)
      (M.get_closest_level M.avg_width au));
  [%expect
    {|
    angle=1.00e+00  closest(avg_edge)= 1  closest(avg_diag)= 1  closest(avg_width)= 1
    angle=1.00e-01  closest(avg_edge)= 4  closest(avg_diag)= 4  closest(avg_width)= 4
    angle=1.00e-02  closest(avg_edge)= 7  closest(avg_diag)= 8  closest(avg_width)= 7
    angle=1.00e-03  closest(avg_edge)=11  closest(avg_diag)=11  closest(avg_width)=10
    angle=1.00e-05  closest(avg_edge)=17  closest(avg_diag)=18  closest(avg_width)=17
    angle=1.00e-08  closest(avg_edge)=27  closest(avg_diag)=28  closest(avg_width)=27
    |}]
;;
