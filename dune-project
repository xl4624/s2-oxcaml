(lang dune 3.22)

(name s2)

(generate_opam_files true)

(source
 (github xl4624/s2-oxcaml))

(authors "Xiaomin Liu <xl4624@nyu.edu>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name s2)
 (synopsis "S2 Geometry library for OxCaml")
 (description "A native OxCaml port of Google's S2 Geometry library.")
  (depends ocaml async core alcotest yojson base_quickcheck unboxed)
 (tags
  ("geometry" "s2" "geospatial")))
