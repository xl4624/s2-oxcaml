(lang dune 3.22)

(name s2)

(generate_opam_files true)

(source
 (github xl4624/s2-oxcaml))

(authors "Xiaomin Liu <xl4624@nyu.edu>")

(maintainers "Xiaomin Liu <xl4624@nyu.edu>")

(license MIT)

(package
 (name s2)
 (synopsis "S2 Geometry library for OxCaml")
 (description "A native OxCaml port of Google's S2 Geometry library.")
 (depends
  (ocaml (>= 5.0))
  async
  core
  unboxed
  ppx_uopt
  (alcotest :with-test)
  (yojson :with-test)
  (base_quickcheck :with-test))
 (tags
  ("geometry" "s2" "geospatial")))
