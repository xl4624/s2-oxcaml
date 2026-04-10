# Agent Porting Protocol for S2 OxCaml

## Purpose

Port Google's S2 Geometry library to OxCaml using Jane Street's `Core` and
Async.

This repo covers the full library surface, including `r1`, `r2`, `r3`, `s1`,
`s2`, and `earth`.

Agents have access to the OxCaml skill.

## Required References

Before writing code for a module, read all of the following:

1. Go source in `~/dev/geo/`
2. C++ header and implementation in `~/dev/s2geometry/src/s2/`
3. The corresponding upstream C++ test file

For OxCaml idioms, unboxed type patterns, and Jane Street library usage, refer
to the codebases in `~/dev/oxcaml/` and `~/dev/oxmono/` as working examples.

Use the Go port for readability and the C++ implementation as the canonical
source of truth for behavior, edge cases, and numerical precision.

## Canonical Style Reference

Use these files as the primary model for structure and style:

- `lib/s2_cell_id.mli`
- `lib/s2_cell_id.ml`
- `test/gen/s2cellid.cc`
- `test/test_s2_cell_id.ml`

## Project Commands

Run commands from the repo root unless a task explicitly requires otherwise.

- `make build`: build the OCaml code with `dune`
- `make test`: run `make fixtures` and then `dune runtest`
- `make test TEST=r1_interval`: run a single test binary
- `make fmt`: run OCaml formatting and `clang-format -i`
- `make all`: run formatting, build, fixtures, and tests
- `dune exec tools/fixture_query.exe -- <fixture.json> [<path>] [--keys]`: query or explore JSON fixtures (e.g., `dune exec tools/fixture_query.exe -- test/fixtures/s2cell.json "faces[0]"`)

`make fmt` is required before finishing work.

## Required Workflow

Follow this sequence for every module.

1. Read the Go source, C++ header, C++ implementation, and upstream C++ tests.
2. Review the public API and invariants.
3. Write a C++ golden-data generator in `test/gen/`.
4. Run `make fixtures` to produce `test/fixtures/<name>.json`.
5. Create `lib/<name>.mli`.
6. Create `lib/<name>.ml`.
7. Create `test/test_<name>.ml`.
8. Run `make test` and iterate until green.
9. Run `make fmt`.

## Definition Of Done

A module is only done when all of the following are true:

- The upstream sources and tests were reviewed.
- A generator exists in `test/gen/`.
- A fixture exists in `test/fixtures/`.
- `lib/<name>.mli` and `lib/<name>.ml` are implemented.
- `test/test_<name>.ml` exists and consumes the fixture.
- `make fmt` has been run.
- `make build` passes.
- `make test` passes.

## Golden Data Requirements

Golden data is mandatory for every module.

Write a small C++ program in `test/gen/` that links against the canonical S2
library and emits JSON to stdout using `nlohmann/json`.

Each section of the generator must reference the upstream C++ `TEST()` it
mirrors, for example `// TEST(S1Angle, Trigonometry)`. See `test/gen/r1interval.cc`
and `test/gen/s1angle.cc` for the expected comment style.

Fixture guidelines:

- Cover normal cases, edge cases, and degenerate cases.
- Prefer structured JSON with clear `input` and `expected` fields.
- Group related cases under stable top-level keys such as `"constructors"`,
  `"accessors"`, or `"ops"`.
- Use descriptive field names such as `p1`, `p2`, `margin`, `x_val`, `y_val`.

All expected values in OCaml tests must come from fixtures. Do not hardcode
expected numeric results in the test body.

### Fixture naming vs. stored values

Generator field names often describe the **scenario**, while the JSON value is
whatever the C++ test **actually compared** (for example the raw result of
`Contains(p)`, not necessarily the negated predicate). When a key contains
words like `not_` or `invalid`, read the generator source and mirror that
meaning in the OCaml test. Do not assume negation from the English label alone.

### Parity with C++ on tiny or degenerate geometry

If a case depends on **almost axis-aligned** points, **very small chord
angles**, or other underflow-prone construction, emit the **final normalized
`S2Point` (or other) values from C++** into the fixture (e.g. three IEEE
doubles) and build the OCaml value from that triple. Rebuilding the same
construction only in OCaml can diverge from canonical S2 due to rounding order.

## Test Requirements

Use Alcotest with fixture-driven cases as the main verification strategy.

Each OCaml test file must begin with a header comment listing the upstream
C++ `TEST()` cases it covers, any extra coverage, and any deliberately omitted
tests. See `test/test_r1_interval.ml` and `test/test_s1_angle.ml` for the
expected format.

- Load fixtures with `Yojson.Safe.from_file`.
- Use `Test_helpers` for shared assertions such as `check_float` and
  `check_float_exact`.
- Prefer the unboxed variants when working with `float#` values:
  - `float_u_of_json_exn` to parse JSON floats directly to `float#`.
  - `check_float_u ?eps msg ~expected ~actual` for `float#` comparisons.
  - `check_float_u_exact msg ~expected ~actual` for exact `float#` equality.
  - These avoid unnecessary boxing/unboxing round-trips in tests.
- If you find yourself reaching for the boxed `float_of_json_exn` in a
  fixture-driven test, that is almost always a smell: the S2 API takes
  `float#`, so `float_u_of_json_exn` is the right call. The only legitimate
  use of the boxed variant is inside a Quickcheck generator or shrinker
  where boxed `float` values are already in hand.
- Use floating-point epsilon `1e-15` unless the upstream reference requires a
  stricter comparison.
- Use `Test_helpers.check_float_u ?eps` when the reference uses a looser
  tolerance (for example degree-scale quantities from lat/lng).
- Compare integer identifiers exactly.

Property-based tests with `Base_quickcheck` are encouraged for checking invariants and mathematical relationships:

- Define `quickcheck_generator` and `quickcheck_shrinker` for your types.
- Use `Base_quickcheck.Test.run_exn` inside a function.
- Register these quickcheck functions in the Alcotest suite using ``Alcotest.test_case "name" `Quick my_quickcheck_fn``.
- See `test/test_r1_interval.ml` and `test/test_r2_rect.ml` for examples.

Expect tests with `ppx_expect` are encouraged for:

- internal invariants
- `_exn` error paths
- `sexp_of` output
- exploratory debugging

Use `show_raise` from `expect_test_helpers_core` when checking exceptions. See `test/expect_snapshots/snapshots.ml` for examples of snapshot testing.

## Generated Files And Large Fixtures

Do not edit generated files manually.

- Do not edit `test/dune`.
- Do not edit `test/dune.inc`.
- Create `test/test_<name>.ml` and let the project tooling pick it up.

## API Design Rules

- Prefer idiomatic OCaml APIs over 1:1 C++ method parity.
- Public functions in `.mli` files should have doc comments.
- When borrowing documentation from C++ or Go, adapt it rather than rewriting
  it unnecessarily.
- Functions that can fail due to invalid arguments or invariant violations
  should expose both:
  - a safe variant returning the module's unboxed `Option.t` (see below)
  - a raising variant suffixed with `_exn`
- Prefer `Core.raise_s` over `failwith` or `invalid_arg`.

Use `match%optional_u.Mod.Option expr with` to branch on option values inline,
avoiding any allocation:

```ocaml
match%optional_u.R2_point.Option face_xyz_to_uv face p with
| None -> ...
| Some uv -> ...
```

See `lib/r2_point.ml`, `lib/s2_cap.ml`, and `lib/s1_chord_angle.ml` for
complete worked examples.

## OxCaml Unboxed Types

### Raising exceptions with non-value layouts

`failwith`, `raise_s`, and `raise` all return `'a : value_or_null`, which
does not unify with unboxed product layouts like `float64 & float64`. Use the
`Nothing.t` refutation pattern (from Jane Street's `Float_u` library) instead:

```ocaml
| name ->
  (match failwith (sprintf "unknown: %s" name) with
   | (_ : Nothing.t) -> .)
```

`assert false` also works (compiler special-cases it for any layout), but the
`Nothing.t` pattern preserves the error message.

### Unboxed float literals

- Positive: `#1.0`, `#0.5`
- Negative: `-#1.0` (not `#(-1.0)`)
- Zero: `#0.0`

### Unboxed constants

Unboxed constants are exposed as `unit -> float#` functions, not plain values.
Call them with `()`:

- `Float_u.infinity ()`, `Float_u.neg_infinity ()`, `Float_u.nan ()`
- `Float_u.pi ()`, `Float_u.max_float ()`, `Float_u.min_float ()`
- `Float_u.epsilon ()`, etc.

### Boxing conversions

- `float` to `float#`: `Float_u.of_float x`
- `float#` to `float`: `Float_u.to_float x`
- Unboxed types cannot appear in tuples; use boxed records or unboxed records
  (`#{ ... }`) instead.
- When a function has several `float#` operations, use `let open Float_u.O in`
  to get infix operators (`+`, `-`, `*`, `/`, `<=`, `>=`, etc.) instead of
  writing `Float_u.( - )`, `Float_u.( <= )`, and so on.

## Naming And Style

- Use `snake_case` for OCaml values and functions.
- Keep module names aligned with upstream concepts, for example `R1_interval`
  and `S2_cell_id`.
- Comment the non-obvious math, invariants, precision choices, or OxCaml
  subtleties.

## Do Not

- Do not hardcode expected results in Alcotest cases.
- Do not manually edit generated dune files.
- Do not write filler comments that merely restate the code.
- Avoid unnecessary allocations in tight loops.

## Final Reporting

When finishing a module, include:

- which upstream Go files were used
- which upstream C++ files were used
- which upstream C++ test file was used
