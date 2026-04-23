.PHONY: all build test fixtures clean fmt bench

# Optional: scope tests to a single module. Picks up three flavors:
#   - test/test_<module>.exe                (fixture-driven Alcotest binary)
#   - test/quickcheck/<module>.ml           (inline Base_quickcheck tests)
#   - test/expect_snapshots/<module>.ml     (ppx_expect snapshots)
# The quickcheck and snapshot stanzas are shared libraries, so when a
# matching file exists the whole library is re-run (still quick in practice).
#   make test                          # all tests
#   make test TEST=r1_interval         # -> test_r1_interval.exe + QC + snapshots
#   make test TEST=test_r1_interval    # same (test_ added once)
#   make test TEST=test/test_r1_interval.exe   # explicit path, Alcotest only
TEST ?=

all: fixtures build test

# Use an explicit workspace root so commands work from git worktrees under
# .worktrees/ (see .gitignore). Otherwise Dune ignores that tree, walks up to
# the main repo, and fails with errors about .worktrees/<name>.
DUNE_ROOT ?= .

build: test/dune.inc
	dune build --root $(DUNE_ROOT)

test/dune.inc: $(wildcard test/test_*.ml)
	./test/gen_dune.sh

test: fixtures
ifeq ($(strip $(TEST)),)
	dune runtest --root $(DUNE_ROOT)
else ifneq ($(filter %.exe,$(TEST)),)
	dune runtest --root $(DUNE_ROOT) --force $(TEST)
else
	@_m=$(patsubst test_%,%,$(TEST)); \
	 dune runtest --root $(DUNE_ROOT) --force test/test_$${_m}.exe; \
	 if [ -f test/quickcheck/$${_m}.ml ]; then \
	   echo "+ dune runtest test/quickcheck (contains $${_m}.ml)"; \
	   dune runtest --root $(DUNE_ROOT) test/quickcheck; \
	 fi; \
	 if [ -f test/expect_snapshots/$${_m}.ml ]; then \
	   echo "+ dune runtest test/expect_snapshots (contains $${_m}.ml)"; \
	   dune runtest --root $(DUNE_ROOT) test/expect_snapshots; \
	 fi
endif

fixtures:
	$(MAKE) -C test/gen all

clean:
	dune clean --root $(DUNE_ROOT)
	$(MAKE) -C test/gen clean

fmt:
	dune build --root $(DUNE_ROOT) @fmt --auto-promote || true
	clang-format -i test/gen/*.cc

# Run region-coverer benchmarks (core_bench). Tune runtime via QUOTA.
#   make bench                    # default 1s/scenario
#   make bench QUOTA=5s           # longer, tighter estimates
#   make bench BENCH_ARGS="+alloc +cycles"
QUOTA ?= 1s
BENCH_ARGS ?=
bench:
	dune build --root $(DUNE_ROOT) bench/bench_region_coverer.exe
	./_build/default/bench/bench_region_coverer.exe -ascii -quota $(QUOTA) $(BENCH_ARGS)
