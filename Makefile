.PHONY: all build test fixtures clean fmt bench

# Optional: run one test executable (Alcotest binary), not all suites.
#   make test                          # all tests
#   make test TEST=r1_interval         # -> test/test_r1_interval.exe
#   make test TEST=test_r1_interval    # same (test_ added once)
#   make test TEST=test/test_r1_interval.exe   # explicit path
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
ifneq ($(strip $(TEST)),)
ifneq ($(filter %.exe,$(TEST)),)
	dune runtest --root $(DUNE_ROOT) --force $(TEST)
else
	dune runtest --root $(DUNE_ROOT) --force test/test_$(patsubst test_%,%,$(TEST)).exe
endif
else
	dune runtest --root $(DUNE_ROOT)
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
