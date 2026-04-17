S2_ROOT := $(HOME)/dev/s2geometry
S2_SRC  := $(S2_ROOT)/src
S2_LIB  := $(S2_ROOT)/build

CXX      := g++
CXXFLAGS := -std=c++17 -O2 -I$(S2_SRC)

ABSL_RANDOM_LIBS := -labsl_random_internal_randen \
  -labsl_random_internal_randen_slow \
  -labsl_random_internal_randen_hwaes \
  -labsl_random_internal_randen_hwaes_impl \
  -labsl_random_internal_platform \
  -labsl_random_internal_seed_material \
  -labsl_random_internal_entropy_pool \
  -labsl_random_seed_sequences \
  -labsl_random_seed_gen_exception

LDFLAGS  := -L$(S2_LIB) -ls2 -Wl,-rpath,$(S2_LIB) \
            $(shell pkg-config --libs absl_base absl_log_internal_check_op absl_strings absl_str_format 2>/dev/null) \
            -labsl_raw_hash_set -labsl_hashtablez_sampler \
            $(ABSL_RANDOM_LIBS)

.PHONY: all clean

all: bench

bench: bench.cc
	$(CXX) $(CXXFLAGS) $< $(LDFLAGS) -o $@

clean:
	rm -f bench
