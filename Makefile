MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

CXXFLAGS := -std=c++20 -Wall -Wextra -pedantic -Wconversion -O2 -Iinclude -Is7
# no -Wall -Wextra -pedantic for s7.c
S7FLAGS := -std=c++20 -O2
outdir := build
tests :=  runfile listiter v2 complex define_function_special_cases errors \
		  from_edge_cases history listiter runfile set signatures star_fns varargs
examples := bignums c_function_variable c_side_macro c_side_star_function \
			call_get_set_vars cpp_repl extend_operator extend_operator_with_method \
			generic_function handle_errors hooks listener_dax load_shared_library \
			namespace notification repl signals_continuations

ifeq ($(OS),Windows_NT)
	_programs := $(patsubst %,$(outdir)/%.exe,$(tests))
	_programs += $(patsubst %,$(outdir)/%.exe,$(examples))
else
	_programs := $(patsubst %,$(outdir)/%,$(tests))
	_programs += $(patsubst %,$(outdir)/%,$(examples))
endif

all: $(outdir) $(_programs)

$(outdir):
	mkdir -p $@

$(outdir)/%.exe: $(outdir)/%.o $(outdir)/s7.o
	g++ $(CXXFLAGS) $< $(outdir)/s7.o -o $@

$(outdir)/%.o: test/%.cpp include/s7.hpp
	g++ $(CXXFLAGS) -c $< -o $@

$(outdir)/%.o: examples/%.cpp include/s7.hpp
	g++ $(CXXFLAGS) -c $< -o $@

$(outdir)/s7.o: s7/s7.c
	g++ $(S7FLAGS) -c $< -o $@

.PHONY: clean

clean:
	rm -rf $(outdir)
