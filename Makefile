MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

CXXFLAGS := -std=c++20 -Wall -Wextra -pedantic -Wconversion -g -Iinclude -Is7
# no -Wall -Wextra -pedantic for s7.c
S7FLAGS := -std=c++20 -g
outdir := build

# all: $(outdir) $(outdir)/tests $(outdir)/examples $(outdir)/runfile
all: $(outdir) $(outdir)/tests.exe $(outdir)/examples.exe $(outdir)/runfile.exe $(outdir)/listiter.exe \
	$(outdir)/v2.exe

$(outdir):
	mkdir -p $@

$(outdir)/%.exe: $(outdir)/%.o $(outdir)/s7.o
	g++ $(CXXFLAGS) $< $(outdir)/s7.o -o $@

$(outdir)/%.o: test/%.cpp include/s7.hpp
	g++ $(CXXFLAGS) -c $< -o $@

$(outdir)/s7.o: s7/s7.c
	g++ $(S7FLAGS) -g -c $< -o $@

.PHONY: clean

clean:
	rm -rf $(outdir) $(outdir)/tests* $(outdir)/examples* $(outdir)/unfile*
