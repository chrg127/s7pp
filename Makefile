CXXFLAGS := -std=c++20 -Wall -Wextra -pedantic -Wconversion -g

all: obj tests examples runfile

tests: obj/tests.o obj/s7.o
	g++ $(CXXFLAGS) $< obj/s7.o -o $@

examples: obj/examples.o obj/s7.o
	g++ $(CXXFLAGS) $< obj/s7.o -o $@

runfile: obj/runfile.o obj/s7.o
	g++ $(CXXFLAGS) $< obj/s7.o -o $@

obj/s7.o: s7/s7.c
	g++ -std=c++20 -g -c $< -o $@
#	g++ -std=c++20 -g -c $< -o $@

obj/tests.o: tests.cpp s7.hpp
	g++ $(CXXFLAGS) -c $< -o $@

obj/examples.o: examples.cpp s7.hpp
	g++ $(CXXFLAGS) -c $< -o $@

obj/runfile.o: runfile.cpp s7.hpp
	g++ $(CXXFLAGS) -c $< -o $@

obj:
	mkdir -p obj
