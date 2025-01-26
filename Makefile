CXXFLAGS := -Wall -Wextra -pedantic -Wconversion -g

all: tests

tests: obj/tests.o obj/s7.o
	g++ $(CXXFLAGS) -std=c++20 $< obj/s7.o -o $@

obj/s7.o: s7/s7.c
	g++ -std=c++20 -c $< -o $@

obj/tests.o: tests.cpp s7.hpp
	g++ $(CXXFLAGS) -std=c++20 -c $< -o $@

