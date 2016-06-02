# Various

ga-ts: 
First larger code i wrote in lisp, solves the traveling postman problem using an genetic algorithm, on an i3 laptop took 7min to calculate for 200 points. Have setup for points all in a circle to see if it got it correct, and random points.
uses an svg converter written by jl2 to show the points and route taken.

GP:
Genetic programming that finds a polynomial that goes through each given point, current points are 5 points in the first eigth of a cos cycle. Found one with a solution with error of e-11, but usually find something quite fast with e-6. calculated with (Sum of the square of difference between the points and the function)

advent:
My solutions to adventofcode.com 
using only the common lisp library and without implementing code from other people
