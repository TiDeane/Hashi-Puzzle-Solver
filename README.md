# Hashi-Puzzle-Solver
IST Logic for Programming project, 2021/2022. Prolog code that solves a given Hashi puzzle.

The objective of this project is to write the first part of a PROLOG program that solves hashi puzzles, from now on just called “puzzles”.

A hashi puzzle is made up of an _n_ × _m_ grid (_n_ rows, _m_ columns). Each position on the grid can be empty, or contain an _island_, with an indication of the number of bridges that island should have when solving the puzzle. The following image (1) shows an example of a 7 × 7 puzzle, with 9 islands.

![Screenshot_2](https://github.com/TiDeane/Hashi-Puzzle-Solver/assets/120483063/44bbeb52-fb91-42ea-af46-93399962fdb7)

In the first line of this puzzle there are 2 islands: one in position (1, 1), indicating 4 bridges, and another in position (1, 7), indicating 3 bridges.

To obtain a solution, the islands must be connected by bridges, in order to respect the number of bridges indicated by each island and the following restrictions:
- There are no more than two bridges between any two islands.
- Bridges can only be vertical or horizontal and cannot cross islands or other bridges.
- When solving the puzzle, bridges allow passage between any two islands.

Thus, the puzzle in figure 1 has a single solution, which is presented below in figure 2.

![Screenshot_1](https://github.com/TiDeane/Hashi-Puzzle-Solver/assets/120483063/91855770-2164-4ebc-b61b-8ae16ad5a0c8)

## Puzzle representation
A puzzle is represented by a list of lists, each inner list corresponding to a line of the puzzle. A position containing an island is represented by a positive integer, corresponding to the number of bridges on that island. Empty positions are represented by zero. For example, the puzzle in Figure 1 is represented by

`[[4, 0, 0, 0, 0, 0, 3],`  \
`[0, 0, 0, 0, 0, 0, 0],`  \
`[0, 0, 1, 0, 1, 0, 0],`  \
`[0, 0, 0, 0, 0, 0, 0],`  \
`[0, 0, 0, 0, 0, 0, 0],`  \
`[0, 0, 3, 0, 4, 0, 2],`  \
`[4, 0, 0, 2, 0, 0, 0]]`

Some useful predicates were provided in the `codigo_comum.pl` file. Some puzzles are defined in the `puzzles_publicos.pl` file. To use these files, you must enter the command `:- [codigocomum, puzzles_publicos].` at the beginning of `hashi_solver.pl`.
