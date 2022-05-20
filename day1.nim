import strutils

proc readInput(inputFilePath: string): seq[int] =
  for line in lines inputFilePath:
    result.add parseInt(line)

var input = readInput "inputs/day1.txt"

# We could just return seq[int] and do the multiplication outside, but trying out structs!
type
  ProblemSolution = object
    matches: seq[int]
    product: int

# Can't read from global var `input`: that's a side effect
func solvePart1(expenses: seq[int]): ProblemSolution =
  for i in 0 ..< expenses.len:
    for j in (i + 1) ..< expenses.len:
      if expenses[i] + expenses[j] == 2020:
        let product = expenses[i] * expenses[j]
        return ProblemSolution(matches : @[expenses[i], expenses[j]],
                               product : product)
  return ProblemSolution(matches : @[],
                         product : 0)

func solvePart2(expenses: seq[int]): ProblemSolution =
  # `result` is implicitly returned, and automatically set with initial values
  for i in 0 ..< expenses.len:
    for j in (i + 1) ..< expenses.len:
      for k in (j + 1) ..< expenses.len:
        if expenses[i] + expenses[j] + expenses[k] == 2020:
          # Slightly differnet way to write it
          result.matches = @[expenses[i] * expenses[j] * expenses[k]]
          result.product = expenses[i] * expenses[j] * expenses[k]

proc printSolution(sol: ProblemSolution) =
  # TODO print seq
  echo sol.product

printSolution solvePart1(input)
printSolution solvePart2(input)
