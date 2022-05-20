import strutils

proc readInput(inputFilePath: string): seq[int] =
  for line in lines inputFilePath:
    result.add parseInt(line)

var input = readInput "inputs/day1.txt"

# Can't read from global var `expenses`: that's a side effect
func solvePart1(expenses: seq[int]): int =
  for i in 0 ..< expenses.len:
    for j in (i + 1) ..< expenses.len:
      if expenses[i] + expenses[j] == 2020:
        return expenses[i] * expenses[j]
  return 0

func solvePart2(expenses: seq[int]): int =
  for i in 0 ..< expenses.len:
    for j in (i + 1) ..< expenses.len:
      for k in (j + 1) ..< expenses.len:
        if expenses[i] + expenses[j] + expenses[k] == 2020:
          return expenses[i] * expenses[j] * expenses[k]
  return 0

echo solvePart1(input)
echo solvePart2(input)
