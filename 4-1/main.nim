import std/strutils
import std/sequtils

type Cell = object
  value: int
  marked: bool

type Row = seq[Cell]
type Board = seq[Row]

type Game = object
  inputs: seq[int]
  boards: seq[Board]

proc toRow(input: string): Row =
  let row = input.splitWhitespace().mapIt(Cell(value: parseInt(it), marked: false))
  result = row

proc toBoard(input: string): Board =
  let rows = input.splitLines().filterIt(len(it) > 0).map(toRow)
  result = rows

proc readFile(): Game =
  let file = open("input.txt")
  defer: file.close()

  let inputs = file.readLine().split(",").map(parseInt) 
  let boards = file.readAll().split("\n\n").map(toBoard)

  result = Game(inputs: inputs, boards: boards)

proc markBoard(board: var Board, value: int) =
  for row in board.mitems:
    for cell in row.mitems:
      if cell.value == value:
        cell.marked = true

proc checkRows(board: Board): bool =
  for row in board:
    if row.allIt(it.marked):
      return true
  return false

proc checkColumns(board: Board): bool =
  let boardSize = len(board[0])
  for column in 0..boardSize-1:
    result = true
    for row in 0..boardSize-1:
      if not board[row][column].marked:
        result = false
        break
    if result:
      break

proc checkBoard(board: Board): bool =
  if checkRows(board) or checkColumns(board):
    return true
  return false

proc runGame() =
  var game = readFile()
  game = readFile()

  for input in game.inputs:
    for board in game.boards.mitems:
      board.markBoard(input)
      if board.checkBoard():
        echo input
        var sum = 0
        for row in board:
          echo row
          for cell in row:
            if not cell.marked:
              sum += cell.value
        sum *= input
        echo sum
        return

runGame()
