# MineSweeper
A text minesweeper game.

Usage:

```
minesweeper-game <rows> <colums>
```

No args defaults to a 20 x 20 grid.

```
$ minesweeper-game 10 10
  Actions:
    Open Cell: o x y
    Flag Cell: f x y


  Mines: 17 Covered: 94 Flagged: 0

     1  2  3  4  5  6  7  8  9 10
  1  .  .  .  .  .  .  .  .  .  .
  2  .  .  .  .  .  .  .  .  .  .
  3  .  .  .  .  .  .  .  .  .  .
  4  .  .  .  .  .  .  .  .  .  .
  5  .  .  .  .  .  .  .  .  .  .
  6  .  .  .  .  .  .  .  .  .  .
  7  .  .  .  .  .  .  .  .  .  .
  8  .  .  .  .  .  .  .  .  1  1
  9  .  .  .  .  .  .  .  .  1  ▢
 10  .  .  .  .  .  .  .  .  1  ▢

Pick a cell:
 ```

## Commands
Open Cell:
```
o <column num> <row num>
```

Flag Cell:
```
f <column num> <row num>
```
## Haddock
[Haskell Code Documentation](https://darrell-roberts.github.io/minesweeper/)