# An experiment with compile-time guarantees.

So, a simple tic-tac-toe games, with quite a few guarantees enforced by types.

The simplest version of the function to _make a move_ in a tic tac toe game,
would look roughly like this:

```elm
move : Int -> Int -> Player -> Board (Maybe Player) -> Board (Maybe Player)
```

This, however, doesn't have a whole lot of compile-time guarantees:

- `X` playing two times in a row won't result in a compile-time error
- playing on an already finished board won't result in a compile-time error
- no compile-time guarantees that those coordinates are valid and represent a
free cell
- no compile-time guarantees that making a move will result in either an ongoing
 game, or a game that is finished, resulting in either a draw _or_ the last
 player to have made a move becoming the winner

Towards that end, the `move` function in this implementations has the following
signature:

```elm
move :
    Board.Cell Board.Free
    -> Game current next
    -> Either (Game next current) (FinishedGame current)
```

The only way to construct a `Game` has the following signature:

```elm
init : Game Cross Naught
```

Which results in our game always starting with an `X`, and continuing, turn by
turn, until the game is finished.
