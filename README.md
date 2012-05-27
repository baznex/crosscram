# crosscram

[Crosscram](http://en.wikipedia.org/wiki/Domineering) is a two-player
[perfect information](http://en.wikipedia.org/wiki/Perfect_information) game.
This is a Clojure project for writing AI bots to play against each other:

    $ lein run crosscram.samples.eager-walk crosscram.samples.random 13 13 100
    Scores: {:bot-a 3, :bot-b 66, :draws 31}

The game is played on a two-dimensional board, much like Chess, Checkers
or Go. The two players alternate turns until one player can no longer
move, based on the game rules. Players place dominoes (2 x 1 pieces)
on empty locations on the board. One player plays horizontal pieces,
the other vertical. The game continues until one player can no longer
make a move.

Here's an example game. In this example, the board
is square, 4 x 4. The first player is playing as horizontal
and the second as vertical.

    # Empty game
    [ ][ ][ ][ ]
    [ ][ ][ ][ ]
    [ ][ ][ ][ ]
    [ ][ ][ ][ ]

    # First move
    [1][1][ ][ ]
    [ ][ ][ ][ ]
    [ ][ ][ ][ ]
    [ ][ ][ ][ ]

    # Second move
    [1][1][ ][ ]
    [2][ ][ ][ ]
    [2][ ][ ][ ]
    [ ][ ][ ][ ]

    # Third move
    [1][1][ ][ ]
    [2][ ][ ][ ]
    [2][ ][ ][ ]
    [ ][ ][3][3]

    # Fourth move
    [1][1][ ][ ]
    [2][ ][4][ ]
    [2][ ][4][ ]
    [ ][ ][3][3]

    # Fifth move
    [1][1][ ][ ]
    [2][ ][4][ ]
    [2][ ][4][ ]
    [5][5][3][3]

    # Sixth move
    [1][1][ ][6]
    [2][ ][4][6]
    [2][ ][4][ ]
    [5][5][3][3]

    # The game is now over
    # because the next player
    # cannot play a horizontal
    # piece

## License
Copyright (C) 2012 Reid Draper and
[contributors](https://github.com/baznex/crosscram/graphs/contributors)

Distributed under the Eclipse Public License, the same as Clojure uses.
See the file COPYING.
