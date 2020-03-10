# Freeze It #

An Emacs minor mode to kill your inner editor! Every writer struggles between
their creative and critical sides, with progress frequently hindered by the
temptation to go back and revise to get things *just right*.

Freeze It aims to combat this tempation. After an idle delay `freeze-it-delay`
all text between `point-min` and a configurable distance before `point` will be
made read-only.

Option `freeze-it-go-back` controls how far this distance "goes back" before
freezing text. This can be nil, `word`, `line`, `visible-line`, `line`, or
`paragraph`.

Text remains read-only until you kill the buffer, so that you can't cheat. This
is by design, because the minor mode targets the psychological *temptation* to
revise your writing, rather than just the ability.
