# Freeze It #

An Emacs minor mode to kill your inner editor! After an idle amount of
time, all text before point will be made read-only.

Option freeze-it-go-back will go back this far before making everything
prior read-only. This can be nil, word, line, visible-line, line, or
paragraph.

Option freeze-it-delay is the number of seconds to wait before freezing
text.
