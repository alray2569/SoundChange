Yasgheld
========

About
-----
The Yasgheld (/'y&aelig;z.g&#603;ld/) sound change applier
is a project to create a sound-change applier to assist
historical conlangers with consistent sound changes.

Installation
------------
At present, you will need to compile Yasgheld on your own;
no binaries are provided. In order to compile Yasgheld,
download the source code and run `ghc -o yasgheld[.exe] Main.hs`
(or some other Haskell compiler). You will need an
installation of Haskell to complete this step.

Usage
-----
Yasgheld takes four arguments on the command line. These
are the paths of the input word list, the list of sound
changes, the list of sound groups, and the destination
file. These arguments are passed in this order.

``yasgheld input.ygw changes.ygc groups.ygg1 output.ygw``

Syntax of the word list
------
The word list should contain exactly one word per line.
All words will be converted to lower case to be run through
the system. Note that all characters beyond the Unicode
block "Latin Extended Additional" (points greater than
U+1EFF) are reserved for internal use. Using characters
beyond this point may have unpredictable effects.

Comments can be included in all files by placing a `;` at
the start of a line or after a valid entry. Comments will be
ignored by Yasgheld.

The author recommends using the file extension
`.ygw` (Yasgheld words) for this file, and for the output
file. If you use this extension, you will not need to
enter it in the command line argument.

Syntax of the sound change list
----
The sound changes should be listed exactly one per line, and
take the form of standard formal sound change notation:

``input > output / situation [& situation [& situation ...]]``

If present, each situation must contain the character '`\_`', to
represent the location of the sound undergoing the change.
Situations may be negated by placing an exclamation point ('`!`')
in front of them, and the beginning and end of the word are
represented by the hash symbol ('`#`'). For example, `a > o / !_#`
changes all instances of 'a' to 'o' except word-finally. Spaces
around sound changes are optional; i.e. `a>o/!_#` is identical
to `a > o / !_#`.

Capital letters represent groups of sounds defined
in the sound group list, while lowercase letters represent
individual sounds. The '`*`' character represents a special group
of all sounds and matches all characters. When referencing a group
with a long name, the long name should be placed in square brackets
(`[]`). Single-character names should not be placed in square brackets.
Groups can be used in inputs and situations only currently,
not in the output argument.

Comments can be included in all files by placing a `;` at
the start of a line or after a valid entry. Comments will be
ignored by Yasgheld.

The author recommends using the file extension `.ygc` (Yasgheld
changes) for this file. If you use this extension, you will not
need to enter it in the command line argument.

Syntax of the sound group list
----
Sound groups should be listed exactly one per line, and take one of
the following forms:
``X: abcd``
``Group: abcd``

where `X` is the one-character capital letter that identifies
the group or `Group` is a multicharacter name that identifies
the group, and "abcd" are the letters that comprise the group.
Sound groups do _not_ support digraphs, so we recommend using
single characters for all sounds.

Comments can be included in all files by placing a `;` at
the start of a line or after a valid entry. Comments will be
ignored by Yasgheld.

The author recommends using the extension `.ygg`
(Yasgheld groups) for these files. If you use this extension,
you will not need to enter it into the command line argument.
If you have files using the version 1 syntax, they will still
work if they have the extension `.ygg1`, however, the extension
must be included in the command line.

API
---
See the [documentation](https://alray2569.github.io/SoundChange/index.html).
