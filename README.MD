Yasgheld
========

About
-----
The Yasgheld (/'y&aelig;z.g&#603;ld/) sound change applier
is a project to create a sound-change applier to assist 
historical conlangers with consistent sound changes. At
present, the project supports only single character
substitutions with environments, or multicharacter
substitutions without environments.

Installation
------------
At present, you will need to compile Yasgheld on your own;
no binaries are provided. In order to compile Yasgheld,
download the source code and run `ghc Main.hs` (or some
other Haskell compiler). You will need an installation of 
Haskell to complete this step.

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
the system. The author recommends using the file extension
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

The author recommends using the file extension `.ygc` (Yasgheld
changes) for this file. If you use this extension, you will not
need to enter it in the command line argument.

Syntax of the sound group list
----
Sound groups should be listed exactly one per line, and take 
the following form:
``X: abcd``

where 'X' is the one-character capital letter that identifies
the group, and "abcd" are the letters that comprise the group.
Note that this syntax is likely to change in future versions
of Yasgheld. The author recommends using the extension `.ygg`
(Yasgheld groups) for these files. If you use this extension,
you will not need to enter it into the command line argument.
If you have files using the version 1 syntax, they will still
work if they have the extension `.ygg1`, however, the extension
must be included in the command line.

API
---
See the [documentation](https://alray2569.github.io/SoundChange/index.html).
