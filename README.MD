Yasgheld
========

About
-----
The Yasgheld (/'j&aelig;z.g&#603;ld/) sound change applier
is a project to create a sound-change applier to assist
historical conlangers with consistent sound changes.
Yasgheld is licensed under the MIT software license. See
the enclosed LICENSE file for the full text of the license.

Installation
------------
To use a release version on Windows, simply click on
"releases" above and download the executable file. If
you wish, you may also download the source and compile
it yourself (see instructions below).

To get the most recent version of Yasgheld, or to use
Yasgheld on a computer running Linux or MacOS,
you will need to compile Yasgheld on your own. In order 
to compile Yasgheld, download the source code and run 
`ghc -o yasgheld[.exe] Main.hs` (or some other Haskell 
compiler). You will need an installation of Haskell to 
complete this step.

Note that no versions of Yasgheld are guaranteed stable,
though the most recent release is most likely to be.

Usage
-----
Yasgheld takes four arguments on the command line. These
are the paths of the input word list, the list of sound
changes, the list of sound groups, and the destination
file. These arguments are passed in this order.

``yasgheld input.ygw changes.ygc groups.ygg output.ygw``

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
must be included in the command line, and the newer file format
is _not_ backwards compatible.

API and Contributing
---
See the [documentation](https://alray2569.github.io/SoundChange/index.html).
If you want to contribute, just fork it. When your features are ready,
make a pull request and I'll look at your code.

If you are contributing, please use a code formatting style and naming 
conventions consistant with existing code and document well using Haddock. 
Undocumented code will _not_ be accepted into the repository. Where 
possible, your changed code should be in new files, though I understand 
that this is not always possible. Please document any external packages you
use in your source. Note that no source utilizing external packages other than
QuickCheck and HUnit will be accepted at this time, though they may be at a 
later date. Additionally, all contributions must be 
[Safe Haskell](https://downloads.haskell.org/~ghc/7.8.2/docs/html/users_guide/safe-haskell.html)
and must be tested thoroughly using either HUnit or QuickCheck.
Thank you for contributing!

If you would like to extend Yasgheld without contributing, you are free to
do so, and even to host and distribute your own version of Yasgheld,
under the provision that it is licensed under the MIT license. I also ask
that you include a reference to this repository. 
