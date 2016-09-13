# GCZM - Genera CLIM Z-Machine

This is an "Infocom" Z-Machine Interpreter targeting Genera using
CLIM 2.0+ for the GUI. It is intended to play v3 Z-machine programs
only (for now).

# Files

* `gczm.lisp` - The Genera CLIM Z-Machine, the CLIM application shell
* `zmi.lisp` - The Z-Machine Interpreter, implements the Z-Machine
* `zork1.z3` - The Zork I game available free
[from Infocom](http://www.infocom-if.org/downloads/downloads.html),
just download the `zork1.zip` and use `DATA\ZORK1.DAT`. Per `file`
it is `Infocom game data (Z-machine 3, Release 88 / Serial 840726)`

# References

* [Z-Machine Specifications](http://inform-fiction.org/zmachine/standards/)
* [Older Spec](http://ifarchive.jmac.org/if-archive/infocom/interpreters/specification/zspec02/zmach06e.pdf) - This
is much better in some ways, e.g., it talks in more detail about the abbreviations table
* [Ztools](http://ifarchive.org/indexes/if-archiveXinfocomXtoolsXztools.html)
or [also here](http://inform-fiction.org/zmachine/ztools.html)

# Main Loop

The initial plan is:

1. Start up CLIM application shell
2. Load initial (hard coded for now) Z-machine state from story file
3. Run Z-machine
4. At a READ z-machine opcode (z-code? z-instruction?) return control to CLIM
5. Accept a string or a clicked menu-bar command from CLIM
6. If a menu-bar command, do that (e.g., quit, load)
7. If a string, return control to z-machine with that input

# TODO

1. Accept "accelerator" keys for menu items, e.g., m-Q for quit
2. Implement z-machine minimally
3. Implement test scripts

# Information

## `zork1.z3`

Header:

```
$ infodump -i genera-src/gczm/zork1.z3

Story file is genera-src/gczm/zork1.z3

    **** Story file header ****

Z-code version:           3
Interpreter flags:        Display score/moves
Release number:           88
Size of resident memory:  4e37
Start PC:                 4f05
Dictionary address:       3b21
Object table address:     02b0
Global variables address: 2271
Size of dynamic memory:   2e53
Game flags:               None
Serial number:            840726
Abbreviations address:    01f0
File size:                14b8c
Checksum:                 a129
```

Abbreviations:

```
$ ./infodump -a genera-src/gczm/zork1.z3

Story file is genera-src/gczm/zork1.z3

    **** Abbreviations ****

[ 0] "the "
[ 1] "The "
[ 2] "You "
[ 3] ", "
[ 4] "your "
[ 5] "is "
[ 6] "and "
[ 7] "you "
[ 8] "There "
[ 9] "can't "
[10] "of "
[11] "to "
[12] ". "
[13] "with "
[14] "are "
[15] "that "
[16] "have "
[17] "which "
[18] "This "
[19] "from "
[20] "large "
[21] "through "
[22] "appears "
[23] "in "
[24] "here"
[25] "water "
[26] "room "
[27] "Cyclops "
[28] "leading "
[29] "narrow "
[30] "cannot "
[31] "small "
[32] "into "
[33] "cyclops "
[34] "his "
[35] "thief "
[36] "It "
[37] "south "
[38] "seems "
[39] "already "
[40] "don't "
[41] "be "
[42] "but "
[43] "would "
[44] "Your "
[45] "there "
[46] "can "
[47] "this "
[48] "It's "
[49] "staircase "
[50] "on "
[51] "for "
[52] "west "
[53] "east "
[54] "north "
[55] "not "
[56] "troll "
[57] "nothing "
[58] "looking "
[59] "I "
[60] "path "
[61] "closed"
[62] "candles "
[63] "Frigid "
[64] "won't "
[65] "probably "
[66] "here "
[67] "Room"
[68] "about "
[69] "A "
[70] "too "
[71] "that"
[72] "room"
[73] "door "
[74] "grating "
[75] "ground "
[76] "out "
[77] "other "
[78] "wall "
[79] "passages "
[80] "impossible "
[81] "has "
[82] "over "
[83] "level "
[84] "knocks "
[85] "before "
[86] "passage "
[87] "Fortunately"
[88] "Unfortunately"
[89] "it "
[90] "an "
[91] "think "
[92] "river "
[93] "troll's "
[94] "doesn't "
[95] "You're "
```
