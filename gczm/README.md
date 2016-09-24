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

# Interesting Bugs

## commit 8c5f6a98858d4bc029b3e1e8829ab53b933bc1e4

```text
ZMI> (load-story-file "zork1.z3")
T
"Loaded zork1.z3 release 88 serial 840726"
ZMI> (mem-slice #x8EDC 6)

#(C1 AB 7F 1 0 48)
ZMI> (disassemble-instr (decode-instruction #x8EDC))

"8EDC: JE              G6F,L00 [FALSE] 8F28 "
ZMI>
```

This is being decoded differently than `./txd -d -n`:

```text
 8edc:  c1 ab 7f 01 00 48       JE              G6f,L00,(SP)+ [FALSE] 8ee8
```

Indeed, 0x8F28 is not even a valid instruction destination per
the `txd` disassembly. However, if the JE destination is two bytes
0x0048, it's a correct, positive offset (= 8EDC + 6 - 2 + 48).

I'm not sure what the "(SP)+" in the disassembly is about. The full
(but apparently incorrectly decoded) instruction is:

``` lisp
#S(DECODED-INSTRUCTION
   :MEMORY-LOCATION 8EDC
   :FIRST-BYTE C1
   :FORM VARIABLE
   :OPERAND-COUNT 2OP
   :OPERAND-TYPES (VARIABLE VARIABLE)
   :OPCODE 1
   :OPCODE-INFO #S(OCI :NAME JE :STORE NIL :BRANCH T :TEXT NIL)
   :OPERANDS (7F 1)
   :STORE NIL
   :BRANCH-IF NIL
   :BRANCH-OFFSET 48
   :LENGTH 6
   :TEXT-LOC NIL
   :TEXT-DATA NIL
   :TEXT-ASCII NIL)
```

This is showing it as a 2OP, but C1 (1100_0001) is wrong... C1 = 193,
which per page 72, says "Opcode numbers 192 to 223: VAR forms of 2OP:0 to 2OP:31."
So, this is very odd.

0xAB as a VARIABLE byte = 1010_1010, which means three variable operands
(Spec 4.2). So, why are we showing only two, yet somehow we get the
length correct nonetheless? Evidently the branch size is also wrong (2 bytes
instead of one). Hm.

As it turns out, function di-decode-op-type-byte had a mistaken assumption:
if we're a VARIABLE form instruction, using the 2OP opcode table, we were
limiting our arguments to only the first two. That's expressly NOT the case.
It's a VARIABLE form instruction (with a 4-field operand type byte) using
the 2OP opcode table.

The file `zmach06e.pdf` contains Chapter 7, "The Structure of Instructions,"
which seems to be a better reference than the newer specification I was using
(`zspec10.pdf`).
I should also rename the `opcode-count` field of the `decoded-instruction`
structure to reflect that this isn't really an opcode count, but rather
a selector for which opcode table (0OP, 1OP, 2OP, VAR or, if we were emulating
V4+, EXT) we should be using.

## commit 2553f32bfbd18b1de6640c995264d56e931802a4

Zork 1 now runs until the first SREAD instruction (command input).

Clearly, there's a bug. The output of running Zork 1 is:

``` text
ZORK I: The Great Underground Empire
Copyright (c) 1981, 1982, 1983 Infocom, Inc. All rights reserved.
ZORK is a registered trademark of Infocom, Inc.
Revision 88 / Serial number 840726

West of House
You are standing in an open field west of a white house, with a boarded front door.

>
```

Whereas,
[Interactive Fiction Archive](http://www.ifiction.org/games/playz.php?cat=&game=3&mode=html)
has different output:

``` text
ZORK I: The Great Underground Empire
Copyright (c) 1981, 1982, 1983 Infocom, Inc. All rights reserved.
ZORK is a registered trademark of Infocom, Inc.
Revision 88 / Serial number 840726

West of House
You are standing in an open field west of a white house, with a boarded front
door.
There is a small mailbox here.

>
```

There are only two differences: wrapping the long line (which we don't do yet)
and "There is a small mailbox here." is missing. That says that there's a bug in
my object/property/attribute handling.

The small mailbox is object number 160. It's parent is 180 ("West of House") and
it is a sibling of 181 ("door").

Instruction 8F3E is a GET_SIBLING that sees the sibling 160 of object 181.

Spec 12.1 says the properties are numbered from 1 upward. What does that mean
about property 0? Is the default for property 1 at the first memory location?

Instruction 8F08 gets property 0x0E for object number 160. There is no property
0x0E (14) on that object.

My function `get-property-default` is assuming that the property default table
is indexed by the property number MINUS ONE. This seems like it could be a misplaced
assumption, but I can't find anything in either spec document about it. I may
need to try it both ways. Changing it to use the exact property number
didn't work (but did get me to implement PRINT_PADDR).

Looks like routine 0x8D92 is what prints the inventory of a place. The only call
to there is from 0x8FA7. This is in routine 0x8EAA, which contains a lot of
child, parent, get_prop and test_attr calls. I may have to reverse engineer
this and see which of my functions is wrong. My next guess will probably be
test_attr. (Routine 0x8EAA seems to be recursive; see instruction at 8F5B.)

Attributes being tested are: 1B, 07, 03, 0E
Properties being gotten are: 0E, 09

```text
160. Attributes: 13, 19
     Parent object: 180  Sibling object:   0  Child object: 161
     Property address: 1a38
         Description: "small mailbox"
          Properties:
              [18] 45 3f 3d 20
              [17] 6e 94
              [16] f4
              [10] 00 0a
```

Ah, shoot. I just realized (tangentially): If we retrieve operands
twice, then we'll pop variables twice, which could/will mess things
up. I have to make sure retrieve-operands is never called more than
once. That definitely means `get_child` is broken, as it retrieves the
operands and then so does `sinstruction-jx`. Same thing with `jl`. I
need to review every instruction and ensure exactly one call to
`retrieve-operands` (or `retrieve-check-operands`) is made.

### Resolution

As it turns out, I misinterpreted Spec 6.4.3 about what happens when
you CALL a 0 address. "When the address 0 is called as a routine, nothing
happens and the return value is false." I interpreted this as "you should
return from the current routine with the value false," but what it actually
means is that "you should store the value false into the store variable
in the CALL instruction."

So, that was causing the CALL instruction at 0x8DA7 (itself an unusual
instruction in that it calls the address at the top of the stack) to
return early from routine 0x8D92.

In doing so, I think I figured some things out.

Routine 0x8D92 prints an object in the room inventory.

Routine 0x8EAA prints an entire room.

My various object attribute and property routines seem to work properly.

Implementing a complex interpreter against an unclear specification is
a very difficult task. Debugging said interpreter is even harder.

Anyway, at this point, Zork I now runs (seemingly properly) until the
first SREAD instruction, with expected output.

## commit 8f9baf593c31223e27bcd7de1bbe01603abe2d9c

There's definitely another bug in the interpreter. We can run the game and get
input, but if we try to open the mailbox in the first room, it tells us there
is no mailbox, and also gives us warnings because it's attempting to access
information about object 0. (I added the warnings because I was not sure if
it was legal to query object 0 when this first happened.)

```text
ZMI> (trace-run-until 'asdfasdf nil nil)
ZORK I: The Great Underground Empire
Copyright (c) 1981, 1982, 1983 Infocom, Inc. All rights reserved.
ZORK is a registered trademark of Infocom, Inc.
Revision 88 / Serial number 840726

West of House
You are standing in an open field west of a white house, with a boarded front door.
There is a small mailbox here.

>open mailbox
[Warning: TEST_ATTR on Object 0 at 0x6D25]
[Warning: GET_PROP_ADDR on Object 0 at 0x6D2C]
[Warning: TEST_ATTR on Object 0 at 0x6D25]
[Warning: GET_PROP_ADDR on Object 0 at 0x6D2C]
You can't see any mailbox here!
```

Now, if I were Neo, maybe there wouldn't be a mailbox, but in my real world, I
expect the Z-Machine to respond:

```text
Opening the small mailbox reveals a leaflet.                                   
```

(Proper response courtesy of 
[ifiction.org](http://www.ifiction.org/games/playz.php?cat=2&game=3&mode=html).
Solution available from [archive.org](https://web.archive.org/web/20030211015946/http://www.wurb.com/if/game/987).)

## Idea

So, I had the idea that I could use another Z-machine and log which instructions
it runs, and compare them to the instructions I run. Pretzil is another CL
implementation, and with only a few changes it is simple to make run without
CLIM. I added a *pc-log* which is just a list with the instruction addresses
being run (in reverse order, cause `push` is fast).

Then, I added the same to GCZM.

Now, I know the instruction address lists diverge at instruction 27716 (decimal):

```
ZMI> (disassemble-instr
      (decode-instruction 27716))
"6C44: INC_CHK         L03,L02 [FALSE] 6C3C "
```

My code is taking the branch to 0x6C3C, while Pretzil is not and continuing on
to 27721. Fortunately, this is also the first time this PC is seen in either of
the two programs.

So, clearly, L03 and L02 are different in Pretzil and GCZM.

In mine, the values of L03 and L02 are 4 and 24. In Pretzil, I re-enabled
the featured-out operand logging for just that instruction address and
its arguments are `Addr: 6C44, instr: 25, args #(4 0)`. So, L02 is getting
the wrong value somewhere. 

L02 is set from the CALL at 0x6D3B, where it was the top of stack
from the previous routine.

```
6D2C: GET_PROP_ADDR   L00,#12 -> L02 
Retrieved and cached operands: (181 18)
GET_PROP_ADDR: Prop 18 of object 181 is at 0x1C4C into VAR 0x3
6D30: GET_PROP_LEN    L02 -> -(SP) 
Retrieved and cached operands: (7244)
GET_PROP_LEN: Len of prop at loc 0x1C4C is 50 -> 0x00
6D33: DIV             (SP)+,#02 -> -(SP) 
DIV: 50 op 2 = 25 (0x19 -> var 0x0)
6D37: SUB             (SP)+,#01 -> -(SP) 
SUB: 25 op 1 = 24 (0x18 -> var 0x0)
6D3B: CALL            6C22 G5A,L02,(SP)+ -> -(SP) 
Retrieved and cached operands: (13841 17727 7244 24)
```

So, it seems that the `GET_PROP_LEN` is getting an unlikely
propery length, probably because `GET_PROP_ADDR` is giving
us an incorrect property address. `infodump -f` shows us:

```
181. Attributes: 14, 23
     Parent object: 180  Sibling object: 160  Child object:   0
     Property address: 1c47
         Description: "door"
          Properties:
              [18] 3f 9d 
              [17] 6d 57 
              [16] c9 ca 
```

So, after looking around my object, it seems that `GET_PROP_LEN`
is wrong. It's not a simple "load byte from memory at location X"
because that's not how that byte is encoded. :) Oops!

However, the game still says `You can't see any mailbox here!`
so we still have work to do.

## Continued investigation

Another look through the PC logs shows that the instruction traces
diverge at 0x6C40. Actually, it's the second 0x6C40.

```text
ZMI> (disassemble-instr (decode-instruction #x6C40))
"6C40: JE              L00,(SP)+ [TRUE] RET(TRUE) "
```

Pretzil does not take this branch (continuing on to 0x6D43) while
GCZM does take it and continues at 0x6C44.

GCZM:
```text
6C40: JE              L00,(SP)+ [TRUE] RET(TRUE) 
Retrieving operands for (VARIABLE VARIABLE): (1 0)
Retrieved and cached operands: (453F 7245)
JE: (453F 7245) if true to 1: Result false
```

Pretzil: (after enabling logging for instruction 6C40 -
note that the args are in decimal vs above in hex)
```
>open mailbox
Addr: 6C40, instr: 61, args #(17727 16285)
Addr: 6C40, instr: 61, args #(17727 17727)
Opening the small mailbox reveals a leaflet.
```

(0x453F = 17727) so t he second argument, the top of stack,
is what differs. Let's trace the top of stack backwards in GCZM.

GCZM:
```text
6C3C: LOADW           L01,L03 -> -(SP) 
Retrieving operands for (VARIABLE VARIABLE): (2 4)
Retrieved and cached operands: (1A3F 0)
LOADW: Loaded 0x7245 from 0x1A3F (as 1A3F[0]) into VAR 0x0
6C40: JE              L00,(SP)+ [TRUE] RET(TRUE) 
```

Pretzil: (decimal)
```text
>open mailbox
Addr: 6C3C, instr: 6F, args #(7245 0)
Addr: 6C40, instr: 61, args #(17727 16285)
Addr: 6C3C, instr: 6F, args #(6720 0)
Addr: 6C40, instr: 61, args #(17727 17727)
Opening the small mailbox reveals a leaflet.
```

0x1A3F == 6719, so there's an off by one error here...! Looking at the previous
0x6C3C it is also off by one. (0x1C4C == 7244)

Local 1 is being set by a call at 0x6D3B to the global G5A.

Searching backwards for writes to G5A, the only obvious one is at 
0x66AD.

I extended the PC logging to include operands. I found a latent bug
in my JE implementation (was matching ALL operands to the first, not
ANY).

But the bug about the mailbox is still there. More later.

## Later

The first difference now between Pretzil and GCZM/ZMI is now at instruction
0x6A69, where Pretzil's operand is 0x1C4D and ZMI is 0x1C4C. Off by one again.

```text
ZMI> (disassemble-instr (decode-instruction #x6A69))
"6A69: JZ              (SP)+ [TRUE] 6A7D "
```

Disassembly courtesy of txd:

```text
 6a65:  52 01 12 00             GET_PROP_ADDR   L00,#12 -> -(SP)
 6a69:  a0 00 d3                JZ              (SP)+ [TRUE] 6a7d
```

So, clearly, `GET_PROP_ADDR` is not working. Operands for that are
0xB5 0x12 (decimal: 181 18). We have the Object 181 shown up there from
`infodump`, so let's figure it out...

```lisp
ZMI> (let ((a #x1c47)) (map 'list (lambda (x) (let ((ao a)) (incf a) (list ao x))) (mem-slice #x1c47 20)))
((1C47 2) (1C48 26) (1C49 94) (1C4A DC) (1C4B A5) (1C4C 32) (1C4D 3F) (1C4E 9D)
 (1C4F 31) (1C50 6D) (1C51 57) (1C52 30) (1C53 C9) (1C54 CA) (1C55 0) (1C56 5)
 (1C57 1E) (1C58 86) (1C59 5D) (1C5A 2A))
```

The data of property 18 is 0x3F9D according to `infodump`, which is clearly at 
0x1C4D. So, my `GET_PROP_ADDR` is returning the location of the SIZE/PROP# byte,
not the actual propery. Fix that and...

```text
>open mailbox
Opening the small mailbox reveals a leaflet.
```

Boom. Fixed. Thank you Pretzil.

# Misc Notes

From `nyef` in `#clim`: It turns out that if you hardcode the random number
generator to return some constant, you can't get past the dwarf.

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
