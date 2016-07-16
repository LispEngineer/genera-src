# x11

Files related to X11

# `dpf-x-kb.lisp`

An X11 keyboard mapping for Genera when connected to a macOS 10.11.5 computer
running Xquartz 2.7.10_beta2 with a Tenkeyless 
87-key keyboard.

This doesn't yet work. Having trouble remapping certain keys/keysyms.

## Notes

Genera seems to read the xmodmap once when the screen is started, to get
all the keynums and keysyms. If you change the xmodmap, it doesn't seem to
pick up on that later.

# `dpf-x-kb-2.lisp`

Testing code for above
