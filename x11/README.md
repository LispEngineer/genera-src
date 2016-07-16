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

* `(x-screen::default-x-screen)` returns the default X-Windows screen. It seems to be the current one, as it will return a different value for each X-Windows connection, e.g.:
  * `#<X-REAL-SCREEN X Screen INTERNET|192.168.1.106:0.0 0 (Genera on BERYLLIUM) 1004206265 exposed>

You can get a display by opening it, but I don't see how to get the current one
out of Flavors yet.

* `(xlib:open-display "INTERNET|192.168.1.106")`
  * This will give an error: `Error: INTERNET|192.168.1.106 does not support X-WINDOW-SYSTEM service.`
  * Simply respond `s-A: Use protocol X-WINDOW-SYSTEM on medium TCP.`
* `(xlib:display-vendor-name \*)
  * `"The X.Org Foundation"`

You can find out a lot of information with the various functions. Search for
`def-clx-class (display` in the `clx.lisp` file to see what all the parts of the
display structure are.

* `(xlib:display-vendor-name ...)`
* `(xlib:display-release-number ...)`
* `(xlib:display-host ...)`
* `(xlib:display-default-screen ...)`
* `(xlib:display-vendor ...)` - returns multiple values, `vendor-name` and `release-number`
* `(xlib::display-keysym-mapping ...)` - `nil`?
* `(xlib::display-modifier-mapping ...)` - `nil`?
* `(xlib::display-keysym-tanslation ...)` - `nil`?
* `(xlib:display-min-keycode ...)`
* `(xlib:display-max-keycode ...)`
* `(xlib:display-roots ...)`
* `(xlib:display-xdefaults ...)` - `nil`?

So the `keyboard-signature` seems to be a way for Genera to automatically figure
out which keyboard is in use at the destination X screen and set the mappings
appropriately! As it turns out, the Xquarts 2.7.10_beta2 on macOS 10.11.5 returns
these relevant data about its screen:

* `min-keycode`: 8 - which is mapped to the `keycode-offset` when creating a signature
* `vendor-name`: `The X.Org Foundation`
* `vendor-version`: 118030000

Indeed, I created a test keyboard signature starting with the below:

```lisp
(define-keyboard-signature :xquartz-87-tenkeyless
                           (:keycode-offset 8
                            :vendor-name "The X.Org Foundation"
                            :vendor-version 11803000)
```

When I started a new X screen (`Start X Screen INTERNET|192.168.1.106 :Reuse No`),
and immediately checked the maping (`Show X Keyboard Mapping`), it responded
as I would hope:

```
The keyboard layout type is :XQUARTZ-87-TENKEYLESS.
```

Now, the next matter turns to the `define-keyboard-mapping`
to try to set the key mapping reasonably usably.

```lisp
(define-keyboard-mapping :xquartz-87-tenkeyless ()
```


 

# `dpf-x-kb-2.lisp`

Testing code for above
