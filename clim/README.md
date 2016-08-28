Attempts to make Genera CLIM be usable when we're running across a
remote X Screen instead of just at the console itself.

# Notes to Self

`(clim:find-port :server-path '(:clx :host "NEON" :screen nil :display-number 0))` works.

```lisp
(clim:find-port :server-path `(:genera :screen ,(car sys:*consoles*)))
```

Doesn't work.

```lisp
(setq myport (clim:find-port :server-path '(:clx :host "NEON" :screen nil :display-number 0)))
(setq frame (clim:make-application-frame 'clim-user::fifteen-puzzle-1 :parent myport))
(clim:run-frame-top-level frame)
```

The above actually has it display on X-Windows, but it is the wrong size. If I do it the same way but without the `:parent` it will run on the Genera console, but again, the wrong size. That's probably because the main code does this:

```lisp
(setq fp1 (make-application-frame 'fifteen-puzzle-1
            :left 200 :right 400 :top 150 :bottom 350))
```

So we need to specify a better size for the window.

