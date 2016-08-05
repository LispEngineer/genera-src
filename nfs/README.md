# NFS Genera Code

## `pw-disable-patch.lisp`

This file disables the UNIX crypt(3) password check for NFS "logins"
as a Genera patch. In essence, the patched code always says that a password is
correct. This is useful as modern Linux distributions don't even store
passwords in `/etc/passwd` (although the password is generally not
blank, but rather `x` or some placeholder.

Use:

* `Load File pw-disable-patch.lisp`
* `Save World` ...
* Restart Genera with the newly saved world

