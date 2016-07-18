Keyboard layouts for a Symbolics 3600-ish keyboard made out
of an 87-key Tenkeyless Keyboard, as well as others.

These can be manufactured by WASD Keyboards.

Created by Douglas Fields  
Copyright 2016 - CC BY-NC-SA 4.0  
https://symbolics.lisp.engineer/

Files:

* `xquartz-87-enh`
  * Genera keyboard layout :xquartz-87-enhanced
  * `_post-path-conversion.svg` is the file for WASD Keyboards
  * `.svg` is the editable file
  * `.png` is a picture of the keyboard layout
* `symbolics-x11`
  * Hypothetical keyboard closely similar to Symbolics 3600
  * Almost 1-1 correspondence to 3600 keyboard (88 vs 87 keys)
  * A few keys changed for ease of use (e.g., colon and backtick swapped)
  
# Keyswitch Type

In testing the various Cherry MX keys offered by WASD alongside
the 3600, the linear keys are clearly the most similar. Indeed,
the linear key with the higher actuation force seems to be the
closest match 

# Keycap Color

The darker key caps on the Symbolics are a close match to the
Charcoal (#373534) color offered by WASD. However, there is no
close analog to the lighter color on the Symbolics keyboard.
The color is somewhere between Beige (#D0CCC0) and Slate (#96938E)
from WASD, but also has a browner/tanner cast. Beige is probably
the closest of the two, but I feel Slate will make a better looking
analogue.

# Keycap Type

WASD provides cylindrical-topped keys. The Symbolics 3600 has
key tops that are more spherically shaped. The `F` and `J` key caps
are particularly deep, while (I believe?) WASD puts small bumps on
those keys to indicate their home status.


# Fonts used

```text
$ grep 'font-family:[^;]*' xquartz-87-enh.svg | sed 's/.*font-family:\([^;]*\).*/\1/g' | sort | uniq
'Andale Mono'
'Bodoni 72'
'Cambria Math'
'Century Gothic'
'Franklin Gothic Book'
'HanziPen TC'
'Hiragino Maru Gothic Pro'
'Hiragino Maru Gothic ProN'
'LiHei Pro'
'Source Code Pro'
Arial
Avenir
Baskerville
Bryant
FrutigerNextLT
Gulim
Meiryo
Sans
Verdana
YuMincho
arial
sans-serif
```