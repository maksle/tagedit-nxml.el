tagedit-nxml.el
===============

Make tagedit compatible with nxml-mode

This makes tagedit compatible with nxml-mode. nxml-mode is the defaut xml mode
and would benefit from tagedit features, but it is not derived from sgml-mode.
This creates nxml versions of the sgml functions that tagedit relies upon or
lets you override.

Caveats:

Experimental features don't work currently but some of them are html-mode
specific. There's no way currently to make
`tagedit-disable-experimental-features' buffer-local that I know of so we have
to (try) to disable them manually.

Also, forward-list and backward-list had to be advised. Maybe in the future
tagedit-mode will let us override them also.

To use it:
```emacs
(add-hook 'nxml-mode-hook
  (lambda () 
    (tagedit-mode)
    (require 'tagedit-nxml)
    (enable-tagedit-nxml))
```


