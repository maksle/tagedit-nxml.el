tagedit-nxml.el
===============

Make tagedit compatible with nxml-mode

This makes tagedit compatible with nxml-mode. nxml-mode is the defaut xml mode
and would benefit from tagedit features, but it is not derived from sgml-mode.
This creates nxml versions of the sgml functions that tagedit relies upon or
lets you override.


Thanks to Magnar Sveen for the awesome tagedit package.


To use it:
```emacs
(add-hook 'nxml-mode-hook
  (lambda () 
    (tagedit-mode)
    (require 'tagedit-nxml)
    (enable-tagedit-nxml))
```


