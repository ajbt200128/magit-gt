[![License GPL 3][badge-license]](https://www.gnu.org/licenses/gpl-3.0.txt)

Magit-GT is a Magit interface to [Graphite]

Supports most non interactive Graphite commands

To install:

## Doom Emacs
Install the package
``` emacs-lisp
;; packages.el
;; ...
(package! magit-gt
  :recipe (:host github :repo "ajbt200128/magit-gt"))

```

Then setup keybindings. I like binding it to W
``` emacs-lisp
;; config.el
;; ...
(use-package! magit-gt
  :config
  (map! :mode 'magit-status-mode :desc "Magit Graphite" :n "w" #'magit-gt))
```

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[Graphite]: https://graphite.dev
