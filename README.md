euswank
=======

Swank Proxy for Euslisp

## Author

Yuki Fututa <<furushchev@jsk.imi.i.u-tokyo.ac.jp>>

## 使い方（若干うろおぼえ）

1. Install euswank

```bash
git clone https://github.com/furushchev/euswank
cd euswank
sudo pip install -e -U .
```

This enable to run `euswank`.

1. Add sctipt to init file

```lisp
;; ~/.emacs.el
(require 'slime-autoload)
(setq slime-contribs
  '(slime-fancy slime-asdf slime-cl-indent))
(setq inferior-lisp-program (executable-find "sbcl")
      slime-net-coding-system 'utf-8-unix
      slime-complete-symbol*-fancy t
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
(slime-setup slime-contribs)
(setq slime-lisp-implementations
  '((euswank ("euswank") :coding-system utf-8-unix)))
```

2. 紙に祈る

3. Emacsを立ち上げる

Type the command:

```bash
M-x slime
```
