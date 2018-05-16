euswank
=======

Swank Proxy for Euslisp

## Author

Yuki Fututa <<furushchev@jsk.imi.i.u-tokyo.ac.jp>>

## 使い方（うろおぼえ）

1. Add sctipt to init file

```
;; ~/.emacs.d/init.el
;;
;; pathを通す
(add-to-list 'load-path "/path/to/euswank") 
;; SBCLをデフォルトのCommon Lisp処理系に設定
(setq inferior-lisp-program "sbcl")
;; ~/.emacs.d/slimeをload-pathに追加
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
;; SLIMEのロード
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner slime-euswank)) 
```

2. 紙に祈る
