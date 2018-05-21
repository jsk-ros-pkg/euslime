euslime
=======

Slime for Euslisp

## Author

Yuki Fututa <<furushchev@jsk.imi.i.u-tokyo.ac.jp>>

## Setup

1. Install `pip`

    ```bash
    sudo apt install python-setuptools
    sudo easy_install pip
    sudo pip install -U pip setuptools
    ```

1. Install `euslime`

    ```bash
    git clone https://github.com/furushchev/euslime
    cd euslime
    sudo pip install -U -e .
    ```

    This enable to run `euslime`.

1. Add script to your emacs init file

    ```lisp
    ;; ~/.emacs.el
    (add-to-list 'load-path "/path/to/slime")
    (add-to-list 'load-path "/path/to/euslime")
    (require 'slime-autoloads)
    (require 'euslime)
    (setq slime-contribs '(slime-fancy))
    (setq inferior-lisp-program "sbcl")
    ```

1. Launch emacs

    Type the command:

    ```bash
    M-x euslime
    ```
