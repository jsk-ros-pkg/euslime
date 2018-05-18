euswank
=======

Swank Proxy for Euslisp

## Author

Yuki Fututa <<furushchev@jsk.imi.i.u-tokyo.ac.jp>>

## Setup

1. Install `pip`

    ```bash
    sudo apt install python-setuptools
    sudo easy_install pip
    sudo pip install -U pip setuptools
    ```

1. Install `euswank`

    ```bash
    git clone https://github.com/furushchev/euswank
    cd euswank
    sudo pip install -U -e .
    ```

    This enable to run `euswank`.

1. Add script to your emacs init file

    ```lisp
    ;; ~/.emacs.el
    (add-to-list 'load-path "/path/to/slime")
    (add-to-list 'load-path "/path/to/euswank")
    (require 'slime-autoloads)
    (require 'euslisp-slime)
    (setq slime-contribs '(slime-fancy))
    (setq inferior-lisp-program "sbcl")
    ```

1. Launch emacs

    Type the command:

    ```bash
    M-x euslisp-slime
    ```
