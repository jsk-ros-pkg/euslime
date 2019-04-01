euslime
=======

Slime for Euslisp

## Author

Yuki Furuta <<furushchev@jsk.imi.i.u-tokyo.ac.jp>>

Affonso Guilherme de Campos <<affonso@jsk.imi.i.u-tokyo.ac.jp>>

## Setup

1. Clone packages
    ```bash
    mkdir euslime_dir
    cd euslime_dir/
    git clone https://github.com/furushchev/euslime.git
    git clone https://github.com/slime/slime.git
    git clone https://github.com/deadtrickster/slime-repl-ansi-color.git
    ```

1. Install `euslime'
    ```bash
    sudo pip install -U -e euslime
    ```

    This enable to run `euslime`.

1. Setup documentation [PROVISORY]

   Until https://github.com/euslisp/EusLisp/pull/359 gets merged. **Use absolute paths and ensure having a '/' in the end**.
    ```bash
    svn checkout https://github.com/Affonso-Gui/EusLisp/branches/update-docs/doc/latex
    # Uncomment and complete the following line in `euslime/slime-util.l`:
    `(setq help::*eus-tex-dir* "/path/to/latex/")`
    ```

1. Fix ROSINFO/ROSWARN bug on ros-kinetic [OPTIONAL]

   ROSINFO and ROSWARN messages are not displayed in ROS Kinetic. To fix this bug, install roseus from source and run the following commands on the roseus workspace:
    ```bash
    git clone https://github.com/ros/rosconsole
    catkin build rosconsole roseus
    ```

1. Add script to your emacs init file

    ```lisp
    ;; ~/.emacs.el
    (add-to-list 'load-path "/path/to/euslime_dir/slime")
    (add-to-list 'load-path "/path/to/euslime_dir/euslime")
    (add-to-list 'load-path "/path/to/euslime_dir/slime-repl-ansi-color")
    (require 'slime-autoloads)
    (require 'euslime)
    (setq inferior-lisp-program "sbcl")
    (setq inferior-euslisp-program "roseus")
    (setq slime-contribs '(slime-fancy slime-repl-ansi-color))
    ```

1. Launch emacs

    Type the command:

    ```bash
    M-x euslime
    ```
