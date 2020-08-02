euslime
=======

Slime for EusLisp


## Quick Start

1. Install

    ```bash
    apt install ros-melodic-euslime
    ```

1. Configure your emacs init file

    ```lisp
    ;; ~/.emacs.el
    (add-to-list 'load-path "/opt/ros/melodic/share/euslime")
    (require 'euslime-config)
    (setq inferior-euslisp-program "roseus")
    (slime-setup '(slime-fancy slime-banner slime-repl-ansi-color))
    ```

1. Run

    Open emacs and type the command:

    ```bash
    M-x euslime
    ```

## Build from Source

1. Setup
    ```bash
    # Clone code
    mkdir ~/euslime_ws/src -p
    cd euslime_ws/src
    git clone https://github.com/jsk-ros-pkg/euslime.git
    # Update submodules
    cd euslime
    git submodule init
    git submodule update
    # Install dependencies
    rosdep install -yr --from-paths . --ignore-src
    ```

1. Build
    ```bash
    cd ~/euslime_ws
    catkin config --install
    catkin build

1. Configure your emacs init file

    ```lisp
    ;; ~/.emacs.el
    (add-to-list 'load-path "~/euslime_ws/install/share/euslime")
    (require 'euslime-config)
    (setq inferior-euslisp-program "roseus")
    (slime-setup '(slime-fancy slime-banner slime-repl-ansi-color))
    ```

1. Run

    Source the package

    ```bash
    source ~/euslime_ws/install/setup.bash
    ```

    Then open emacs and type the command:

    ```bash
    M-x euslime
    ```

## Cheat sheet

| On slime buffer | |
| --- | --- |
| [TAB] | completion |
| C-c C-d d |  describe/ help |
| C-c C-d a |  apropos |
| C-c C-d p |  apropos package |
| M-.  |  look for definition |
| C-c [RET] |  macroexpansion |
| ,quit  |  quit session |
| ,restart-inferior-lisp  |  restart session |

| On editing buffers | |
| --- | --- |
| C-c C-c | load expression |
| C-c C-l | load-file |

| On other slime buffers | |
| --- | --- |
| q | quit buffer |
| [RET] | select option |


## How it Works

Euslime is composed by several layers of software, ultimately connecting the emacs interface to the EusLisp interpreter.


**EMACS** acts as the front-end interface, accepting user input and displaying output correspondingly. It also provides a series of modes, commands and key bindings for improved user experience, which are introduced at both [euslime.el](https://github.com/jsk-ros-pkg/euslime/blob/master/euslime.el.in), [euslime-config.el](https://github.com/jsk-ros-pkg/euslime/blob/master/euslime-config.el), and at the original slime framework.

**SWANK** is the original slime backend, which handles interactions with emacs by delimiting a protocol that translates emacs commands into s-expressions. Such expressions are sent to and from the inferior lisp client (originally Common Lisp, in this case EusLisp) during an exchange which is logged at the `*slime-events*` buffer on emacs. A simple example is given below, illustrating an evaluation request for `(1+ 1)` followed by an output request for `2`.
```
(:emacs-rex
 (swank-repl:listener-eval "(1+ 1)\n")
 "COMMON-LISP-USER" :repl-thread 6)
(:write-string "2" :repl-result)
```

**PYTHON WRAPPER** is responsible for encoding and decoding swank commands into actual EusLisp input/output, as well as managing the communications between the swank and the EusLisp layers. Although such functionality was implemented on slime as a part of the native common lisp framework, here we opt to establish an additional python layer due to its ability to (i) handle multithreading; (ii) cope with EusLisp crashes without compromising the emacs session; and (iii) minimize changes done to the EusLisp lexical environment.

The python middleware is divided into six files with well determined functionality, as detailed in the following.

[server.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/server.py) configures a socket server that communicates with the swank layer.

[protocol.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/protocol.py) parses incoming s-expressions into python functions, and also defines utility used to generate common swank forms, such as evaluation result or errors.

[handler.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/handler.py) holds the actual definitions of the python functions responsible for handling the swank requests.

[bridge.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/bridge.py) deals with the communications with the EusLisp layer. The EusLisp interpreter is started as a subprocess and interacts with the python layer through pipes and sockets. Pipes are used to pass user input and program output, while sockets are used to transmit evaluation results, errors, and to process other request while avoiding updating the REPL history and variables.

[logger.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/logger.py) configures the logging function, which is displayed in the `*inferior-lisp*` emacs buffer.

[cli.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/cli.py) bundles all of the above and arranges command line arguments for an euslime executable invoked upon startup.

Finally, in the **EUSLISP** layer [slime-toplevel.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-toplevel.l) and [slime-util.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-util.l) are loaded into the default interpreter to establish a framework for dealing with socket communications and introduce handler functions that would be hard to define solely with python coding.