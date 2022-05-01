euslime
=======

Interactive interpreter for EusLisp with support for completion, on-spot referencing, auto-documetation, definition search, and more. Euslime is built on top of [slime](https://slime.common-lisp.dev/) and runs on emacs.

For a quick guide to emacs try the following links:
- [GNU Emacs - Guided Tour](https://www.gnu.org/software/emacs/tour/index.html)
- [Absolute Beginner's Guide to Emacs](http://www.jesshamrick.com/2012/09/10/absolute-beginners-guide-to-emacs/)
- [Prof. Inaba Suggested Commands](https://gist.github.com/Affonso-Gui/7280b1b1ac02a39bf798bc97d3a341b6)

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
| C-c TAB | completion |
| C-c C-c | load expression |
| C-c C-l | load-file |
| C-c C-d o |  go back to repl buffer |

| On other slime buffers | |
| --- | --- |
| q | quit buffer |
| [RET] | select option |


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

## Run tests
```shell
# Run all tests
tests/euslime_tests.py

# RUN TESTS FOR EUSLISP PROGRAM
tests/euslime_tests.py eus
tests/euslime_tests.py irteusgl
tests/euslime_tests.py roseus

# RUN A SINGLE TEST
tests/euslime_tests.py eus.test_eval_1

# RUN MATCHING TESTS
tests/euslime_tests.py eus.test_eval_*
tests/euslime_tests.py *.test_eval_1
```

## How it Works

Euslime is composed by several layers of software, ultimately connecting the emacs interface to the EusLisp interpreter.

![euslime-communications](https://user-images.githubusercontent.com/20625381/89138044-2cef6d80-d575-11ea-9923-5eac3dd9c8cc.jpg)

**EMACS** acts as the front-end interface, accepting user input and displaying output correspondingly. It also provides a series of modes, commands and key bindings for improved user experience, which are introduced at the original slime framework and slightly customized at [euslime.el](https://github.com/jsk-ros-pkg/euslime/blob/master/euslime.el.in) and [euslime-config.el](https://github.com/jsk-ros-pkg/euslime/blob/master/euslime-config.el).

**SWANK** is the original slime backend, which handles interactions with emacs by delimiting a protocol that translates emacs commands into s-expressions. Such expressions are sent to and from the inferior lisp client (originally Common Lisp, in this case EusLisp) during an exchange which is logged at the `*slime-events*` buffer on emacs. A simple example is given below, illustrating an evaluation request for `(1+ 1)` followed by an output request for `2`.
```
(:emacs-rex
 (swank-repl:listener-eval "(1+ 1)\n")
 "COMMON-LISP-USER" :repl-thread 6)
(:write-string "2" :repl-result)
```

**PYTHON WRAPPER** is responsible for encoding and decoding swank commands into actual EusLisp input/output, as well as managing the communications between the swank and the EusLisp layers.
In the above example, this means that the python middleware would forward the expression `(1+ 1)` for evaluation in the EusLisp client, and then wrap the result into a suitable `:write-string` form which is finally sent to the emacs in order to display it on the screen.
Although such functionality was originally implemented on slime as a part of the native common lisp framework, here we opt to establish an additional python layer due to its ability to (i) handle multithreading; (ii) cope with EusLisp crashes without compromising the emacs session; and (iii) minimize changes done to the EusLisp lexical environment.

The python middleware is divided into six files with well determined functionality, as detailed in the following.

[server.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/server.py) configures a socket server that communicates with the swank layer, receiving and answering to requests.

[protocol.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/protocol.py) parses incoming s-expressions into python functions, and also defines utility used to generate common swank forms, such as evaluation results or errors.

[handler.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/handler.py) holds the actual definitions of the python functions responsible for handling the swank requests. For instance, the `swank_repl_listener_eval` function which answers to `:swank-repl:listener-eval` requests.

[bridge.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/bridge.py) deals with the communications with the EusLisp layer. The EusLisp interpreter is started as a subprocess and interacts with the python layer through pipes and sockets. Pipes are used to pass user input and program output, while sockets are used to transmit evaluation results, errors, and to process other internal requests while avoiding updating the REPL history and variables.

[logger.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/logger.py) configures the logging function, which is displayed in the `*inferior-lisp*` emacs buffer.

[cli.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/cli.py) bundles all of the above and arranges command line arguments for an euslime executable invoked upon startup.

Finally, in the **EUSLISP** layer a new package named `SLIME` is defined and a few overwrites are performed in order to establish a framework for properly dealing with socket communications and handler requests. Such functionality is distributed among the following three files:

[slime-util.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-util.l) gathers utility function designed to promptly respond to handler requests which would be hard to define solely with python coding, such as autodoc and completion.

[slime-connection.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-connection.l) configures the socket communication framework, defining two different socket channels: one started from another thread allowing parallel evaluation and the other added to the top-selector allowing access to thread special variables. A custom input stream which communicates to swank at every read attempt is also defined here, allowing for the slime framework to differentiate user input answering to `read` functions from new evaluation requests.

[slime-toplevel.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-toplevel.l) mainly overwrites lisp repl functions and error handlers so that they can send suitable socket information at each step of the evaluation. It is also responsible for setting up the sockets and streams defined at slime-connection and starting the main evaluation loop.


A more detailed explanation can be found in [technical-report.md](https://github.com/jsk-ros-pkg/euslime/blob/master/technical-report.md).
