## Introduction

In order to deliver an enhanced user experience, the slime framework relies on a communication paradigm much more complex than the simple streamed input/output adopted by the original EusLisp interpreter.
In general the slime framework demands much more information -- such as distinguishing error messages from evaluation results, or user input consumed by user-invoked `read` functions from that consumed by the main REPL --,
synchronized requests -- the opposite to the streamed input/output which is fundamentally asynchronous --, and multiple channels of communication -- to handle some of the secondary functionality.

The most difficult point in the implementation of euslime is to expand the original streamed input/output paradigm to meet the slime criteria, while also having to cope with limited access to the EusLisp engine -- we can and do implement customization on the lisp level, but c-language related issues are much more difficult to resolve.

In this technical report we provide a more detailed discussion on each of the most important slime requirements and features, as well as on the adopted solutions for each of them. We hope that this will prove helpful for future maintainers and provide insights on futile implementation methodology.

## REPL evaluation

The most important element of an interpreter is indubitably the Read Eval Print Loop (REPL).

In the slime framework when the user inputs an evaluation request in the prompt and presses enter an indexed emacs request `swank-repl:listener-eval` is issued. This request is then expected to be sent for evaluation, yielding any possible intermediate outputs, the evaluation result itself, a possible change in the prompt, and finally a message signalizing completion of the request, which is also numbered and must match the request index. It is important to stress that in the slime framework any pending requests -- that is, any request that has not yet received its completion message -- usually blocks the evaluation of any further request, both REPL and non-REPL related.

In euslime this is done by implementing a customized lisp REPL function ([slime-toplevel.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-toplevel.l)) which prints a termination token after each evaluation and communicates the result through socket.
The termination token is important to delimit the end of the request while ensuring that all possible intermediate outputs were safely communicated to the emacs interface and displayed before the evaluation result and a novel prompt.
For standard evaluation the socket channel for communicating the results could be omitted, but it is implemented as it proves useful for handling more complex situations such as errors, abortions and other anomalies during the evaluation.
The termination token defaults to the group separator character followed by the string "euslime-token-" and then the number of the port opened for socket communications on the given stream, and is removed from every displayed output.
Although the deliberate output of such a token by the user could disrupt the REPL evaluation and cause synchronization issues, since both the token and the socket message result are needed to finish an evaluation it is more likely that it will only be concealed from the displayed output without further side effects.

In the python wrapper the output channel (standard output and standard error) is constantly monitored on a separate thread, which forwards output for emacs display through the `:write-string` statement. This thread also verifies if each output contains a termination token, setting the `finished_output` event if a match is found ([bridge.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/bridge.py)).
During normal REPL evaluation, the python wrapper firstly forwards the expression to the EusLisp piped input and then monitors the socket channel for any incoming messages. When a result message is received the wrapper then verifies and possibly waits for the reception of a termination token, and then sends the evaluation result for display using the `:write-string :repl-result` statement. Then, the python wrapper sends an internal evaluation request for the `slime-prompt` utility function through socket communication, which verifies if the previous evaluation has caused any change in the prompt string, in which case it is reported through the `:new-package` statement. Finally, a message signalizing the end of the request is issued, leading the emacs to display another prompt and await for the next evaluation request.

A full example of such process is given below, and can be observed in the `*slime-events*` buffer on emacs.
Here an emacs request from the "USER" package and indexed as number 16 for the evaluation of the expression `(format t "Hello")` is issued, causing the display of the intermediate output "Hello", followed by the evaluation result `nil` and then a new line. Finally, a `:return` statement for the request number 16 and status of `:ok` is signalized, officially completing the request.

```lisp
(:emacs-rex
 (swank-repl:listener-eval "(format t \"Hello\")\n")
 "USER" :repl-thread 16)
(:write-string "Hello")
(:write-string "nil" :repl-result)
(:write-string "\n" :repl-result)
(:return
 (:ok nil)
 16)
```


#### Handling read functions

The slime framework demands for user input to be distinguished between input consumed by the main REPL and input consumed by other read invocations -- such as `y-or-n-p` calls.
User input consumed by the main REPL is issued as emacs requests calling for the `:swank-repl:listener-eval` statement, which triggers the complex synchronous process explained above.
User input provided as answers to read requests, on the other hand, is issued as a detached call for the `:emacs-return-string` statement, which is simply forwarded to the input stream without any requirements for termination tokens, completion messages or any other feedback whatsoever.

Every time the user presses enter, what determines if the input will be labeled as `:swank-repl:listener-eval` or `:emacs-return-string` is the absence or presence of a previous and unresolved `:read-string` statement, respectively. Both in original slime and euslime this works by implementing a customized input stream which dispatch such `:read-string` statements every time it is attempted to be read from ([slime-connection.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-connection.l)).


#### Handling errors

In the slime framework errors are displayed in a separate buffer, which provides the user a clean interface to examine the callstack and opt to continue or reset the REPL level.
When an error is detected a `:debug` call containing basic information on the error and possible restarts is communicated to the emacs and used to initialize the debugger buffer. The selection of a restart option or the closing of the debugger buffer then dispatch `swank:invoke-nth-restart-for-emacs` or `swank:throw-to-toplevel` emacs requests, respectively. The chosen restart option is then communicated to the client process, and three completion messages indicating that the restart request, the debugger, and the original evaluation request have all been processed and finished normally are issued. It is important to stress that the slime framework will not resume operation and accept the next evaluation request until all pendant requests have been processes -- that is, until all of the above three completion messages have been received.

In euslime this is done by implementing a custom error handler which communicates errors through socket ([slime-connection.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-connection.l)) and a system in the python wrapper capable of keeping track of each active debugger and request id in order to ensure that all requests are safely terminated ([handler.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/handler.py)). In such system any error occurring during evaluation leads to the creation of a `DebuggerHandler`  object containing information on the error, the level (depth) of the debugger being created and the associated request id. Such information is later accessed when the user opts for a restart option, and used to issue the respective completion messages.

A full example of such process is given below, and can be observed in the `*slime-events*` buffer o
n emacs.
Here an emacs request `swank-repl:listener-eval` from the "USER" package and indexed as number 6 causes the evaluation of the faulty expression `(1+ nil)`, triggering an error which leads to the creation of a debugger on thread 0 with level 1. This is signalized through the `:debug` method, which also holds information on the error message, possible restart options, and the callstack. In this case the user then selects the "QUIT" restart option, which issues the emacs request `swank:invoke-nth-restart-for-emacs` on debugger level 1 with the restart option 0 (meaning the first on the restart list), issued from the package "USER" and indexed as 7. This is then communicated to EusLisp, which executes a `reset` command that unwinds to the top REPl level. Finally, the python wrapper uses `:return` and `:debug-return` statements to communicate that the emacs request number 7, the debugger on thread 0 and level 1, and the emacs request number 6 have all been completed (or, more precisely, aborted).

```lisp
(:emacs-rex
 (swank-repl:listener-eval "(1+ nil)\n")
 "USER" :repl-thread 6)
(:debug 0 1
        ("Integer expected in (1+ nil)" "" nil)
        (("QUIT" "Quit to the SLIME top level")
         ("CONTINUE" "Ignore the error and continue in the same stack level")
         ("RESTART" "Restart euslisp process"))
        ((0 "(1+ nil)"
            (:restartable nil))
         (1 "(slime:slimetop)"
            (:restartable nil))
         (2 "(slime:slimetop)"
            (:restartable nil))
         (3 "#<compiled-code #X556e5b44c3e8>"
            (:restartable nil)))
        (nil))
(:emacs-rex
 (swank:invoke-nth-restart-for-emacs 1 0)
 "USER" 0 7)
(:return
 (:abort nil)
 7)
(:debug-return 0 1 nil)
(:return
 (:abort "'Integer expected'")
 6)
 ```

**Restart options**

The default restart options are to return to the REPL top level ("QUIT"), to continue on the same error stack ("CONTINUE") and to restart the whole session ("RESTART").
Fatal errors only provide the "RESTART" option, and internal errors -- errors derived from the evaluation of secondary, non-REPL related, requests -- only provide the "CONTINUE" option.

Closing the debugger buffer with the `q` key or sending any subsequent attempt for REPL evaluation while a debugger session is still active are correspondent to invoking the first possible restart option -- "QUIT" or the only available one for fatal and internal errors.
This is chosen by design to encourage the user to handle the error before moving forward to the next evaluation.

It is, however, still possible to trigger nested errors through the evaluation of faulty internal requests (such as with the `slime-compile-defun` command). This leads to the creation of debugger sessions with consecutively higher levels (depth).
When dealing with nested errors, the restart options for "QUIT" and "RESTART" will cause to exit all active debuggers, while "CONTINUE" will only resolve the debugger of higher level and prompt the user on the handling of the next debugger on stack. This is illustrated in the following example.

```lisp
(:emacs-rex
 (swank-repl:listener-eval "(1+ none)\n")
 "USER" :repl-thread 24)
(:debug 0 1
        ("Unbound variable none in (1+ none)" "" nil)
        (("QUIT" "Quit to the SLIME top level")
         ("CONTINUE" "Ignore the error and continue in the same stack level")
         ("RESTART" "Restart euslisp process"))
        ((0 "(1+ none)"
            (:restartable nil))
         (1 "(slime:slimetop)"
            (:restartable nil))
         (2 "(slime:slimetop)"
            (:restartable nil))
         (3 "#<compiled-code #X556e5b44c3e8>"
            (:restartable nil)))
        (nil))
(:emacs-rex
 (swank:compile-string-for-emacs "(1+ nil)\n" "test.l"
                                 '((:position 1)
                                   (:line 1 1))
                                 "/tmp/test.l" 'nil)
 "USER" t 25)
(:debug 0 2
        ("Integer expected in (1+ nil)" "" nil)
        (("QUIT" "Quit to the SLIME top level")
         ("CONTINUE" "Ignore the error and continue in the same stack level")
         ("RESTART" "Restart euslisp process"))
        ((0 "(1+ nil)"
            (:restartable nil))
         (1 "(progn (1+ nil))"
            (:restartable nil))
         (2 "slime:slime-error"
            (:restartable nil))
         (3 "slime:slime-error"
            (:restartable nil))
         (4 "(1+ none)"
            (:restartable nil))
         (5 "(slime:slimetop)"
            (:restartable nil))
         (6 "(slime:slimetop)"
            (:restartable nil))
         (7 "#<compiled-code #X556e5b44c3e8>"
            (:restartable nil)))
        (nil))
(:emacs-rex
 (swank:invoke-nth-restart-for-emacs 2 1)
 "USER" 0 26)
(:return
 (:abort nil)
 26)
(:debug-return 0 2 nil)
(:return
 (:abort "'Integer expected'")
 25)
(:debug 0 1
        ("Unbound variable none in (1+ none)" "" nil)
        (("QUIT" "Quit to the SLIME top level")
         ("CONTINUE" "Ignore the error and continue in the same stack level")
         ("RESTART" "Restart euslisp process"))
        ((0 "(1+ none)"
            (:restartable nil))
         (1 "(slime:slimetop)"
            (:restartable nil))
         (2 "(slime:slimetop)"
            (:restartable nil))
         (3 "#<compiled-code #X556e5b44c3e8>"
            (:restartable nil)))
        (nil))
(:emacs-rex
 (swank:invoke-nth-restart-for-emacs 1 1)
 "USER" 0 27)
(:return
 (:abort nil)
 27)
(:new-package "USER" "E2-irteusgl")
(:debug-return 0 1 nil)
(:return
 (:abort "'Unbound variable none'")
 24)
```

**Call Stack formatting**

Part of the information sent in the `:debug` method is the current callstack. Because this information is administered by the EusLisp in a c-language level and cannot be simply reconfigured by custom error handlers, in euslime we take a textual approach based on accumulating and parsing the piped output.

Firstly, upon initialization the default `lisp::*max-callstack-depth*` is set to zero, suppressing any possible callstack messages. Then, when the occurrence of an error is communicated through socket the python wrapper sends an evaluation request for `slime:print-callstack` ([slime-connection.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-connection.l)), which sets the callstack depth to a desired value and incites a dummy error, collecting all of the piped output during the process. The collected output is then parsed in order to adapt the output to the demanded format and to remove any segments related to the dummy error. Finally, the REPL is rewinded one level to undo the influence of the dummy error and the `:debug` method is molded and dispatched.

This system also allows to dynamically change the displayed callstack depth, which is by default expanded every time the user selects the `more` option at the bottom of the displayed callstack, triggering the `swank:backtrace` emacs request.


**Handling fatal errors**

The current euslime version has poor handling of segmentation faults and other fatal errors. This is because such errors trigger a c-language defined error handler, instead of the customized one defined in the lisp layer.
As a consequence, euslime is unable to detect fatal errors through socket communication, and instead matches a typical fatal error message string against every received output ([server.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/server.py)), in a process similar to the search for termination tokens.

Because we cannot be sure whether such error message templates are user issued or represent a true fatal error, we opt to avoid the standardized slime error handling process and instead enter what we call the _read-mode_.
During read-mode a direct, unprocessed link between the emacs interface and the piped euslisp process is established, streaming all input from emacs directly to the euslisp process and all output from the euslisp process directly to the emacs interface -- as it would normally happen in the default shell interpreter.

Upon the occurrence of a fatal error, the read mode provides the user raw access to the c-language evaluation loop handler triggered by such errors, being able to inspect variables and reset to a higher level. Because such read-mode is ensured to be deactivated when every evaluation request is completed, it is hard --but not impossible -- for the user to design print statements containing such template strings that would lead to system crashes.
One problem, however, is that because in euslime the streams originated from the euslisp backend are not tty, the default c-language evaluation loop does not display any prompts (a "Fatal: " message would be usually displayed), leaving the user to an inactive screen that, although fully functional and responsive, must be guessed as being so.


#### Handling reset and other top level throws

Throw statements to different REPL levels are challenging because they provide an opportunity for code to be completed without reaching the end of the evaluation function, where the communication of request completion takes place. Without proper addressing, this would lead to situations in which the slime framework indefinitely waits for the response from a request that has in fact already finished.

In euslime this is solved by introducing an abort socket message at the beginning of the `reploop` function ([slime-toplevel.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-toplevel.l)).
Since catch statements for each `*replevel*` are designed to re-invoke such `reploop` function, the abort message ensures that direct calls for the `reset` function and other throws successfully return a socket response which terminates the ongoing request. In addition, this implementation also proves to be useful when dealing with direct calls for the `reploop` function itself, in which case the emacs request is immediately terminated and a new fully functional slime prompt is introduced, instead of the default behavior of adopting streamed read-mode communications until the original request is solved. When `reploop` is invoked from within error handlers, such abort message are shaded by prior socket communications communicating the error.


#### Handling unix commands

The euslisp allows to start unix commands directly for the REPL, a feature which is not supported in the original slime framework.

In euslime this is dealt with by entering what we call the _read-mode_.
During read-mode a direct, unprocessed link between the emacs interface and the piped euslisp process is established, streaming all input from emacs directly to the euslisp process and all output from the euslisp process directly to the emacs interface -- as it would normally happen in the default shell interpreter.
This raw exchange of information makes possible to run unix commands on euslime mostly as they would run in the original shell interpreter, although with some notable differences related to the fact that stream in euslime are not tty. A visible consequence of this is that eus commands will not print any prompt, although being fully responsive and functional.

This is illustrated in the following example. Please note how entering read-mode the information is sent as `:emacs-return-string` calls instead of emacs requests for `swank-repl:listener-eval`, and how no particular prompts are displayed.

```lisp
(:emacs-rex
 (swank-repl:listener-eval "eus\n")
 "USER" :repl-thread 5)
(:read-string 0 1)
(:emacs-return-string 0 1 "(1+ 1)\n")
(:read-string 0 1)
(:write-string "2\n")
(:emacs-return-string 0 1 "quit\n")
(:read-string 0 1)
(:write-string "0" :repl-result)
(:write-string "\n" :repl-result)
(:read-aborted 0 1)
(:return
 (:ok nil)
 5)
```

#### Handling interruptions

Pressing `C-c C-c` during an euslime session triggers the `:emacs-interrupt` command, which then sends a SIGINT signal to the emacs process and triggers a custom signal handler which emits an abort signal through socket communication ([slime-connection.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-connection.l)).

During read-mode, a SIGINT signal is instead send to the (negative of the) process group id, causing it to also be propagated to any child processes, such as possible unix commands.


## Expanded functionality and helper functions

Far beyond enhanced REPL display and formatting, the slime framework also provides abundant secondary utility such as symbolic completion, automatic documentation, code search, integration with different emacs buffers, and many others.

Euslime currently supports only a fraction of such utility, which are stated on [handler.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/handler.py) in a bottom-up fashion, and commonly rely on helper functions defined on [slime-util.l](https://github.com/jsk-ros-pkg/euslime/blob/master/slime-util.l).

Although sometimes complex, the real challenge in such implementation is not the coding itself, but rather the configuration of an even broader communication network capable of handling simultaneous requests. In the current version, this unrolls into the following communication channels:

- piped I/O, mainly used for standard REPL evaluation
- socket output for communicating the status of the REPL request (result, error, abort, read, read-mode)
- socket input, running as the main loop on another thread, for simultaneous handling of secondary utility
- socket input, running on the port-selector on the main thread, for handling secondary requests that demand access to thread specific variables (_e.g._ *replevel*, *reptype*, *irtviewer*)

Currently, the list of supported features executed in the parallel thread are:
```
swank:simple-completions
swank:fuzzy-completions
swank:completions-for-character
swank:find-tag-name-for-emacs
swank:describe-symbol
swank:describe-function
swank:describe-definition-for-emacs
swank:swank-expand-1
swank:list-all-package-names
swank:apropos-list-for-emacs
swank:default-directory
swank:set-default-directory
```

The list of supported features executed on the main thread are:
```
swank:create-repl
swank-repl:create-repl
swank:autodoc
swank:operator-arglist
swank:completions
swank:completions-for-keyword
swank-repl:clear-repl-variables
swank:set-package
```

And the list of supported features related to the piped I/O are:
```
:emacs-return-string
swank:eval
swank:interactive-eval
swank:interactive-eval-region
swank:pprint-eval
swank-repl:listener-eval
swank:backtrace
swank:compile-string-for-emacs
swank:compile-notes-for-emacs
swank:compile-file-for-emacs
swank:load-file
```

Because the socket communication with the main thread is orchestrated by the unix select method on the top selector, it is only processed when the thread is available. Therefore, it is not capable of handling simultaneous requests, which can in some times lead to certain glitches, such as when attempting to execute the `swank:set-package` command during active REPL evaluation. In such case, because the emacs is expecting an immediate result from the `swank:set-package` request, the whole interface freezes until the REPL evaluation has been finished and the given request has been processed and returned.

It is also worth noted that compile and load related functionality are fed the the piped input channel, as standard REPL requests. This is done in order to provide more transparent error handling, avoiding to classify such as internal errors and deprive some of the restart options.
It is thought to be good practice to require REPL-like evaluation with any functionality that potentially requires the evaluation of arbitrary user-provided expressions, although this implies updating of the REPL history and variables, and is also to be weighted with possible requirements for simultaneous evaluation. Because of such, the `swank:expand-1` is opted to be implemented through socket communications on the parallel thread instead.

Finally, the implications multi-threading have on memory stability are yet to be fully verified, although we are optimistic about such because of the fundamentally read-only nature of the functionality.


## Test framework

The euslime test framework can be found on the [tests](https://github.com/jsk-ros-pkg/euslime/tree/master/tests) directory, although yet in a rudimentary level.

Each of the tests typically describes one or more input requests and their expected result. Each input request is wrapped in a `:euslime-test` statement which allows to weave a message signalizing process termination ([protocol.py](https://github.com/jsk-ros-pkg/euslime/blob/master/src/euslime/protocol.py)), and then sent to the euslime for evaluation. The obtained result is than matched against the expected result, according to the following functions:
- `assertSocket`: evaluates a single request, ensuring that the evaluation result matches the expected result in strict order.
- `assertSocketSameResult`: evaluates multiple requests, in order, ensuring that all of them yield the same result.
- `assertSocketPossibleResults`: evaluates a single request, ensuring that the evaluation result is included in a list of possible expected results.
- `assertSocketWriteString`: evaluates a single request, ensuring that the evaluation result matches the expected result in such a way that the displayed message is the same. This is useful because output can be split between multiple `:write-string` calls at inconstant points.
- `assertSocketIgnoreAddress`: evaluates a single request, ensuring that the evaluation result matches the expected result in strict order, but ignoring any output segments related to pointer addresses.
- `assertAsyncRequest`: evaluates multiple request, in order and sent in a defined time interval, ensuring that the evaluation result matches the expected result. The matching can be performed in order or not, and opt to ignore pointer addresses.

The test framework includes six different test suites for the programs `eus`, `irteusgl` and `roseus`, with color mode enabled and disabled. Each test suite inherits tests from each other as follows:

- eus
  - eus_color
  - irteusgl
    - irteusgl_color
    - roseus
      - roseus_color

One big challenge of implementing the test framework is ensuring that the result of each test does not influence the outcome of the other tests, which is partly assured by incorporating clean up statements within relevant tests and ensuring that they are executed even if an error occurs.