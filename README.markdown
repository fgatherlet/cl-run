# Run - run external program easily.

## Usage

This is simple and thin `run-program` wrapper.
This supports only sbcl.
Because this uses `sb-sys:make-fd-stream` to wrap pipe fd to be stream.

```lisp
$ ros install fgatherlet/cl-run
$ ros -s run run
(run:slurp (run:yarn "ls" "sort" '("tr" "'[a-z]'" "'[A-Z]'")))
;; print result of execution. similar to shell script `ls | sort | tr '[a-z]' '[A-Z]'

(run:slurp (run:yarn "ls" "sort" (list "tr" "'[a-z]'" "'[A-Z]'"))
    #p"/tmp/output")
;; like shell redirection.

(with-open-file (out "/tmp/output" :direction :output :if-exists :append :if-does-not-exist :create)
    (run:slurp (run:yarn "ls" "sort" (list "tr" "'[a-z]'" "'[A-Z]'"))
        out))
;; append redirection. slurp is wrapper of `uiop:sliurp-input-stream`

(run:slurp (run:yarn (lambda ()
                       (dotimes (i 10)
                         (format t "abcde~%")))
                     (lambda ()
                       (loop for x = (read-char *standard-input* nil nil)
                          while x
                          do
                            (format t "[~a]" x)))
                     "cat"))
;; collaborattion of process and thread.

(run:yarn "ls" :i '("ls" "/etc/") :i)
;; `:i` means ignoreing the output of task(process or thread).
```

## Installation

```
$ ros install fgatherlet/fgatherlet
```

## Author

* fgatherlet

## Copyright

Copyright (c) 2020 fgatherlet

## License

Licensed under the MIT License.
