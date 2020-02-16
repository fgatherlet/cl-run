# Run - run external program easily.

## Usage

This is simple and thin `run-program` wrapper.

```
$ ros install fgatherlet/cl-run
$ ros run -s run
(run:to-text (run:run "ls" "sort" (list "tr" "'[a-z]'" "'[A-Z]'")))
;; print result of execution. similar to shell script `ls | sort | tr '[a-z]' '[A-Z]'

(let ((str (run:to-stream (run:run "ls" "sort" (list "tr" "'[a-z]'" "'[A-Z]'")))))
  (with-open-file (os "/tmp/result" :direction :output :if-exists :supersede)
    (series:collect-stream os
      (series:scan-stream str #'read-char)
    #'write-char)))
;; like shell redirection.
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
