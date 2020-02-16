# Run - run external program easily.

## Usage

This is simple and thin `run-program` wrapper.

```
$ ros install fgatherlet/cl-run
$ ros run -Q cl-run
* (run:text (run:run "ls" "sort" (list "tr" "'[a-z]'" "'[A-Z]'")))
;; print result of execution. similar to shell script `ls | sort | tr '[a-z]' '[A-Z]'

* (series:collect-file "/tmp/result.txt"
    (series:scan-stream
      (run:stream (run:run "ls" "sort" (list "tr" "'[a-z]'" "'[A-Z]'")))
      #'read-char)
    #'write-char)
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
