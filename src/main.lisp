;;(in-package :scratch)
;;(slurp :string (run "ls" '("sort" "-r")))

(defpackage run
  (:use :cl :series)
  (:export
   :run
   :to-stream
   :slurp
   :cwd
   ))
(in-package :run)

(defun run (first-command &rest commands &aux last-process)
  (labels ((rec (commands)
             (setq last-process (run% (car commands) last-process))
             (when (cdr commands)
               (rec (cdr commands)))))
    (rec (cons first-command commands))
    last-process))

(defun run% (command input-process &aux input-stream)
  (unless (listp command) (setq command (list command)))
  (when input-process (setq input-stream (uiop:process-info-output input-process)))
  (uiop:launch-program command :input input-stream :output :stream))

(defun to-stream (process)
  (uiop:process-info-output process))

(defun slurp (processor process)
  "Processor is the same as the uiop:slurp-sinput-stream's 0th argument.
:string, t, (the stream x), (the pathname x) and so on."
  ;;(collect 'string (scan-stream (uiop:process-info-output process) #'read-char))
  (uiop:slurp-input-stream processor (to-stream process)))

(defun logd (&rest args)
  (apply #'format t args)
  (write-char #\Newline)
  (finish-output *standard-output*))

(defun walk (dir fn) (error "wip"))

(defun cwd ()
  (values *default-pathname-defaults* (uiop:getcwd)))

(defsetf cwd () (dir)
  `(let ((,dir (truename ,dir)))
     (setq *default-pathname-defaults* ,dir)
     (uiop:chdir ,dir)))

