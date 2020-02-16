(defpackage run
  (:use :cl :series)
  (:export :run :text :stream))
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

(defun text (process)
  (collect 'string (scan-stream (uiop:process-info-output process) #'read-char)))

(defun stream (process)
  (uiop:process-info-output process))

(defun logd (&rest args)
  (apply #'format t args)
  (write-char #\Newline)
  (finish-output *standard-output*))


