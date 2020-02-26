;;(in-package :scratch)
;;(slurp :string (run "ls" '("sort" "-r")))
(ql:quickload :run)
(run:run "ls")

(defpackage run
  (:use :cl :series)
  (:export
   :run
   :to-stream
   :slurp
   :cwd
   #+sbcl :make-pipe-stream
   ))
(in-package :run)

(defun run (first-command &rest commands &aux last-process-out)
  (labels ((rec (commands)
             (setq last-process-out (run% (car commands) last-process-out))
             (when (cdr commands)
               (rec (cdr commands)))))
    (rec (cons first-command commands))
    last-process-out))

(defun run% (command in)
  (check-type in (or stream null))
  (unless (listp command) (setq command (list command)))
  (uiop:process-info-output (uiop:launch-program command :input in :output :stream :directory *default-pathname-defaults*)))

(run% "ls" nil)

(defun to-stream (process)
  (uiop:process-info-output process))

(defun slurp (processor in)
  "Processor is the same as the uiop:slurp-sinput-stream's 0th argument.
:string, t, (the stream x), (the pathname x) and so on."
  ;;(collect 'string (scan-stream (uiop:process-info-output process) #'read-char))
  (uiop:slurp-input-stream processor in))

(defun logd (&rest args)
  (apply #'format t args)
  (write-char #\Newline)
  (finish-output *standard-output*))

(defun cwd ()
  (values *default-pathname-defaults* (uiop:getcwd)))

(defsetf cwd () (dir)
  `(let ((,dir (truename ,dir)))
     (setq *default-pathname-defaults* ,dir)
     (uiop:chdir ,dir)))

(defun walk (dir fn) (error "wip"))

(defun make-pipe-stream ()
  #-sbcl(error "sbcl only")
  #+sbcl
  (multiple-value-bind (in out) (sb-posix:pipe)
    (let* ((in (sb-sys:make-fd-stream in :input t :element-type :default))
           (out (sb-sys:make-fd-stream out :output t :element-type :default))
           (pipe (make-two-way-stream in out)))
      pipe)))


#+nil(defun pathname-values (pathname)
       (let ((pathname (uiop:coerce-pathname pathname)))
         (let* ((name (pathname-name pathname))
                (type (pathname-type pathname))
                (directory-string (princ-to-string (make-pathname :name nil :type nil :defaults pathname)))
                filename-string)
           (setq filename-string
                 (cond
                   ((and (not (eql :uncertain name))
                         (not (eql :uncertain type)))
                    (format nil "~a.~a" name type))
                   ((not (eql :uncertain name)) name)
                   ;; may not reach because ".txt" -> name=.txt type=:unsertain
                   ((not (eql :uncertain type)) (format nil ".~a" type))
                   (t nil)))
           (values filename-string directory-string))))

(defun pathname-values (pathname)
  "example:
   (multiple-value-bind (file dir) (pathname-values x)
     (if file
         (when (ppcre:scan \"lisp$\" file) (princ \"x is lisp\"))
         (drill-down-to-directory-code...)))
  "
  (let ((pathname (uiop:coerce-pathname pathname)))
    (let* ((file (file-namestring pathname))
           (directory (directory-namestring pathname)))
      (when (equal "" file) (setq file nil))
      (values file directory))))


