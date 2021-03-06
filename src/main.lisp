(defpackage run
  (:use :cl :series)
  (:export
   :yarn
   :slurp

   :cwd
   :cd
   #+sbcl :make-pipe-stream
   ))
(in-package :run)

(defun make-pipe-stream ()
  #-sbcl(error "sbcl only")
  #+sbcl
  (multiple-value-bind (in out) (sb-posix:pipe)
    (let* ((in (sb-sys:make-fd-stream in :input t :element-type :default))
           (out (sb-sys:make-fd-stream out :output t :element-type :default))
           (pipe (make-two-way-stream in out)))
      pipe)))

;; I want to know a better way.
(defun make-null-output-stream ()
  (open "/dev/null" :direction :output :if-exists :append :element-type :default))


(defclass thread-task ()
  ((input :initarg :input :initform nil)
   (output :initarg :output :initform nil)
   (thread :initarg :thread :initform nil)

   (wait-cvar :initform (bt:make-condition-variable :name "wait-cbar-thread-task"))
   (wait-lock :initform (bt:make-lock "wait-lock-thread-task"))
   ))

(deftype task () `(or uiop/launch-program::process-info
                      thread-task))

;;;;

(defgeneric task-output (task))
(defgeneric task-wait (task))

(defmethod task-wait ((task t))
  ;; do nothing
  )

;;;; process

(defmethod task-output ((task uiop/launch-program::process-info))
  (uiop:process-info-output task))
(defmethod task-wait ((task uiop/launch-program::process-info))
  (uiop:wait-process task))

;;;; thread

(defmethod task-output ((task thread-task))
  (slot-value task 'output))
(defmethod task-wait ((task thread-task))
  (with-slots (thread wait-cvar wait-lock) task
    (bt:with-lock-held (wait-lock)
      (bt:condition-wait wait-cvar wait-lock)
      (format t ">>wait finish~%"))))

;;;;

(defun close-output-stream (stream)
  (typecase stream
    (two-way-stream (close (two-way-stream-output-stream stream)))
    (t (close stream))))

(defun make-thread-task (fn &key (input *standard-input*) (output (make-pipe-stream)))
  (let ((task (make-instance 'thread-task :input input :output output)))
    (setf (slot-value task 'thread)
          (bt:make-thread
           (lambda ()
             (funcall fn)
             (bt:condition-notify (slot-value task 'wait-cvar))
             ;;(finish-output output)
             (close-output-stream output)
             )
           :initial-bindings `(,@bt:*default-special-bindings*
                               (*standard-input* . ,input)
                               (*standard-output* . ,output))))
    ;; bt:*default-special-bindings*
    task))

;;(yarn "ls" "cat" 

(defun yarn1 (in command)
  ;;(check-type in (or task null))
  
  (when (eql :i command)
    ;; wait and ignore input.
    (task-wait in)
    (return-from yarn1 nil))
  
  ;; already yarned.
  (when (typep command 'task) (return-from yarn1 command))
  
  ;;(unless in (setq in *standard-input*))
  
  (let (in-stream)
    (if in
        (setq in-stream (task-output in))
      (setq in-stream (open "/dev/null" :element-type :default)))
    
    ;; thread.
    (when (functionp command)
      (return-from yarn1 (make-thread-task command :input in-stream)))
    
    ;; process.
    (logd "aaa")
    (unless (listp command) (setq command (list command)))
    (logd "bbb")
    (let* ((proc-info (uiop:launch-program command
                                           :input in-stream
                                           :output :stream
                                           :directory *default-pathname-defaults*)))
      (logd "proc-info:~a" proc-info)
      proc-info)))

(defun yarn (first-command &rest commands
            &aux
              last-task
              )
  (labels ((rec (commands)
             (setq last-task (yarn1 last-task (car commands)))
             (when (cdr commands)
               (rec (cdr commands)))))
    (rec (cons first-command commands)))
  last-task)

(defun slurp (task &optional (processor :lines))
  "Processor is the same as the uiop:slurp-sinput-stream's 0th argument.
:string, t, (the stream x), (the pathname x) and so on."
  (uiop:slurp-input-stream processor (task-output task)))

(defun logd (&rest args)
  (apply #'format t args)
  (write-char #\Newline)
  (finish-output *standard-output*))

(defun cwd ()
  (values *default-pathname-defaults* (uiop:getcwd)))

(defsetf cwd () (dir)
  `(let ((,dir (truename ,dir)))
     (setq *default-pathname-defaults* ,dir)
     (uiop:chdir ,dir)
     ,dir))
(defun cd (dir)
  "Syntax sugar for (setf (cwd) dir)"
  (setf (cwd) dir))

(defun walk (dir fn) (error "wip"))



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


