(defpackage swaywm-repl
  (:use :cl)
  (:export :main))
(in-package :swaywm-repl)

(defparameter *swaysock-address*
  (sb-ext:posix-getenv "SWAYSOCK"))

(defconstant +run-command+ 0)
(defconstant +get-workspaces+ 1)
(defconstant +subscribe+ 2)
(defconstant +get-outputs+ 3)
(defconstant +get-tree+ 4)
(defconstant +get-marks+ 5)
(defconstant +get-bar-config+ 6)
(defconstant +get-version+ 7)
(defconstant +get-binding-modes+ 8)
(defconstant +get-config+ 9)
(defconstant +send-tick+ 10)
(defconstant +sync+ 11)
(defconstant +get-binding-state+ 12)
(defconstant +get-inputs+ 13)
(defconstant +get-seats+ 14)

(defun within-type-range (number)
  (<= number +get-seats+))

(deftype payload-type ()
  `(and (unsigned-byte 32)
        (satisfies within-type-range)))

(defclass sway-message-payload ()
  ((payload-length
    :type (unsigned-byte 32)
    :initarg :payload-length
    :accessor payload-length)
   (payload-type
    :type (payload-type)
    :initarg :payload-type
    :accessor payload-type)
   (payload-data
    :initarg :payload-data
    :accessor payload-data)))

(defclass sway-message ()
  ((magic-string
    :initform "i3-ipc"
    :accessor magic-string
    :allocation :class)
   (payload
    :type (sway-message-payload)
    :initarg :payload
    :accessor payload)))

(defgeneric make-message (object)
  (:documentation
   "Returns a string readable by swaymsg."))

(defmethod make-message ((object sway-message))
  (with-accessors ((payload-length payload-length)
                   (payload-type payload-type)
                   (payload payload-data))
      (payload object)
    (concatenate 'string
		 (magic-string object)
		 (map 'string #'code-char
		      (append
		       (number->bytes payload-length)
		       (number->bytes payload-type)))
		 payload)))

(declaim (ftype
          (function (unsigned-byte)
		    (values list &optional))
          number->bytes))
(defun number->bytes (number)
  (loop for i from 0 to 3
        collect (logand #xFF
                        (ash number (* 8 (* i -1))))))

(declaim (ftype
	  (function (sb-bsd-sockets:socket string))
	  send-message))
(defun send-message (socket message-buffer)
  (sb-bsd-sockets:socket-send socket message-buffer
			      (length message-buffer)))

(declaim (ftype
	  (function (sb-bsd-sockets:socket) (values string &optional))
	  receive-reply))
(defun receive-reply (socket)
  (sb-bsd-sockets:socket-receive socket nil (* 2 8192)))

(declaim (ftype
          (function (string) (values sway-message &optional))
          parse-command))
(defun parse-command (command)
  (let* ((command-segments (uiop:split-string command))
	 (payload-type (choose-payload-type (car command-segments)))
	 (payload-data (cadr command-segments))
	 (payload-length (length payload-data))
	 (payload (make-instance 'sway-message-payload
				 :payload-data payload-data
				 :payload-type payload-type
				 :payload-length payload-length)))
    (make-instance 'sway-message :payload payload)))

(defun choose-payload-type (type)
  (cond
    ((string= "run_command" type) +run-command+) ; throw an error until this works
    ((string= "get_workspaces" type) +get-workspaces+)
    ((string= "subscribe" type) +subscribe+)
    ((string= "get_outputs" type) +get-outputs+)
    ((string= "get_tree" type) +get-tree+)
    ((string= "get_marks" type) +get-marks+)
    ((string= "get_bar_config" type) +get-bar-config+)
    ((string= "get_version" type) +get-version+)
    ((string= "get_binding_modes" type) +get-binding-modes+)
    ((string= "get_config" type) +get-config+)
    ((string= "send_tick" type) +send-tick+)
    ((string= "sync" type) +sync+)
    ((string= "get_binding_state" type) +get-binding-state+)
    (t (error type))))

(declaim (ftype
          (function () (values sb-bsd-sockets:socket &optional))
          connect-to-sway))
(defun connect-to-sway ()
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket
			       :type :stream)))
    (sb-bsd-sockets:socket-connect socket *swaysock-address*)))

(declaim (ftype (function (sb-bsd-sockets:socket)) eval-commands))
(defun eval-commands (socket)
  (flet ((prompt ()
           (progn
             (format t "~&swaywm-repl> ")
             (finish-output)
             (format nil "~(~a~)" (read nil 'eof nil)))))
    (loop for command = (funcall #'prompt)
          if (string/= command "exit")
            do (let ((message (make-message (parse-command command))))
                 (send-message socket message)
		 (receive-reply socket))
          else
            do (loop-finish))))

(defun main ()
  (let ((socket (connect-to-sway)))
    (eval-commands socket)
    (sb-ext:finalize socket
                     (lambda ()
                       (sb-bsd-sockets:socket-close socket)))
    socket))
