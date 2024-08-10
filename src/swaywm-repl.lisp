(defpackage swaywm-repl
  (:use :cl)
  (:export :main))
(in-package :swaywm-repl)

(deftype payload-type ()
  '(member :run-command :get-workspaces :subscribe :get-outputs :get-tree
    :get-marks :get-bar-config :get-version :get-binding-modes :get-config
    :send-tick :sync :get-binding-state :get-inputs :get-seats))

(defclass sway-message-data ()
  ((payload-length
    :type (unsigned-byte 32)
    :initarg :payload-length
    :accessor payload-length)
   (payload-type
    :type (payload-type)
    :initarg :payload-type
    :accessor payload-type)
   (payload
    :initarg :payload
    :accessor payload)))

(defclass sway-message ()
  ((magic-string
    :initform "i3-ipc"
    :accessor magic-string
    :allocation :class)
   (data
    :type (sway-message-data)
    :initarg :data
    :accessor data)))

(defclass sway-message-writer ()
  ((socket
    :type (sb-bsd-sockets:socket)
    :initarg :socket
    :accessor socket)))

(defun send-sway-message (writer cmd) cmd)

(declaim (ftype (function (string) (values sway-message &optional)) parse-command))
(defun parse-command (cmd)
  (make-instance 'sway-message))

(declaim (ftype (function () (values sway-message-writer &optional)) connect-to-sway))
(defun connect-to-sway ()
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
	(socket-address (sb-ext:posix-getenv "SWAYSOCK")))
    (sb-bsd-sockets:socket-connect socket socket-address)
    (make-instance 'sway-message-writer :socket socket)))

(declaim (ftype (function (sway-message-writer)) eval-commands))
(defun eval-commands (writer)
  (flet ((prompt ()
	   (progn
	     (format t "~&swaywm-repl> ")
	     (finish-output)
	     (format nil "~(~a~)" (read nil 'eof nil)))))
    (loop for cmd = (funcall #'prompt)
	  if (string/= cmd "exit")
	    do (let ((msg (parse-command cmd)))
		 (send-sway-message writer msg))
	  else
	    do (loop-finish))))

(defun main ()
  (let ((writer (connect-to-sway)))
    (eval-commands writer)))
