(asdf:defsystem "swaywm-repl"
  :version "0.1.0"
  :author "Ross Edwards"
  :description "REPL for interacting with a SwayWM environment."
  :components ((:module "src"
		:components
		((:file "swaywm-repl"))))
  :build-operation "program-op"
  :build-pathname "swaywm-repl"
  :entry-point "swaywm-repl::main"
  :in-order-to ((asdf:test-op (asdf:test-op :swaywm-repl/test))))

(asdf:defsystem "swaywm-repl/test"
  :depends-on (:swaywm-repl :fiveam)
  :components ((:module "tests"
		:components
		((:file "swaywm-repl"))))
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :fiveam :run!
					   (uiop:find-symbol* :swaywm-repl :swaywm-repl/test))))

