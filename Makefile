SYSTEM_NAME = swaywm-repl
TARGET = $(SYSTEM_NAME)
ENTRYPOINT = $(SYSTEM_NAME):main

all: $(TARGET)

$(TARGET):
	sbcl --no-userinit --no-sysinit --non-interactive \
	     --eval '(load (sb-ext:posix-getenv "ASDF"))' \
	     --load ./swaywm-repl.asd \
	     --eval '(asdf:make :swaywm-repl)' \
	     --eval '(quit)'

clean:
	-rm -f $(TARGET)
