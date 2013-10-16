
SRCDIR = src
EXECDIR = .
EXEC = $(EXECDIR)/ObjectiveCrapRecognition
MAKESRC = $(MAKE) -C $(SRCDIR) 

all: $(EXEC)

$(EXEC):
	@$(MAKESRC)

clean:
	@$(MAKESRC) clean
