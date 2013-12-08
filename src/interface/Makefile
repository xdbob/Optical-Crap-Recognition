######### do not erase #############
OCAML=ocamlc
OCAMLFLAGS= -I +lablgtk2
OCAMLLD= labgtk.cma labgtk2.cma

all: 
	make clean; ./sdz

interface: sdz.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o sdz sdz.ml

clean::
	rm -f *~ *.o *.cm? *mli *cmi sdz
