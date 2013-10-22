######### do not erase #############
OCAMLMAKEFILE = src/OCamlMakefile
####################################

# Donner la liste des librairies utilis�es, et d�-commmenter si non vide
LIBS = bigarray sdl sdlloader
INCDIRS= +sdl +site-lib/sdl

# Donenr les fichers souces de votre programme
SOURCES = src/matrix.ml src/neuron.ml src/img.ml src/chars.ml src/main.ml 
RESULT = ./ObjectiveCrapRecognition

all: native-code

# D�-commenter pour une application utilisant les threads
# THREADS=yes

######### do not erase #############
-include $(OCAMLMAKEFILE)
####################################
