######### do not erase #############
OCAMLMAKEFILE = OCamlMakefile
####################################

# Donner la liste des librairies utilis�es, et d�-commmenter si non vide
LIBS = bigarray sdl sdlloader
INCDIRS= +sdl

# Donenr les fichers souces de votre programme
SOURCES = matrix.ml img.ml main.ml 
RESULT = ObjectiveCrapRecognition

all: native-code

# D�-commenter pour une application utilisant les threads
# THREADS=yes

######### do not erase #############
-include $(OCAMLMAKEFILE)
####################################
