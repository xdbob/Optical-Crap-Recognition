######### do not erase #############
OCAMLMAKEFILE = OCamlMakefile
####################################

# Donner la liste des librairies utilisées, et dé-commmenter si non vide
LIBS = bigarray sdl sdlloader
INCDIRS= +sdl

# Donenr les fichers souces de votre programme
SOURCES = matrix.ml img.ml main.ml 
RESULT = ObjectiveCrapRecognition

all: native-code

# Dé-commenter pour une application utilisant les threads
# THREADS=yes

######### do not erase #############
-include $(OCAMLMAKEFILE)
####################################
