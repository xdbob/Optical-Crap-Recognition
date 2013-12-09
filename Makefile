######### do not erase #############
OCAMLMAKEFILE = src/OCamlMakefile
####################################

# Donner la liste des librairies utilisées, et dé-commmenter si non vide
LIBS = bigarray sdl sdlloader lablgtk lablgl lablgtkspell
INCDIRS= +sdl +site-lib/sdl +lablgtk2 +lablgl

# Donenr les fichers souces de votre programme
SOURCES = src/math.ml src/perceptron.ml src/matrix.ml src/img.ml src/segmentation.ml src/interface.ml 
RESULT = ./ObjectiveCrapRecognition

all: native-code

# Dé-commenter pour une application utilisant les threads
# THREADS=yes

######### do not erase #############
-include $(OCAMLMAKEFILE)
####################################
