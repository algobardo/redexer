LIB=fsm
EXEC=fsa
#EXECOPT=fsax

XTYPE=xtype
XTYPE_DIR=../xtype/
XTYPE_LIB=$(XTYPE).cma
#XTYPE_LIBA=$(XTYPE).a
#XTYPE_LIBCMX=$(XTYPE).cmxa
#XTYPE_SOURCES=$(wildcard $(XTYPE_DIR)/*.ml) $(wildcard $(XTYPE_DIR)/*.mli)

XTYPE_ML_SOURCES=delim.ml print.ml mstring.ml x.ml file.ml abstract.ml \
table.ml mbool.ml mlist.ml mset.ml mmap.ml xlist.ml xset.ml xmap.ml

XTYPE_SOURCES=$(patsubst %,$(XTYPE_DIR)%,$(XTYPE_ML_SOURCES))
XTYPE_CMX=$(patsubst %.ml,%.cmx,$(XTYPE_SOURCES))

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLOPTFLAGS=-inline 32#-unsafe -ccopt "-O3 -march=i686"

INCLUDE=-I $(XTYPE_DIR)
OCAMLFLAGS=-g $(INCLUDE)

ML_SOURCES=symbols.ml vocab.ml node.ml edges.ml machines.ml acceptors.ml
MLI_SOURCES=$(patsubst %.ml,%.mli,$(ML_SOURCES)) $(EXEC).mli
CMOFILES=$(patsubst %.ml,%.cmo,$(ML_SOURCES)) $(EXEC).cmo
CMIFILES=$(patsubst %.ml,%.cmi,$(ML_SOURCES)) $(EXEC).cmi
CMXFILES=$(patsubst %.ml,%.cmx,$(ML_SOURCES)) $(EXEC).cmx


all:	$(XTYPE_LIB) $(CMOFILES) $(CMIFILES) 
	$(OCAMLC) -a $(OCAMLFLAGS) $(XTYPE_LIB) $(ML_SOURCES) -o $(LIB).cma
	$(OCAMLC) $(OCAMLFLAGS) $(XTYPE_LIB) $(CMOFILES) -o $(EXEC)

$(XTYPE_LIB): $(XTYPE_SOURCES)
	cd $(XTYPE_DIR) && $(MAKE)

opt:	$(XTYPE_LIBCMX) $(CMXFILES) $(CMIFILES)
#	$(OCAMLOPT) -a $(OCAMLOPTFLAGS) $(INCLUDE) $(XTYPE_LIBA) $(CMXFILES) -o $(LIB).cmxa
#	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $(EXECOPT) $(INCLUDE) $(XTYPE_LIBCMX) $(CMXFILES) 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $(EXEC) unix.cmxa str.cmxa $(XTYPE_CMX) $(CMXFILES)

$(XTYPE_LIBCMX): $(XTYPE_SOURCES)
	cd $(XTYPE_DIR) && $(MAKE) opt

.SUFFIXES: .ml .mli .cmo .cmi .cmx

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(XTYPE_LIB) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(XTYPE_LIB) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(INCLUDE) $(OCAMLOPTFLAGS) -c $<


.PHONY: clean doc

clean:
	rm -f *.cm*
	rm -f *.o
	rm -f *.obj
	rm -f *.d
	rm -f *.a
	rm -f *~
	rm -f $(EXEC)

# add $(XTYPE_SOURCES) to the ocamldoc lines?

INCLUDE_DOC = -I $(XTYPE_DIR)/doc

doc: 	all
	ocamldoc $(INCLUDE) $(INCLUDE_DOC) -html -d doc $(ML_SOURCES) $(MLI_SOURCES) 
	ocamldoc $(INCLUDE) $(INCLUDE_DOC) -dot -o doc/fsa.dot $(ML_SOURCES) $(MLI_SOURCES) 
	ocamldoc $(INCLUDE) $(INCLUDE_DOC) -texi -o doc/fsa.texinfo $(ML_SOURCES) $(MLI_SOURCES) 
