MODULES=matrix polynomial toImage main newton lineDrawer mandelbrot fromImage parse gui
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
NEWTON=newtonDrawer.byte
MAINTERM=mainTerminal.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
BW=black_and_white.byte


default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)
	
newton:
	$(OCAMLBUILD) $(NEWTON) && ./$(NEWTON)


terminalFractal:
	$(OCAMLBUILD) $(MAINTERM) && ./$(MAINTERM)


zip:
	zip ms1.zip *.ml* _tags Makefile *.txt

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

image:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

bw:
	$(OCAMLBUILD) $(BW) && ./$(BW) 1000 1000 a.bmp

clean:
	ocamlbuild -clean
	rm -rf ./*.bmp
	rm -rf doc.public doc.private *.bmp ms1.zip 

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package \
	camlimages.core,ANSITerminal,graphics,camlimages.graphics \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package \
	camlimages.core,ANSITerminal,graphics,camlimages.graphics \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

fractal: 
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)