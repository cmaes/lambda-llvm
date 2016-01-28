all: lambda.byte

OBJECTS = syntax.cmo lexer.cmo parser.cmo prettyprint.cmo eval.cmo compile.cmo

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	menhir --explain parser.mly

syntax.cmo: syntax.ml
	ocamlc -c syntax.ml

parser.cmo: parser.mli parser.ml syntax.cmo
	ocamlc -c parser.mli
	ocamlc -c parser.ml

lexer.cmo: lexer.ml parser.cmo
	ocamlc -c lexer.ml

prettyprint.cmo: prettyprint.ml syntax.ml
	ocamlc -c prettyprint.ml

eval.cmo: eval.ml syntax.ml
	ocamlc -c eval.ml

compile.cmo: compile.ml syntax.ml
	ocamlfind ocamlc -c -package llvm compile.ml

lambda.byte: lambda.ml $(OBJECTS)
	ocamlfind ocamlc -package llvm,llvm.analysis -linkpkg $(OBJECTS) lambda.ml -o lambda.byte

clean:
	-rm lexer.ml parser.ml parser.mli *.cmo *.cmi
	-rm *.conflicts
	-rm *.byte
