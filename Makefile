all: lambda.byte

OBJECTS = syntax.cmo lexer.cmo parser.cmo prettyprint.cmo eval.cmo compile.cmo
LLVM_MODULES = llvm,llvm.analysis,llvm.executionengine,llvm.target,llvm.scalar_opts

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
	ocamlfind ocamlc -package $(LLVM_MODULES) -linkpkg $(OBJECTS) lambda.ml -o lambda.byte

%.ll: %.lam lambda.byte
	./lambda.byte $<

%.bc: %.ll
	llvm-as-3.7 $<

%.s: %.bc
	llc-3.7 $<

runtime.o: runtime.c
	clang-3.7 -c runtime.c

%.o: %.s
	clang-3.7 -c $<

%.exe: %.o runtime.o
	clang-3.7 -o $@ $^

clean:
	-rm lexer.ml parser.ml parser.mli *.cmo *.cmi
	-rm *.conflicts
	-rm *.byte
	-rm *.ll *.bc *.s *.o *.exe
