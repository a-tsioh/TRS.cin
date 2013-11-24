MOE_JSON="../moedict-data-twblg/dict-twblg.json"

reader: moeReader.ml romanisation.ml
	ocamlfind ocamlopt -linkpkg -package ulex,yojson,pcre,uunf romanisation.ml moeReader.ml -o reader

result.cin: reader head.cin
	cat head.cin > result.cin;
	./reader $(MOE_JSON) >> result.cin;
	echo "%chardef end" >> result.cin
	
