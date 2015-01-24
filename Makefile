MOE_JSON="./dict-twblg.json"

reader: moeReader.ml romanisation.ml
	ocamlfind ocamlopt -linkpkg -package ulex,yojson,pcre,uunf romanisation.ml moeReader.ml -o reader

result.cin: reader head.cin
	cat head.cin > result.cin;
	./reader $(MOE_JSON) >> result.cin;
	echo "%chardef end" >> result.cin

#ibus/taigi-trs.txt: reader
#	cat ibus/taigi-trs-header.txt > ibus/taigi-trs.txt
#	./reader $(MOE_JSON) | sort -u | sed -r "s/ /\t/g" | sed 's/$$/\t1/'  >> ibus/taigi-trs.txt
#	echo "END TABLE" >> ibus/taigi-trs.txt

ibus/taigi-trs.txt: create_ibus_table.py 
	cat ibus/taigi-trs-header.txt > ibus/taigi-trs.txt
	python create_ibus_table.py >> ibus/taigi-trs.txt
	echo "END TABLE" >> ibus/taigi-trs.txt

ibus/taigi-trs.db: ibus/taigi-trs.txt
	ibus-table-createdb -n ibus/taigi-trs.db -s ibus/taigi-trs.txt
	
jszhuyin-data.txt: reader
	./reader $(MOE_JSON) | sort -u | perl -CSDA -pe 'use utf8; s/^([^ ]+) ([^ \n]+)$$/\2 \1 -1.0/' > jszhuyin-data.txt
