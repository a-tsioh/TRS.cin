TRS.cin
=======

cin table for Taiwan Romanisation System (台語羅馬字拼音方案ê gcin/jscin 輸入法 table)

Require
=======

Ocaml with YoJSON, Ulex, Uunf and soon Pcre

COMPILATION
===========

ocamlfind ocamlopt -linkpkg -package ulex,yojson,pcre,uunf,str romanisation.ml moeReader.ml -o reader

GENERATE
========

cat headers.cin > result.cin
./reader ../moedict-data-twblg/dict-twblg.json | sort -u >> result.cin

TODO
====
Still have to 
* Use Pcre and not Str for safe Unicode
* set proper headers for .cin file
* decide how to join multi-character words
* decide what "fuzzification" to apply
* addcredits to g0v and MOE
