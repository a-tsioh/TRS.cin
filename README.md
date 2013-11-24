TRS.cin
=======

cin/ibus tables for Taiwan Romanisation System (台語羅馬字拼音方案ê gcin/jscin/ibus-table 輸入法 table)

Requirements
------------

Ocaml with YoJSON, Ulex, Uunf and Pcre

Compilation and table generation
---------------------------------

* set the path to dict-twblg.json in the Makefile
* `make result.cin` for jsCin 
* `make ibus/taigi-trs.db` for ibus-table


TODO
----
Still have to...
* add some options for fuzzyfication and separators (tricky)
* add tonalized TRS as output (trivial)
* add bopomo as input (not hard)
* add some doc for linux and mac and/or package into a .deb or something (boring)
* add credits to g0v and MOE (must)
