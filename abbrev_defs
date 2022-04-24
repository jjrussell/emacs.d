;;-*-coding: utf-8;-*-
(define-abbrev-table 'global-abbrev-table
  '(
    ("hte" "the" nil :count 4)
    ("rdb" "require 'debugger' ; debugger" nil :count 2)
    ("teh" "the" nil :count 5)
   ))

(define-abbrev-table 'jdee-mode-abbrev-table
  '(
    ("catch" "catch" c-electric-continued-statement :count 0)
    ("else" "else" c-electric-continued-statement :count 0)
    ("finally" "finally" c-electric-continued-statement :count 0)
    ("while" "while" c-electric-continued-statement :count 0)
   ))

(define-abbrev-table 'ruby-mode-abbrev-table
  '(
    ("rdb" "begin require 'pry-debugger' ; rescue LoadError ; require 'pry-byebug';  end ; binding.pry" nil :count 41)
    ("rdbr" "require 'pry-remote' ; require 'pry-debugger' ; binding.remote_pry" nil :count 12)
   ))

