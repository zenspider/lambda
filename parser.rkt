#lang ragg

prog: expr*
expr: name | func | appl
name: NAME
func: LAMBDA name DOT body
appl: OPEN lexp aexp CLOSE
body: expr
lexp: expr
aexp: expr
