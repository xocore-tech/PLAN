x+word                               ;;;; Basic trailing cases.
x+"text"
x+'quip
x+''
  ugly
  ''
x+' slug
x+(a,b)
x+(a.b)
x+(ab)
x+(a b)
x+(a . b)
x+(. a b)

word+x
"text"+x
('quip)+x
'quip+'quip
'quip+''
      ugly
      ''
'quip+' slug
(a,b)+c
(a.b)+c
(ab)+c
(a b)+c
(a . b)+c
(. a b)+c

;;;; Compound-leading cases (TODO)

;;;; Compound-trailing cases (TODO)
