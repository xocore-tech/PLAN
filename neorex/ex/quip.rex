(
'Quip
'Quip'Quip
'Quip;Semi
'Quip"Trad String"
)

'Quip'Quip' Slug
          ' Slug

'Quip' Slug

'Quip''
     Ugly
     ''

(') ; empty quip

('x, 'y)      ; uglies do not include trailing runes
('+ , '+++++) ; but you can use uglies to quote runes

[
  '(a) '[b] '{c}  ; various types of nesting
  '(]) '[}] '{)}  ; Can use mismatched ends
  'm(x + y / z)   ; math expression
  '{x:a, y:[b]}   ; records
  '(a (b) c)      ; nested parens
]

'(        ; ) Nested quips can contain comments.
  ")"     ; ) Nested quips can contain strings.
  'asdf   ; ) Nested quips can contain slugs.
  ''
  ugly
  ''      ; ) Nested quips can contain uglies.
  'm(a+b) ; ) Nested quips can contain quips.
)lol()    ; ) Big quips can still have more stuff at the end.

'QED

; Indented quip + HTML
    'html[]{
      'body[bgcolor='#eeeeee]{
        'span[class=wd]{
          asdf
        }
      }
    }
