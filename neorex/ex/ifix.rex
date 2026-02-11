x.y                      ; basic
x"y".a"b"                ; basic+juxt
foo.x=bar.x              ; precidence demo
(a, b.c, 'd, "e")        ; tuple with various
[k1: v1, 'k2(lol): v2]   ; record with quipped keys
{a:x+x,b:y,c:z+z}        ; tight record with math.
(a => b || c < d)        ; precidence demo
(a b , + 3 , f 4)        ; rune+rune
(a b , 4 , +)            ; rune+rune
(a b , 4 +)              ; trailing rune
foo.x + 3 * 7            ; top-level infix block

; rune+rune in record
{ name: items
, value: ~ 1
         ~ 2
         ~ 3 }

; rune+rune in tuple
( + 3 4
, add 3 4
, ? x (add x x)
)

; infix forms support trailing runes.
(3,)
(3,4,)
(
 3,
 4,
)
(3+4,)
(3+4,)
