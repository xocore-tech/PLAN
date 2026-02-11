= thunk
{ f:  ? (f x)
      ^ [_, _, _]
      | add @ fx (f x)
            | mul 3 fx
      | add x x

, x:  | add 3
      | add 3 4
}

= thunk
( map
, & x
  | foo | add x x
  x
, ~[3 4 5]
,
)
