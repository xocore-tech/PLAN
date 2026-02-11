    (good x y)

    (bad x y)

    (= nest [a , b , c])

    foo([x])

    ()

    foo(() x)

    x"trad
      """x

    ( BAD:"'''
           ugly
            '''"
      'abc[x]
    )

    (= nest
       [ ( This
           should
           probably
           be
           rejected
           by
           a
           linter
         )
       , ( since
           it's
           indented
           less
           than
           the
           opening-nest
         )
       , ((but the parser accepts it) .)
       ]
    )

    'html{
        'body[bgcolor=black fgcolor=white]{
            Body text for HTML example.
        }
    }

    [+ 3 4]

    [3 + 4]

    [3+4]

    [3 4]

    [3]

    []

    [(+ 3 4) (+ 3 4)]


