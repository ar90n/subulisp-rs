(
    (def tarai (x y z)
        (if
            (<= (x) (y))
            (y)
            (tarai
                (tarai
                    (decf (x))
                    (y)
                    (z)
                )
                (tarai
                    (decf (y))
                    (z)
                    (x)
                )
                (tarai
                    (decf (z))
                    (x)
                    (y)
                )
            )
        )
   )
    (tarai 5 2 0)
)