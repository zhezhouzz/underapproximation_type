(set-option :smt.mbqi true)
(assert
(not
  (exists ((b Int))
  (forall ((a Int))
    (implies (> a 0)
    (exists ((x Int))
      (forall ((v Int))
        (implies (= v x) (= v a))
      )
    )
    )
  )
  )
)
)
(apply
  (and-then
    (using-params snf :mode skolem)
  )
  :print_benchmark true
  :print false
)