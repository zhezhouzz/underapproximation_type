(assert
(exists ((v Int))
             (forall ((x Int)) (and (= v x) (not (= v (+ 1 x)))))
  )
  )

(check-sat)
(get-model)
