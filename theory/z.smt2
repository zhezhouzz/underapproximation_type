(assert
(not (exists ((_x0 Int) (_q_y1 Int) (_q_x1 Int))
               (let ((a!1 (or (and (= -1 0))
                              (and false (not (<= -1 -2)) (= -1 _q_y1)))))
               (let ((a!2 (and (not (<= -1 -2))
                               (= -1 _q_y1)
                               (= _x0 0)
                               (= _x0 _q_x1)
                               false)))
                 (or (<= -1 -2) a!2)))))
                 )

(check-sat)
(get-model)
