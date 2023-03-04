(declare-fun mem (Int Int) Bool)
(declare-fun hd (Int Int) Bool)
(declare-fun empty (Int) Bool)
(declare-const v Int)
(declare-const u!6 Int)
(declare-const u!7 Int)


(assert (forall ((l Int))
  (forall ((u Int)) (forall ((w Int)) (=> (and (hd l u) (hd l w)) (= u w))))))

(assert (forall ((l Int)) (forall ((u Int)) (=> (hd l u) (mem l u)))))

(assert (forall ((l Int)) (forall ((u Int)) (=> (empty l) (not (mem l u))))))

(assert

(forall ((u!2 Int))
             (forall ((u!3 Int))
               (forall ((u Int))
                 (forall ((w Int))
                   (forall ((|mem!0,l!0,u!2| Bool))
                     (forall ((|mem!1,l!0,u!3| Bool))
                       (forall ((|mem!2,l!0,u!2| Bool))
                         (forall ((|mem!3,l!0,u!3| Bool))
                           (forall ((|hd!0,l!0,u!2| Bool))
                             (forall ((|hd!1,l!0,u!3| Bool))
                               (forall ((|hd!2,l!0,u!2| Bool))
                                 (forall ((|hd!3,l!0,u!3| Bool))
                                   (forall ((|empty!0,l!0| Bool))
                                     (forall ((|empty!1,l!0| Bool))
                                       (let ((a!1 (and (=> (empty v)
                                                           (not (mem v u)))
                                                       (=> (hd v u) (mem v u))
                                                       (=> (not (empty v))
                                                           (mem v u!7))
                                                       (=> (not (empty v))
                                                           (hd v u!6))
                                                       (=> (and (hd v u)
                                                                (hd v w))
                                                           (= u w))))
                                             (a!2 (and (=> (= u!3 u!2)
                                                           (=> (mem v u!3)
                                                               (mem v u!2)))
                                                       (=> (= u!3 u!2)
                                                           (=> |mem!1,l!0,u!3|
                                                               |mem!0,l!0,u!2|))
                                                       (=> (= u!3 u!2)
                                                           (=> (hd v u!3)
                                                               (hd v u!2)))
                                                       (=> (= u!3 u!2)
                                                           (=> |hd!1,l!0,u!3|
                                                               |hd!0,l!0,u!2|))
                                                       (=> (not |empty!0,l!0|)
                                                           |hd!0,l!0,u!2|)
                                                       (=> (not |empty!0,l!0|)
                                                           |mem!1,l!0,u!3|)
                                                       (=> (and |hd!1,l!0,u!3|
                                                                |hd!1,l!0,u!3|)
                                                           (= u!3 u!3))
                                                       (=> (and |hd!1,l!0,u!3|
                                                                |hd!0,l!0,u!2|)
                                                           (= u!3 u!2))
                                                       (=> (and |hd!0,l!0,u!2|
                                                                |hd!1,l!0,u!3|)
                                                           (= u!2 u!3))
                                                       (=> (and |hd!0,l!0,u!2|
                                                                |hd!0,l!0,u!2|)
                                                           (= u!2 u!2))
                                                       (=> |hd!1,l!0,u!3|
                                                           |mem!1,l!0,u!3|)
                                                       (=> |hd!0,l!0,u!2|
                                                           |mem!0,l!0,u!2|)
                                                       (=> |empty!0,l!0|
                                                           (not |mem!1,l!0,u!3|))
                                                       (=> |empty!0,l!0|
                                                           (not |mem!0,l!0,u!2|))
                                                       (=> |mem!3,l!0,u!3|
                                                           |hd!3,l!0,u!3|)
                                                       (not |empty!1,l!0|)
                                                       (=> |mem!2,l!0,u!2|
                                                           |hd!2,l!0,u!2|)
                                                       (not |empty!1,l!0|)
                                                       (= |mem!3,l!0,u!3|
                                                          (mem v u!3))
                                                       (= |mem!2,l!0,u!2|
                                                          (mem v u!2)))))
                                         (and a!1 (not a!2)))))))))))))))))
)

(check-sat)
(get-model)