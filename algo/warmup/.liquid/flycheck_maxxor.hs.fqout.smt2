(set-option :auto-config false)
(set-option :model true)
(set-option :model.partial false)
(set-option :smt.mbqi false)
(define-sort SMT_Elt () Int)
(define-sort SMT_Set () (Array SMT_Elt Bool))
(define-fun smt_set_emp () SMT_Set ((as const SMT_Set) false))
(define-fun smt_set_mem ((x SMT_Elt) (s SMT_Set)) Bool (select s x))
(define-fun smt_set_add ((s SMT_Set) (x SMT_Elt)) SMT_Set (store s x true))
(define-fun smt_set_cup ((s1 SMT_Set) (s2 SMT_Set)) SMT_Set ((_ map or) s1 s2))
(define-fun smt_set_cap ((s1 SMT_Set) (s2 SMT_Set)) SMT_Set ((_ map and) s1 s2))
(define-fun smt_set_com ((s SMT_Set)) SMT_Set ((_ map not) s))
(define-fun smt_set_dif ((s1 SMT_Set) (s2 SMT_Set)) SMT_Set (smt_set_cap s1 (smt_set_com s2)))
(define-fun smt_set_sub ((s1 SMT_Set) (s2 SMT_Set)) Bool (= smt_set_emp (smt_set_dif s1 s2)))
(declare-fun z3v60 () Int)
(declare-fun z3f61 (Int) Int)
(declare-fun z3v62 () Int)
(declare-fun z3v63 () Int)
(declare-fun z3f64 (Int) Int)
(declare-fun z3v65 () Int)
(declare-fun z3v66 () Int)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v60) 0) (>= (z3f61 z3v62) 0) (= z3v62 z3v60) (>= (z3f61 z3v62) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(declare-fun z3v67 () Int)
(declare-fun z3v68 () Int)
(declare-fun z3v69 () Int)
(declare-fun z3v70 () Int)
(push 1)
(assert (not (= z3v68 1)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v68 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v68 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v68 0))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v68 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v68 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v68 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v68 z3v70))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v68 z3v69))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v68 z3v67))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v68 z3v67)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v68 z3v67)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v68 z3v67)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v68 z3v67)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v68 z3v67)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v68 z3v67)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v68 z3v67)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v68 z3v70))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v68 z3v69))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v68 z3v67))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v68 z3v67)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v68 z3v67)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v68 z3v70)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v68 z3v69)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v68 z3v67)))
(check-sat)
(pop 1)
(pop 1)
(declare-fun z3v71 () Int)
(push 1)
(assert true)
(assert (= z3v71 z3v60))
(assert (>= (z3f61 z3v71) 0))
(assert (and (>= (z3f61 z3v60) 0) (>= (z3f61 z3v62) 0) (= z3v62 z3v60) (>= (z3f61 z3v62) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(declare-fun z3v72 () Int)
(declare-fun z3v73 () Int)
(push 1)
(assert (not (>  (z3f61 z3v71) 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v71) 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v73 (z3f61 z3v71))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v72 (z3f61 z3v71))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v71) (- z3v73 1))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v71) (- z3v72 1))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v71) (+ z3v73 1))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v71) (+ z3v72 1))))
(check-sat)
(pop 1)
(pop 1)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v60) 0) (>= (z3f61 z3v62) 0) (= z3v62 z3v60) (>= (z3f61 z3v62) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(declare-fun z3v74 () Int)
(declare-fun z3v75 () Int)
(declare-fun z3v76 () Int)
(push 1)
(assert (not (= z3v75 1)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v75 (+ (z3f61 z3v62) z3v76))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v75 (+ (z3f61 z3v62) z3v74))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v75 (z3f61 z3v62))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v75 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v75 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v75 0))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v75 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v75 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v75 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v75 z3v76))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v75 z3v74))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v75 z3v74)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v75 z3v74)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v75 z3v74)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v75 z3v74)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v75 z3v74)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v75 z3v74)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v75 z3v74)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v75 z3v76))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v75 z3v74))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v75 z3v74)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v75 z3v74)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v75 z3v76)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v75 z3v74)))
(check-sat)
(pop 1)
(pop 1)
(declare-fun z3v77 () Int)
(declare-fun z3v78 () Int)
(declare-fun z3v79 () Int)
(declare-fun z3v80 () Int)
(declare-fun z3f81 (Int) Int)
(declare-fun z3v82 () Int)
(declare-fun z3f83 (Int) Int)
(declare-fun z3f84 (Int) Bool)
(declare-fun z3f85 (Int Int) Int)
(declare-fun z3v86 () Int)
(declare-fun z3v87 () Int)
(push 1)
(assert true)
(assert (= z3v87 z3v79))
(assert (>= (z3f61 z3v87) 0))
(assert (and (>= (z3f61 z3v79) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v86) 0) (= z3v86 z3v79) (>= (z3f61 z3v86) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v77) 0))
(declare-fun z3v88 () Int)
(declare-fun z3v89 () Int)
(push 1)
(assert (not (>  (z3f61 z3v87) 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v89 (z3f61 z3v87))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v88 (z3f61 z3v87))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v80 (z3f61 z3v87))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) (- z3v89 1))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) (- z3v88 1))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) (- z3v80 1))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) (+ z3v89 1))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) (+ z3v88 1))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) (+ z3v80 1))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  (z3f61 z3v87) (z3f61 z3v78))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  (z3f61 z3v87) (z3f61 z3v82))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  (z3f61 z3v87) (z3f61 z3v77))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) (z3f61 z3v78))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) (z3f61 z3v82))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= (z3f61 z3v87) (z3f61 z3v77))))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  (z3f61 z3v87) (z3f61 z3v78))))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  (z3f61 z3v87) (z3f61 z3v82))))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  (z3f61 z3v87) (z3f61 z3v77))))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= (z3f61 z3v87) (z3f61 z3v78))))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= (z3f61 z3v87) (z3f61 z3v82))))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= (z3f61 z3v87) (z3f61 z3v77))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= (z3f61 z3v87) (z3f61 z3v78))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= (z3f61 z3v87) (z3f61 z3v82))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= (z3f61 z3v87) (z3f61 z3v77))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v87 z3v78)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v87 z3v82)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v87 z3v77)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v87 z3v78)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v87 z3v82)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v87 z3v77)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v87 z3v78)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v87 z3v82)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v87 z3v77)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v87 z3v78)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v87 z3v82)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v87 z3v77)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v87 z3v78))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v87 z3v82))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v87 z3v77))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v87 z3v78)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v87 z3v82)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v87 z3v77)))
(check-sat)
(pop 1)
(pop 1)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v79) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v86) 0) (= z3v86 z3v79) (>= (z3f61 z3v86) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v77) 0))
(declare-fun z3v90 () Int)
(declare-fun z3v91 () Int)
(declare-fun z3v92 () Int)
(declare-fun z3v93 () Int)
(push 1)
(assert (not (= z3v90 1)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v77) z3v93))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v77) z3v92))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v77) z3v91))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v77) z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v82) z3v93))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v82) z3v92))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v82) z3v91))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v82) z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v78) z3v93))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v78) z3v92))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v78) z3v91))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (+ (z3f61 z3v78) z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (z3f61 z3v78))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (z3f61 z3v82))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 (z3f61 z3v77))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v90 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v90 0))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v90 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v90 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v90 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v90 z3v93))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v90 z3v92))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v90 z3v91))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v90 z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v90 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v90 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v90 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v90 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v90 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v90 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v90 z3v93))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v90 z3v92))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v90 z3v91))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v90 z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v90 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v90 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v90 z3v93)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v90 z3v92)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v90 z3v91)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v90 z3v80)))
(check-sat)
(pop 1)
(pop 1)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v79) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v86) 0) (= z3v86 z3v79) (>= (z3f61 z3v86) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v77) 0))
(declare-fun z3v94 () Int)
(declare-fun z3v95 () Int)
(declare-fun z3v96 () Int)
(push 1)
(assert (not (= z3v94 1)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v86) z3v96))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v86) z3v95))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v86) z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v77) z3v96))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v77) z3v95))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v77) z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v82) z3v96))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v82) z3v95))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v82) z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v78) z3v96))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v78) z3v95))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (+ (z3f61 z3v78) z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (z3f61 z3v78))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (z3f61 z3v82))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (z3f61 z3v77))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 (z3f61 z3v86))))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v94 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v94 0))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v94 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v94 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v94 0)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v94 z3v96))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v94 z3v95))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v94 z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v94 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v94 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (= z3v94 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v94 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<  z3v94 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (<= z3v94 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v94 z3v96))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v94 z3v95))))
(check-sat)
(pop 1)
(push 1)
(assert (not (not (= z3v94 z3v80))))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>  z3v94 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v94 z3v80)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v94 z3v96)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v94 z3v95)))
(check-sat)
(pop 1)
(push 1)
(assert (not (>= z3v94 z3v80)))
(check-sat)
(pop 1)
(pop 1)
(declare-fun z3v97 () Int)
(declare-fun z3v98 () Int)
(declare-fun z3f99 () Int)
(declare-fun z3v100 () Int)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v98) 0) (= (z3f61 z3v98) 0) (= (z3f84 z3v98) true) (= z3v98 z3f99) (>= (z3f61 z3v98) 0) (= z3v98 z3v97) (>= (z3f61 z3v98) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v97) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v100) 0) (= z3v100 z3v82) (>= (z3f61 z3v100) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v98) 0))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v97) 0))
(assert (>= (z3f61 z3v77) 0))
(pop 1)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v98) 0) (= (z3f61 z3v98) 0) (= (z3f84 z3v98) true) (= z3v98 z3f99) (>= (z3f61 z3v98) 0) (= z3v98 z3v97) (>= (z3f61 z3v98) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v97) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v100) 0) (= z3v100 z3v82) (>= (z3f61 z3v100) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v98) 0))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v97) 0))
(assert (>= (z3f61 z3v77) 0))
(pop 1)
(declare-fun z3v101 () Int)
(push 1)
(assert true)
(assert (= z3v101 z3v82))
(assert (>= (z3f61 z3v101) 0))
(assert (and (>= (z3f61 z3v98) 0) (= (z3f61 z3v98) 0) (= (z3f84 z3v98) true) (= z3v98 z3f99) (>= (z3f61 z3v98) 0) (= z3v98 z3v97) (>= (z3f61 z3v98) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v97) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v100) 0) (= z3v100 z3v82) (>= (z3f61 z3v100) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v98) 0))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v97) 0))
(assert (>= (z3f61 z3v77) 0))
(push 1)
(assert (not (and (>= (z3f61 z3v101) 0) (<  (z3f61 z3v101) (z3f61 z3v77)))))
(check-sat)
(pop 1)
(pop 1)
(declare-fun z3v102 () Int)
(push 1)
(assert true)
(assert (= z3v102 z3v82))
(assert (>= (z3f61 z3v102) 0))
(assert (and (>= (z3f61 z3v98) 0) (= (z3f61 z3v98) 0) (= (z3f84 z3v98) true) (= z3v98 z3f99) (>= (z3f61 z3v98) 0) (= z3v98 z3v97) (>= (z3f61 z3v98) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v97) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v100) 0) (= z3v100 z3v82) (>= (z3f61 z3v100) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v98) 0))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v97) 0))
(assert (>= (z3f61 z3v77) 0))
(push 1)
(assert (not (>= (z3f61 z3v102) 0)))
(check-sat)
(pop 1)
(pop 1)
(declare-fun z3v103 () Int)
(declare-fun z3v104 () Int)
(declare-fun z3v105 () Int)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v98) 0) (= (z3f81 z3v98) z3v103) (= (z3f83 z3v98) z3v104) (= (z3f61 z3v98) (+ 1 (z3f61 z3v104))) (= (z3f84 z3v98) false) (= z3v98 (z3f85 z3v103 z3v104)) (>= (z3f61 z3v98) 0) (= z3v98 z3v97) (>= (z3f61 z3v98) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v104) 0) (>= (z3f61 z3v97) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v105) 0) (= z3v105 z3v104) (>= (z3f61 z3v105) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v98) 0))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v97) 0))
(assert (>= (z3f61 z3v77) 0))
(pop 1)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v98) 0) (= (z3f81 z3v98) z3v103) (= (z3f83 z3v98) z3v104) (= (z3f61 z3v98) (+ 1 (z3f61 z3v104))) (= (z3f84 z3v98) false) (= z3v98 (z3f85 z3v103 z3v104)) (>= (z3f61 z3v98) 0) (= z3v98 z3v97) (>= (z3f61 z3v98) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v104) 0) (>= (z3f61 z3v97) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v105) 0) (= z3v105 z3v104) (>= (z3f61 z3v105) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v98) 0))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v97) 0))
(assert (>= (z3f61 z3v77) 0))
(pop 1)
(declare-fun z3v106 () Int)
(push 1)
(assert true)
(assert (= z3v106 z3v104))
(assert (>= (z3f61 z3v106) 0))
(assert (and (>= (z3f61 z3v98) 0) (= (z3f81 z3v98) z3v103) (= (z3f83 z3v98) z3v104) (= (z3f61 z3v98) (+ 1 (z3f61 z3v104))) (= (z3f84 z3v98) false) (= z3v98 (z3f85 z3v103 z3v104)) (>= (z3f61 z3v98) 0) (= z3v98 z3v97) (>= (z3f61 z3v98) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v104) 0) (>= (z3f61 z3v97) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v105) 0) (= z3v105 z3v104) (>= (z3f61 z3v105) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v98) 0))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v97) 0))
(assert (>= (z3f61 z3v77) 0))
(push 1)
(assert (not (and (>= (z3f61 z3v106) 0) (<  (z3f61 z3v106) (z3f61 z3v97)))))
(check-sat)
(pop 1)
(pop 1)
(declare-fun z3v107 () Int)
(push 1)
(assert true)
(assert (= z3v107 z3v104))
(assert (>= (z3f61 z3v107) 0))
(assert (and (>= (z3f61 z3v98) 0) (= (z3f81 z3v98) z3v103) (= (z3f83 z3v98) z3v104) (= (z3f61 z3v98) (+ 1 (z3f61 z3v104))) (= (z3f84 z3v98) false) (= z3v98 (z3f85 z3v103 z3v104)) (>= (z3f61 z3v98) 0) (= z3v98 z3v97) (>= (z3f61 z3v98) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v104) 0) (>= (z3f61 z3v97) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v105) 0) (= z3v105 z3v104) (>= (z3f61 z3v105) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v98) 0))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v97) 0))
(assert (>= (z3f61 z3v77) 0))
(push 1)
(assert (not (>= (z3f61 z3v107) 0)))
(check-sat)
(pop 1)
(pop 1)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v79) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v86) 0) (= z3v86 z3v79) (>= (z3f61 z3v86) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v77) 0))
(pop 1)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v79) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v86) 0) (= z3v86 z3v79) (>= (z3f61 z3v86) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v77) 0))
(pop 1)
(declare-fun z3v108 () Int)
(push 1)
(assert true)
(assert (= z3v108 z3v79))
(assert (>= (z3f61 z3v108) 0))
(assert (and (>= (z3f61 z3v79) 0) (>= (z3f61 z3v78) 0) (= (z3f81 z3v78) z3v80) (= (z3f83 z3v78) z3v82) (= (z3f61 z3v78) (+ 1 (z3f61 z3v82))) (= (z3f84 z3v78) false) (= z3v78 (z3f85 z3v80 z3v82)) (>= (z3f61 z3v78) 0) (= z3v78 z3v77) (>= (z3f61 z3v78) 0) (>= (z3f61 z3v82) 0) (>= (z3f61 z3v77) 0) (>= (z3f61 z3v86) 0) (= z3v86 z3v79) (>= (z3f61 z3v86) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(assert (>= (z3f61 z3v78) 0))
(assert (>= (z3f61 z3v77) 0))
(push 1)
(assert (not (>= (z3f61 z3v108) 0)))
(check-sat)
(pop 1)
(pop 1)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v60) 0) (>= (z3f61 z3v62) 0) (= z3v62 z3v60) (>= (z3f61 z3v62) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(pop 1)
(push 1)
(assert true)
(assert (and (>= (z3f61 z3v60) 0) (>= (z3f61 z3v62) 0) (= z3v62 z3v60) (>= (z3f61 z3v62) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(pop 1)
(declare-fun z3v109 () Int)
(push 1)
(assert true)
(assert (= z3v109 z3v60))
(assert (>= (z3f61 z3v109) 0))
(assert (and (>= (z3f61 z3v60) 0) (>= (z3f61 z3v62) 0) (= z3v62 z3v60) (>= (z3f61 z3v62) 0) (= (z3f64 z3v63) z3v63) (= (z3f64 z3v65) z3v65) (= (z3f64 z3v66) z3v66)))
(push 1)
(assert (not (>= (z3f61 z3v109) 0)))
(check-sat)
(pop 1)
(pop 1)
